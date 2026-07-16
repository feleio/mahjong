import http from 'node:http';
import fs from 'node:fs';
import express from 'express';
import cors from 'cors';
import { Server } from 'socket.io';
import { PrismaClient } from '@prisma/client';
import { Engine } from './engine.js';
import { RoomManager } from './rooms.js';
import type { EngineAction } from './types.js';

const PORT = Number(process.env.PORT ?? 4000);
const ENGINE_JAR = process.env.ENGINE_JAR ?? '../../target/scala-2.12/mahjong-assembly-0.1.0.jar';
const CORS_ORIGIN = process.env.CORS_ORIGIN ?? 'http://localhost:3000';
// Optional AI-coach models (ONNX, "name=path,name=path", strongest first —
// the first entry is the UI default). Decision prompts carry every model's
// action probabilities so players can compare how each trained net plays.
const COACH_MODELS =
  process.env.COACH_MODELS ??
  [
    // Danger v4 first (UI default): champion-strength play PLUS opponent
    // tenpai/deal-in warnings from its danger heads (tenpai AUC 0.86).
    'Danger v4=../../rl/checkpoints/exit_v4/exit_final.onnx',
    'Champion=../../rl/checkpoints/exit_sp1b_soft/exit_final.onnx',
    'ExIt r3=../../rl/checkpoints/exit_v3_r3_soft/exit_final.onnx',
    'Imitation=../../rl/checkpoints/imitation_v3d/imitation_epoch10.onnx',
    'Student (fast)=../../rl/checkpoints/student/student48_sp1b.onnx',
  ].join(',');

const prisma = new PrismaClient();
const app = express();
app.use(cors({ origin: CORS_ORIGIN }));
app.get('/health', (_req, res) => res.json({ ok: true }));

const server = http.createServer(app);
const io = new Server(server, { cors: { origin: CORS_ORIGIN } });
// Missing model files ⇒ dropped with a warning (a bad path must not crashloop
// the engine); order is preserved so the first surviving entry is the default.
const coachModels = COACH_MODELS.split(',')
  .map((spec) => {
    const eq = spec.indexOf('=');
    return eq > 0 ? { name: spec.slice(0, eq).trim(), path: spec.slice(eq + 1).trim() } : null;
  })
  .filter((m): m is { name: string; path: string } => {
    if (!m) return false;
    if (!fs.existsSync(m.path)) {
      console.warn(`[coach] model not found: ${m.name} = ${m.path} — skipped`);
      return false;
    }
    return true;
  });
console.log(`[coach] models: ${coachModels.map((m) => m.name).join(', ') || '(none)'}`);
const engine = new Engine(ENGINE_JAR, coachModels);
const roomManager = new RoomManager(io, prisma, engine, coachModels.map((m) => m.name));

io.use(async (socket, next) => {
  try {
    const { token, name } = socket.handshake.auth as { token?: string; name?: string };
    if (!token || typeof token !== 'string' || token.length < 8) {
      return next(new Error('missing auth token'));
    }
    const displayName = (typeof name === 'string' && name.trim().slice(0, 24)) || 'Player';
    const user = await prisma.user.upsert({
      where: { token },
      create: { token, name: displayName },
      update: { name: displayName },
    });
    socket.data.userId = user.id;
    socket.data.name = displayName;
    next();
  } catch (e) {
    next(e as Error);
  }
});

type Ack = (res: Record<string, unknown>) => void;
const withAck =
  (fn: (payload: Record<string, unknown> | undefined) => Promise<Record<string, unknown>> | Record<string, unknown>) =>
  async (payload: Record<string, unknown> | undefined, ack?: Ack) => {
    try {
      const res = await fn(payload);
      ack?.({ ok: true, ...res });
    } catch (e) {
      ack?.({ ok: false, error: e instanceof Error ? e.message : 'unknown error' });
    }
  };

io.on('connection', (socket) => {
  const userId = socket.data.userId as string;
  const name = socket.data.name as string;
  socket.join(`u:${userId}`);
  roomManager.userConnected(userId, socket.id, name);

  socket.on(
    'room:create',
    withAck(async () => ({ room: await roomManager.createRoom(userId) })),
  );

  socket.on(
    'room:join',
    withAck(async (payload) => {
      const code = String(payload?.code ?? '').trim();
      if (!code) throw new Error('Room code required');
      return { room: await roomManager.joinRoom(userId, code) };
    }),
  );

  socket.on(
    'room:leave',
    withAck(async () => {
      await roomManager.leaveRoom(userId);
      return {};
    }),
  );

  socket.on(
    'room:addBot',
    withAck(() => ({ room: roomManager.addBot(userId) })),
  );

  socket.on(
    'room:removeBot',
    withAck((payload) => ({ room: roomManager.removeBot(userId, Number(payload?.seat)) })),
  );

  socket.on(
    'room:setTimeLimit',
    withAck((payload) => ({
      room: roomManager.setTimeLimit(userId, Boolean(payload?.enabled)),
    })),
  );

  socket.on(
    'room:start',
    withAck(async () => {
      await roomManager.startGame(userId);
      return {};
    }),
  );

  socket.on(
    'room:state',
    withAck(() => roomManager.syncState(userId) as unknown as Record<string, unknown>),
  );

  socket.on(
    'game:action',
    withAck((payload) => {
      const requestId = Number(payload?.requestId);
      if (!Number.isFinite(requestId)) throw new Error('requestId required');
      const action = (payload?.action ?? null) as EngineAction;
      roomManager.submitAction(userId, requestId, action);
      return {};
    }),
  );

  socket.on('disconnect', () => {
    roomManager.userDisconnected(userId, socket.id);
  });
});

// Rooms live in memory and do not survive a restart — close out stale rows.
await prisma.room.updateMany({
  where: { status: { not: 'closed' } },
  data: { status: 'closed' },
});

server.listen(PORT, () => {
  console.log(`mahjong backend listening on :${PORT} (engine jar: ${ENGINE_JAR})`);
});
