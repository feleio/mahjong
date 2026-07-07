import { randomUUID } from 'node:crypto';
import type { Server } from 'socket.io';
import type { PrismaClient } from '@prisma/client';
import type { Engine } from './engine.js';
import type {
  Decision,
  DecisionContext,
  DecisionKind,
  EngineAction,
  EngineMessage,
  EngineSnapshot,
  GameEvent,
  GameView,
  RoomState,
  Seat,
  SeatInfo,
  WinnersInfo,
} from './types.js';

const DISCARD_TIMEOUT_MS = 45_000;
const CLAIM_TIMEOUT_MS = 20_000;
const PACE_MS = 600; // gap between broadcast events so bot turns are watchable
const EMPTY_ROOM_TTL_MS = 5 * 60_000;

interface RoomSeat {
  userId: string | null;
  name: string;
  isBot: boolean;
  balance: number;
}

interface PendingDecision {
  requestId: number;
  seat: Seat;
  decision: DecisionKind;
  context: DecisionContext;
  deadlineTs: number;
  timer: NodeJS.Timeout;
}

interface ActiveGame {
  gameId: string;
  dbGameId: string;
  dealerSeat: Seat;
  snapshot: EngineSnapshot;
  pendingDiscard: { seat: Seat; tile: number } | null;
  lastDrawn: { seat: Seat; tile: number } | null;
  lastEvent: GameEvent | null;
  pendingDecision: PendingDecision | null;
  queue: Promise<void>; // pacing chain for engine messages
}

interface Room {
  id: string;
  code: string;
  hostUserId: string;
  status: 'lobby' | 'playing';
  seats: RoomSeat[];
  dealerSeat: Seat;
  gamesPlayed: number;
  game: ActiveGame | null;
  emptySince: number | null;
}

const emptySeat = (): RoomSeat => ({ userId: null, name: '', isBot: false, balance: 0 });

const sleep = (ms: number) => new Promise<void>((r) => setTimeout(r, ms));

function makeCode(): string {
  const chars = 'ABCDEFGHJKLMNPQRSTUVWXYZ23456789'; // no 0/O/1/I
  return Array.from({ length: 6 }, () => chars[Math.floor(Math.random() * chars.length)]).join('');
}

export class RoomManager {
  private rooms = new Map<string, Room>(); // by code
  private userRoom = new Map<string, string>(); // userId -> room code
  private userSockets = new Map<string, Set<string>>(); // userId -> socket ids
  private userNames = new Map<string, string>();

  constructor(
    private io: Server,
    private prisma: PrismaClient,
    private engine: Engine,
  ) {
    setInterval(() => this.sweepEmptyRooms(), 30_000).unref();
  }

  // ── connection tracking ────────────────────────────────────────────────────

  userConnected(userId: string, socketId: string, name: string) {
    if (!this.userSockets.has(userId)) this.userSockets.set(userId, new Set());
    this.userSockets.get(userId)!.add(socketId);
    this.userNames.set(userId, name);
    const room = this.roomOfUser(userId);
    if (room) {
      room.emptySince = null;
      // If this seat's decision was fast-forwarded to the 3s disconnect timer,
      // restore the full deadline now that the player is back.
      const game = room.game;
      const p = game?.pendingDecision;
      if (game && p && room.seats[p.seat].userId === userId) {
        clearTimeout(p.timer);
        const remaining = Math.max(1_000, p.deadlineTs - Date.now());
        p.timer = setTimeout(() => this.autoAct(room, game, p.requestId), remaining);
      }
      this.broadcastRoom(room);
    }
  }

  userDisconnected(userId: string, socketId: string) {
    const sockets = this.userSockets.get(userId);
    if (sockets) {
      sockets.delete(socketId);
      if (sockets.size === 0) this.userSockets.delete(userId);
    }
    const room = this.roomOfUser(userId);
    if (room) {
      if (!this.roomHasConnectedHuman(room)) room.emptySince = Date.now();
      this.broadcastRoom(room);
    }
  }

  private isConnected(userId: string | null): boolean {
    return userId !== null && (this.userSockets.get(userId)?.size ?? 0) > 0;
  }

  roomOfUser(userId: string): Room | null {
    const code = this.userRoom.get(userId);
    return code ? (this.rooms.get(code) ?? null) : null;
  }

  private roomHasConnectedHuman(room: Room): boolean {
    return room.seats.some((s) => s.userId && this.isConnected(s.userId));
  }

  private sweepEmptyRooms() {
    for (const room of [...this.rooms.values()]) {
      if (room.emptySince !== null && Date.now() - room.emptySince > EMPTY_ROOM_TTL_MS) {
        this.closeRoom(room, 'room expired (all players left)');
      }
    }
  }

  // ── room state serialization ───────────────────────────────────────────────

  roomState(room: Room, forUserId: string): RoomState {
    const seats: SeatInfo[] = room.seats.map((s, i) => ({
      seat: i as Seat,
      name: s.isBot ? s.name : s.userId ? (this.userNames.get(s.userId) ?? s.name) : '',
      isBot: s.isBot,
      userId: s.userId,
      connected: s.isBot ? true : this.isConnected(s.userId),
      balance: s.balance,
      empty: !s.isBot && !s.userId,
    }));
    const youSeat = room.seats.findIndex((s) => s.userId === forUserId);
    return {
      code: room.code,
      status: room.status,
      hostUserId: room.hostUserId,
      seats,
      youSeat: youSeat >= 0 ? (youSeat as Seat) : null,
      youUserId: forUserId,
      gamesPlayed: room.gamesPlayed,
    };
  }

  private broadcastRoom(room: Room) {
    for (const seat of room.seats) {
      if (seat.userId) {
        this.io.to(`u:${seat.userId}`).emit('room:update', this.roomState(room, seat.userId));
      }
    }
  }

  private emitToSeat(room: Room, seat: Seat, event: string, payload: unknown) {
    const userId = room.seats[seat].userId;
    if (userId) this.io.to(`u:${userId}`).emit(event, payload);
  }

  // ── lobby operations ───────────────────────────────────────────────────────

  async createRoom(userId: string): Promise<RoomState> {
    if (this.userRoom.has(userId)) await this.leaveRoom(userId);
    let code = makeCode();
    while (this.rooms.has(code)) code = makeCode();

    const dbRoom = await this.prisma.room.create({
      data: { code, hostId: userId },
    });
    const room: Room = {
      id: dbRoom.id,
      code,
      hostUserId: userId,
      status: 'lobby',
      seats: [emptySeat(), emptySeat(), emptySeat(), emptySeat()],
      dealerSeat: 0,
      gamesPlayed: 0,
      game: null,
      emptySince: null,
    };
    room.seats[0] = {
      userId,
      name: this.userNames.get(userId) ?? 'Player',
      isBot: false,
      balance: 0,
    };
    this.rooms.set(code, room);
    this.userRoom.set(userId, code);
    await this.prisma.roomMember.create({
      data: { roomId: room.id, userId, seat: 0 },
    });
    return this.roomState(room, userId);
  }

  async joinRoom(userId: string, code: string): Promise<RoomState> {
    const room = this.rooms.get(code.toUpperCase());
    if (!room) throw new Error('Room not found');

    const existingSeat = room.seats.findIndex((s) => s.userId === userId);
    if (existingSeat >= 0) {
      // rejoin / reconnect
      this.userRoom.set(userId, room.code);
      this.broadcastRoom(room);
      return this.roomState(room, userId);
    }
    if (room.status !== 'lobby') throw new Error('Game already in progress');

    const free = room.seats.findIndex((s) => !s.userId && !s.isBot);
    if (free < 0) throw new Error('Room is full');

    if (this.userRoom.has(userId)) await this.leaveRoom(userId);
    room.seats[free] = {
      userId,
      name: this.userNames.get(userId) ?? 'Player',
      isBot: false,
      balance: 0,
    };
    this.userRoom.set(userId, room.code);
    await this.prisma.roomMember.upsert({
      where: { roomId_userId: { roomId: room.id, userId } },
      create: { roomId: room.id, userId, seat: free },
      update: { seat: free },
    });
    room.emptySince = null;
    this.broadcastRoom(room);
    return this.roomState(room, userId);
  }

  async leaveRoom(userId: string): Promise<void> {
    const room = this.roomOfUser(userId);
    if (!room) return;
    this.userRoom.delete(userId);

    const seatIdx = room.seats.findIndex((s) => s.userId === userId);
    if (seatIdx >= 0 && room.status === 'lobby') {
      room.seats[seatIdx] = emptySeat();
      await this.prisma.roomMember
        .delete({ where: { roomId_userId: { roomId: room.id, userId } } })
        .catch(() => {});
    }
    // During a game the seat is kept (timeouts auto-play); the user just detaches.

    const humansLeft = room.seats.some((s) => s.userId && this.userRoom.get(s.userId) === room.code);
    if (!humansLeft) {
      this.closeRoom(room, 'all players left');
      return;
    }
    if (room.hostUserId === userId) {
      const nextHost = room.seats.find((s) => s.userId && this.userRoom.get(s.userId) === room.code);
      if (nextHost?.userId) room.hostUserId = nextHost.userId;
    }
    this.broadcastRoom(room);
  }

  private closeRoom(room: Room, reason: string) {
    if (room.game) {
      const g = room.game;
      if (g.pendingDecision) clearTimeout(g.pendingDecision.timer);
      this.engine.abort(g.gameId);
    }
    for (const seat of room.seats) {
      if (seat.userId) {
        this.io.to(`u:${seat.userId}`).emit('room:closed', { reason });
        this.userRoom.delete(seat.userId);
      }
    }
    this.rooms.delete(room.code);
    this.prisma.room
      .update({ where: { id: room.id }, data: { status: 'closed' } })
      .catch((e) => console.error('room close db error', e));
  }

  addBot(userId: string): RoomState {
    const room = this.requireHostLobby(userId);
    const free = room.seats.findIndex((s) => !s.userId && !s.isBot);
    if (free < 0) throw new Error('Room is full');
    room.seats[free] = { userId: null, name: `Bot ${free + 1}`, isBot: true, balance: 0 };
    this.broadcastRoom(room);
    return this.roomState(room, userId);
  }

  removeBot(userId: string, seat: number): RoomState {
    const room = this.requireHostLobby(userId);
    if (seat < 0 || seat > 3 || !room.seats[seat].isBot) throw new Error('No bot at that seat');
    room.seats[seat] = emptySeat();
    this.broadcastRoom(room);
    return this.roomState(room, userId);
  }

  private requireHostLobby(userId: string): Room {
    const room = this.roomOfUser(userId);
    if (!room) throw new Error('Not in a room');
    if (room.hostUserId !== userId) throw new Error('Only the host can do that');
    if (room.status !== 'lobby') throw new Error('Game already in progress');
    return room;
  }

  // ── game lifecycle ─────────────────────────────────────────────────────────

  async startGame(userId: string): Promise<void> {
    const room = this.requireHostLobby(userId);
    if (!room.seats.some((s) => s.userId)) throw new Error('Need at least one player');

    // Fill empty seats with bots
    room.seats.forEach((s, i) => {
      if (!s.userId && !s.isBot) {
        room.seats[i] = { userId: null, name: `Bot ${i + 1}`, isBot: true, balance: 0 };
      }
    });

    // Claim the room synchronously before any await — a second room:start
    // (double-click, second tab) must fail the lobby check above.
    room.status = 'playing';

    const seed = Math.floor(Math.random() * 2 ** 31);
    const gameId = randomUUID();
    let dbGame;
    try {
      dbGame = await this.prisma.game.create({
        data: {
          roomId: room.id,
          seed: BigInt(seed),
          dealerSeat: room.dealerSeat,
          seats: room.seats.map((s, i) => ({
            seat: i,
            userId: s.userId,
            isBot: s.isBot,
            name: s.isBot ? s.name : (this.userNames.get(s.userId!) ?? s.name),
          })),
        },
      });
    } catch (e) {
      room.status = 'lobby';
      throw e;
    }

    const seatSpecs = room.seats.map((s) => (s.userId ? 'remote' : 'chicken')) as (
      | 'remote'
      | 'chicken'
    )[];

    this.engine.newGame(
      gameId,
      { seed, dealerSeat: room.dealerSeat, seats: seatSpecs },
      (msg) => this.onEngineMessage(room, msg),
    );

    room.game = {
      gameId,
      dbGameId: dbGame.id,
      dealerSeat: room.dealerSeat,
      snapshot: { hands: [[], [], [], []], groups: [[], [], [], []], discards: [], remaining: 0, curSeat: room.dealerSeat },
      pendingDiscard: null,
      lastDrawn: null,
      lastEvent: null,
      pendingDecision: null,
      queue: Promise.resolve(),
    };
    this.broadcastRoom(room);
  }

  // ── engine message pump (paced) ────────────────────────────────────────────

  private onEngineMessage(room: Room, msg: EngineMessage) {
    const game = room.game;
    if (!game) return;
    game.queue = game.queue
      .then(() => this.processEngineMessage(room, game, msg))
      .catch((e) => console.error('engine msg error', e));
  }

  private async processEngineMessage(room: Room, game: ActiveGame, msg: EngineMessage) {
    if (room.game !== game) return; // game was torn down

    switch (msg.type) {
      case 'game_started': {
        game.snapshot = msg.snapshot;
        for (let seat = 0 as Seat; seat < 4; seat++) {
          this.emitToSeat(room, seat as Seat, 'game:started', {
            gameId: game.gameId,
            dealerSeat: game.dealerSeat,
            view: this.buildView(room, game, seat as Seat),
          });
        }
        break;
      }

      case 'event': {
        game.snapshot = msg.snapshot;
        const e = msg.event;
        if (e.kind === 'draw') {
          game.pendingDiscard = null; // uncontested discard is now in the river
          game.lastDrawn = { seat: e.seat, tile: e.tile };
        } else if (e.kind === 'discard') {
          game.pendingDiscard = { seat: e.seat, tile: e.tile };
        } else if (e.kind === 'pong' || e.kind === 'kong' || e.kind === 'chow') {
          game.pendingDiscard = null;
          game.lastDrawn = null;
        }
        game.lastEvent = e as GameEvent;

        for (let seat = 0; seat < 4; seat++) {
          const view = this.buildView(room, game, seat as Seat);
          this.emitToSeat(room, seat as Seat, 'game:event', { event: view.lastEvent, view });
        }
        await sleep(PACE_MS);
        break;
      }

      case 'decision_request': {
        game.snapshot = msg.snapshot;
        const timeoutMs = msg.decision === 'discard' ? DISCARD_TIMEOUT_MS : CLAIM_TIMEOUT_MS;
        const deadlineTs = Date.now() + timeoutMs;
        const timer = setTimeout(() => this.autoAct(room, game, msg.requestId), timeoutMs);
        game.pendingDecision = {
          requestId: msg.requestId,
          seat: msg.seat,
          decision: msg.decision,
          context: msg.context,
          deadlineTs,
          timer,
        };
        const decision: Decision = {
          requestId: msg.requestId,
          decision: msg.decision,
          context: msg.context,
          deadlineTs,
          view: this.buildView(room, game, msg.seat),
        };
        this.emitToSeat(room, msg.seat, 'game:decision', decision);
        for (let seat = 0; seat < 4; seat++) {
          if (seat !== msg.seat) {
            this.emitToSeat(room, seat as Seat, 'game:waiting', {
              seat: msg.seat,
              decision: msg.decision,
            });
          }
        }
        // If the seat's human is gone (left/disconnected), act quickly.
        const seatInfo = room.seats[msg.seat];
        if (seatInfo.userId && !this.isConnected(seatInfo.userId)) {
          clearTimeout(timer);
          game.pendingDecision.timer = setTimeout(
            () => this.autoAct(room, game, msg.requestId),
            3_000,
          );
        }
        break;
      }

      case 'game_over': {
        game.snapshot = msg.snapshot;
        if (game.pendingDecision) clearTimeout(game.pendingDecision.timer);
        game.pendingDecision = null;
        await sleep(PACE_MS); // let the last event land visually
        await this.finishGame(room, game, msg.winnersInfo, msg.balances);
        break;
      }

      case 'aborted':
        break;

      case 'error': {
        console.error(`[engine] game ${game.gameId} error: ${msg.message}`);
        if (game.pendingDecision) clearTimeout(game.pendingDecision.timer);
        room.game = null;
        room.status = 'lobby';
        this.broadcastRoom(room);
        break;
      }
    }
  }

  private async finishGame(
    room: Room,
    game: ActiveGame,
    winnersInfo: WinnersInfo | null,
    balances: number[],
  ) {
    // apply balances
    room.seats.forEach((s, i) => (s.balance += balances[i] ?? 0));
    room.gamesPlayed += 1;

    // dealer rotation: dealer stays on draw or dealer win, otherwise passes on
    const dealerWon = winnersInfo?.winners.some((w) => w.seat === game.dealerSeat) ?? false;
    if (winnersInfo && !dealerWon) room.dealerSeat = ((room.dealerSeat + 1) % 4) as Seat;

    room.status = 'lobby';
    room.game = null;

    for (let seat = 0; seat < 4; seat++) {
      // final view: reveal nothing extra per-seat, but include full hands via handCounts;
      // winners' hands are revealed through winnersInfo + final snapshot hands below.
      const view = {
        ...this.buildView(room, { ...game, pendingDecision: null }, seat as Seat),
        hands: game.snapshot.hands, // table is over — reveal all four hands
      };
      this.emitToSeat(room, seat as Seat, 'game:over', { winnersInfo, balances, view });
    }
    this.broadcastRoom(room);

    await this.prisma.game.update({
      where: { id: game.dbGameId },
      data: {
        endedAt: new Date(),
        winnersInfo: winnersInfo === null ? undefined : (winnersInfo as object),
        balances,
      },
    });
    await Promise.all(
      room.seats.map((s, i) =>
        s.userId
          ? this.prisma.roomMember
              .update({
                where: { roomId_userId: { roomId: room.id, userId: s.userId } },
                data: { balance: s.balance },
              })
              .catch(() => {})
          : Promise.resolve(null),
      ),
    );
    await this.prisma.room.update({
      where: { id: room.id },
      data: { gamesPlayed: room.gamesPlayed, dealerSeat: room.dealerSeat },
    });
  }

  // ── decisions ──────────────────────────────────────────────────────────────

  private defaultAction(p: PendingDecision, game: ActiveGame): EngineAction {
    switch (p.decision) {
      case 'discard': {
        const valid = p.context.validTiles ?? [];
        const drawn = game.lastDrawn;
        if (drawn && drawn.seat === p.seat && valid.includes(drawn.tile)) return drawn.tile;
        return valid[0] ?? 0;
      }
      case 'self_win':
      case 'win':
        return true;
      case 'pong':
      case 'kong':
        return false;
      case 'chow':
      case 'self_kong':
        return null;
    }
  }

  private autoAct(room: Room, game: ActiveGame, requestId: number) {
    const p = game.pendingDecision;
    if (!p || p.requestId !== requestId || room.game !== game) return;
    const action = this.defaultAction(p, game);
    game.pendingDecision = null;
    this.engine.action(game.gameId, requestId, action);
  }

  submitAction(userId: string, requestId: number, action: EngineAction): void {
    const room = this.roomOfUser(userId);
    if (!room || !room.game) throw new Error('No active game');
    const game = room.game;
    const p = game.pendingDecision;
    if (!p || p.requestId !== requestId) throw new Error('Decision expired');
    if (room.seats[p.seat].userId !== userId) throw new Error('Not your decision');

    // Light validation; the engine is also defensive.
    if (p.decision === 'discard') {
      if (typeof action !== 'number' || !(p.context.validTiles ?? []).includes(action))
        throw new Error('Invalid tile');
    } else if (p.decision === 'self_win' || p.decision === 'win' || p.decision === 'pong' || p.decision === 'kong') {
      if (typeof action !== 'boolean') throw new Error('Invalid action');
    } else if (p.decision === 'chow') {
      if (action !== null && !(p.context.positions ?? []).includes(action as 0 | 1 | 2))
        throw new Error('Invalid chow position');
    } else if (p.decision === 'self_kong') {
      if (action !== null && !(p.context.validTiles ?? []).includes(action as number))
        throw new Error('Invalid kong tile');
    }

    clearTimeout(p.timer);
    game.pendingDecision = null;
    this.engine.action(game.gameId, requestId, action);
  }

  // ── views ──────────────────────────────────────────────────────────────────

  private buildView(room: Room, game: ActiveGame, seat: Seat): GameView {
    const s = game.snapshot;
    const lastEvent = game.lastEvent
      ? game.lastEvent.kind === 'draw' && game.lastEvent.seat !== seat
        ? { ...game.lastEvent, tile: null }
        : game.lastEvent
      : null;
    return {
      gameId: game.gameId,
      yourSeat: seat,
      dealerSeat: game.dealerSeat,
      curSeat: s.curSeat,
      remaining: s.remaining,
      hand: s.hands[seat] ?? [],
      handCounts: s.hands.map((h) => h.length),
      melds: s.groups,
      discards: s.discards,
      pendingDiscard: game.pendingDiscard,
      lastDrawnTile: game.lastDrawn && game.lastDrawn.seat === seat ? game.lastDrawn.tile : null,
      lastEvent,
    };
  }

  /** Full resync payload for room:state */
  syncState(userId: string): {
    room: RoomState | null;
    game: GameView | null;
    decision: Decision | null;
  } {
    const room = this.roomOfUser(userId);
    if (!room) return { room: null, game: null, decision: null };
    const state = this.roomState(room, userId);
    const seatIdx = room.seats.findIndex((s) => s.userId === userId);
    if (!room.game || seatIdx < 0) return { room: state, game: null, decision: null };

    const view = this.buildView(room, room.game, seatIdx as Seat);
    const p = room.game.pendingDecision;
    const decision: Decision | null =
      p && p.seat === seatIdx
        ? {
            requestId: p.requestId,
            decision: p.decision,
            context: p.context,
            deadlineTs: p.deadlineTs,
            view,
          }
        : null;
    return { room: state, game: view, decision };
  }
}
