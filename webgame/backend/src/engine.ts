import { spawn, type ChildProcessWithoutNullStreams } from 'node:child_process';
import { createInterface } from 'node:readline';
import path from 'node:path';
import type { EngineAction, EngineMessage } from './types.js';

const MAIN_CLASS = 'io.fele.app.mahjong.web.WebGameServer';

export type EngineHandler = (msg: EngineMessage) => void;

/**
 * Manages the persistent Scala engine child process (JSON lines over stdio).
 * One process hosts many concurrent games; messages are routed to per-game
 * handlers by gameId.
 */
export class Engine {
  private proc: ChildProcessWithoutNullStreams | null = null;
  private handlers = new Map<string, EngineHandler>();
  private jarPath: string;
  private coachModels: { name: string; path: string }[];
  private alive = false;

  constructor(jarPath: string, coachModels: { name: string; path: string }[] = []) {
    this.jarPath = path.resolve(jarPath);
    this.coachModels = coachModels.map((m) => ({ name: m.name, path: path.resolve(m.path) }));
    this.start();
  }

  private start() {
    const spec = this.coachModels.map((m) => `${m.name}=${m.path}`).join(',');
    const args = spec ? [`-Dweb.coachmodels=${spec}`] : [];
    this.proc = spawn('java', [...args, '-cp', this.jarPath, MAIN_CLASS], {
      stdio: ['pipe', 'pipe', 'pipe'],
    });
    this.alive = true;
    const rl = createInterface({ input: this.proc.stdout });
    rl.on('line', (line) => {
      let msg: EngineMessage;
      try {
        msg = JSON.parse(line);
      } catch {
        console.error('[engine] unparseable line:', line.slice(0, 300));
        return;
      }
      const gameId = 'gameId' in msg ? msg.gameId : undefined;
      if (gameId && this.handlers.has(gameId)) {
        this.handlers.get(gameId)!(msg);
        if (msg.type === 'game_over' || msg.type === 'aborted') this.handlers.delete(gameId);
      } else if (msg.type === 'error') {
        console.error('[engine] error:', msg.message);
      }
    });
    this.proc.stderr.on('data', () => {
      /* engine logs — ignore */
    });
    this.proc.on('exit', (code) => {
      this.alive = false;
      console.error(`[engine] process exited (code ${code}), restarting…`);
      // Fail all in-flight games, then restart the process.
      for (const [gameId, handler] of this.handlers) {
        handler({ type: 'error', gameId, message: 'engine process crashed' });
      }
      this.handlers.clear();
      setTimeout(() => this.start(), 1000);
    });
  }

  private send(obj: Record<string, unknown>) {
    this.proc?.stdin.write(JSON.stringify(obj) + '\n');
  }

  newGame(
    gameId: string,
    opts: { seed: number; dealerSeat: number; seats: ('remote' | 'chicken')[] },
    handler: EngineHandler,
  ) {
    if (!this.alive || !this.proc) {
      // Engine is mid-restart — fail fast so the room returns to lobby
      // instead of wedging in "playing" with a game that never starts.
      setImmediate(() => handler({ type: 'error', gameId, message: 'engine restarting, try again' }));
      return;
    }
    this.handlers.set(gameId, handler);
    this.send({ cmd: 'new_game', gameId, ...opts });
  }

  action(gameId: string, requestId: number, action: EngineAction) {
    this.send({ cmd: 'action', gameId, requestId, action });
  }

  abort(gameId: string) {
    this.send({ cmd: 'abort_game', gameId });
    this.handlers.delete(gameId);
  }
}
