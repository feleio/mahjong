// Shared protocol types — mirrors webgame/PROTOCOL.md

export type Seat = 0 | 1 | 2 | 3;

export interface SeatInfo {
  seat: Seat;
  name: string;
  isBot: boolean;
  userId: string | null;
  connected: boolean;
  balance: number;
  empty: boolean;
}

export interface RoomState {
  code: string;
  status: 'lobby' | 'playing';
  hostUserId: string;
  seats: SeatInfo[];
  youSeat: Seat | null;
  youUserId: string;
  gamesPlayed: number;
}

export interface MeldGroup {
  kind: 'pong' | 'kong' | 'chow';
  tiles: number[];
}

export type GameEvent =
  | { kind: 'draw'; seat: Seat; tile: number | null }
  | { kind: 'discard'; seat: Seat; tile: number }
  | { kind: 'pong'; seat: Seat; fromSeat: Seat; tile: number }
  | { kind: 'kong'; seat: Seat; fromSeat: Seat; tile: number }
  | { kind: 'chow'; seat: Seat; fromSeat: Seat; tile: number; position: 0 | 1 | 2 };

export interface GameView {
  gameId: string;
  yourSeat: Seat;
  dealerSeat: Seat;
  curSeat: Seat;
  remaining: number;
  hand: number[];
  handCounts: number[];
  melds: MeldGroup[][];
  discards: { seat: Seat; tile: number }[];
  pendingDiscard: { seat: Seat; tile: number } | null;
  lastDrawnTile: number | null;
  lastEvent: GameEvent | null;
}

export type DecisionKind =
  | 'discard'
  | 'self_win'
  | 'win'
  | 'self_kong'
  | 'kong'
  | 'pong'
  | 'chow';

export interface DecisionContext {
  tile?: number;
  score?: number;
  fromSeat?: Seat;
  validTiles?: number[];
  positions?: (0 | 1 | 2)[];
}

/**
 * AI-coach hint attached by the engine (web.coachmodel): the strongest net's
 * probability over this decision's actions. Keys: discard → tile value;
 * binary decisions → "pass"/"accept"; chow → "pass"/position id;
 * self_kong → "pass"/tile value. `value` is the net's $-scale estimate of
 * the current position.
 */
export interface CoachHint {
  probs: Record<string, number>;
  value?: number;
}

export interface Decision {
  requestId: number;
  decision: DecisionKind;
  context: DecisionContext;
  deadlineTs: number;
  view: GameView;
  coach?: CoachHint;
}

export interface WinnersInfo {
  winners: { seat: Seat; score: number }[];
  loserSeat: Seat | null;
  winningTile: number;
  isSelfWin: boolean;
}

// ── Engine wire messages (Scala WebGameServer stdio protocol) ──

export interface EngineSnapshot {
  hands: number[][];
  groups: MeldGroup[][];
  discards: { seat: Seat; tile: number }[];
  remaining: number;
  curSeat: Seat;
}

export type EngineMessage =
  | { type: 'pong' }
  | { type: 'game_started'; gameId: string; dealerSeat: Seat; snapshot: EngineSnapshot }
  | {
      type: 'event';
      gameId: string;
      event: { kind: string; seat: Seat; tile: number; fromSeat?: Seat; position?: number };
      snapshot: EngineSnapshot;
    }
  | {
      type: 'decision_request';
      gameId: string;
      requestId: number;
      seat: Seat;
      decision: DecisionKind;
      context: DecisionContext;
      snapshot: EngineSnapshot;
      coach?: CoachHint;
    }
  | {
      type: 'game_over';
      gameId: string;
      winnersInfo: {
        winners: { seat: Seat; score: number }[];
        loserSeat: Seat | null;
        winningTile: number;
        isSelfWin: boolean;
      } | null;
      balances: number[];
      snapshot: EngineSnapshot;
    }
  | { type: 'aborted'; gameId: string }
  | { type: 'error'; gameId?: string; message: string };

export type EngineAction = boolean | number | null;
