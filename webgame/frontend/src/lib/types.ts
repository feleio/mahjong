// Protocol types — mirrors PROTOCOL.md exactly.

export type Seat = 0 | 1 | 2 | 3;

export interface SeatInfo {
  seat: Seat;
  name: string; // display name or "Bot 2"
  isBot: boolean;
  userId: string | null; // null for bots / empty seats
  connected: boolean; // false when a human's socket is down
  balance: number; // cumulative across games in this room
  empty: boolean; // true if nobody (lobby only)
}

export interface RoomState {
  code: string; // 6-char join code
  status: "lobby" | "playing";
  hostUserId: string;
  seats: SeatInfo[]; // always length 4
  youSeat: Seat | null; // your seat in this room
  youUserId: string;
  gamesPlayed: number;
  coachModels: string[]; // available AI-coach models, strongest first ([] = coach off)
  enforceTimeLimit: boolean; // false = no decision countdown (default)
}

export interface MeldGroup {
  kind: "pong" | "kong" | "chow";
  tiles: number[]; // full set, sorted
}

export interface GameView {
  gameId: string;
  yourSeat: Seat;
  dealerSeat: Seat;
  curSeat: Seat; // whose turn (engine's current player)
  remaining: number; // tiles left in wall
  hand: number[]; // YOUR tiles only, sorted
  handCounts: number[]; // concealed tile count per seat
  melds: MeldGroup[][]; // per seat, exposed groups
  discards: { seat: Seat; tile: number }[]; // chronological river
  pendingDiscard: { seat: Seat; tile: number } | null; // just-discarded, claimable
  lastDrawnTile: number | null; // set only if YOU just drew it
  lastEvent: GameEvent | null;
  // Not in the PROTOCOL.md type, but "the final view reveals all hands".
  // If the server includes per-seat revealed hands at game over, render them.
  hands?: (number[] | null)[];
}

export type GameEvent =
  | { kind: "draw"; seat: Seat; tile: number | null } // tile null unless it's you
  | { kind: "discard"; seat: Seat; tile: number }
  | { kind: "pong" | "kong"; seat: Seat; fromSeat: Seat; tile: number }
  | { kind: "chow"; seat: Seat; fromSeat: Seat; tile: number; position: 0 | 1 | 2 };

export type DecisionKind =
  | "discard"
  | "self_win"
  | "win"
  | "self_kong"
  | "kong"
  | "pong"
  | "chow";

/**
 * AI-coach hint: the strongest model's probability over this decision's
 * actions. Keys: discard → tile value; win/pong/kong → "pass"/"accept";
 * chow → "pass"/position id; self_kong → "pass"/tile value.
 * Present only when the server runs with a coach model.
 */
export interface CoachHint {
  probs: Record<string, number>;
  value?: number; // net's $-scale estimate of the current position
}

export interface Decision {
  requestId: number;
  decision: DecisionKind;
  context: {
    tile?: number; // the tile involved (win/kong/pong/chow/self_win)
    score?: number; // win score (win/self_win)
    fromSeat?: Seat; // who discarded (win/kong/pong/chow)
    validTiles?: number[]; // discard: your hand; self_kong: kongable tiles
    positions?: (0 | 1 | 2)[]; // chow options
  };
  deadlineTs: number | null; // epoch ms; server auto-acts after this. null = untimed
  view: GameView;
  coach?: Record<string, CoachHint>; // keyed by model name (see RoomState.coachModels)
}

export interface WinnersInfo {
  winners: { seat: Seat; score: number }[];
  loserSeat: Seat | null; // null on self-draw win
  winningTile: number;
  isSelfWin: boolean;
}

// ---- ack payloads ----

export type Ack<T = object> = ({ ok: true } & T) | { ok: false; error: string };

export type RoomAck = Ack<{ room: RoomState }>;
export type StateAck = Ack<{
  room: RoomState | null;
  game: GameView | null;
  decision: Decision | null;
}>;
export type PlainAck = Ack;

// ---- server -> client payloads ----

export interface GameStartedPayload {
  gameId: string;
  dealerSeat: Seat;
  view: GameView;
}

export interface GameEventPayload {
  event: GameEvent;
  view: GameView;
}

export interface GameWaitingPayload {
  seat: number;
  decision: string;
}

export interface GameOverPayload {
  winnersInfo: WinnersInfo | null;
  balances: number[]; // length 4
  view: GameView;
}

export interface RoomClosedPayload {
  reason: string;
}
