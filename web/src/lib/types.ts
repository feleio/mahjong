export type SeatKind =
  | "human"
  | "open"
  | "ai_chicken"
  | "ai_random"
  | "ai_first_felix"
  | "ai_3point_chicken"
  | "ai_champion";

export const AI_KINDS: SeatKind[] = [
  "ai_chicken",
  "ai_random",
  "ai_first_felix",
  "ai_3point_chicken",
  "ai_champion",
];

export const seatLabel: Record<SeatKind, string> = {
  human: "Human",
  open: "Open (waiting for human)",
  ai_chicken: "AI – Chicken",
  ai_random: "AI – Random",
  ai_first_felix: "AI – Felix",
  ai_3point_chicken: "AI – 3-point Chicken",
  ai_champion: "AI – Champion",
};

export interface Seat {
  index: number;
  kind: SeatKind;
  playerId: string | null;
  name: string;
}

export type RoomStatus = "waiting" | "playing" | "finished";

export interface Room {
  id: string;
  name: string;
  hostId: string;
  seats: Seat[];
  status: RoomStatus;
  createdAt: string;
}

export interface CreateRoomResp {
  room: Room;
  hostPlayerId: string;
}

export interface JoinResp {
  room: Room;
  seat: number;
  playerId: string;
}

/* ---------- gameplay messages ---------- */

export interface PlayerView {
  seat: number;
  name: string;
  kind: SeatKind;
  fixedGroups: { kind: "kong" | "pong" | "chow"; tiles: string[] }[];
  handCount: number;
  handTiles?: string[];
}

export interface GameSnapshot {
  type: "snapshot";
  roomId: string;
  yourSeat: number | null;
  curPlayer: number;
  remainingTiles: number;
  players: PlayerView[];
  discards: { seat: number; tile: string }[];
  lastEvent: string | null;
  winners: { seat: number; score: number }[];
  isFinished: boolean;
  selfWin: boolean;
}

export type PromptKind =
  | "self_win"
  | "win"
  | "self_kong"
  | "kong"
  | "pong"
  | "chow"
  | "discard";

/* ---------- coach (champion hints, issue #37) ---------- */

export interface OppTenpai {
  seat: number;
  p: number;
}

export interface CoachHint {
  /** action key -> champion probability. Keys: tile wire names for
   *  discard/self_kong, "pass"/"accept" for yes-no claims, "pass"/"LEFT"/
   *  "MIDDLE"/"RIGHT" for chow. */
  probs: Record<string, number>;
  top: string;
  value: number;
  oppTenpai?: OppTenpai[] | null;
  dangerByTile?: Record<string, number> | null;
}

export interface Prompt {
  type: "prompt";
  kind: PromptKind;
  seat: number;
  tile: string | null;
  score: number | null;
  selfKongTiles: string[] | null;
  chowPositions: string[] | null;
  handTiles: string[] | null;
  coach?: CoachHint | null;
  /** How long you have before the engine plays a default for you (#42). */
  timeoutMs?: number | null;
  /** Identifies this decision; echoed back so a late or replayed answer is
   *  never applied to a different one. */
  promptId?: number;
}

/** Your prompt expired and the engine moved on your behalf (#42). */
export interface TimeoutMsg {
  type: "timeout";
  seat: number;
  kind: PromptKind;
}

export interface LobbyMsg {
  type: "lobby";
  room: Room;
}

export interface EndMsg { type: "end" }
export interface ErrorMsg { type: "error"; message: string }
export interface ReadyUpdateMsg { type: "ready_update"; readySeats: number[] }

export type WsMessage =
  | GameSnapshot | Prompt | LobbyMsg | EndMsg | ErrorMsg | ReadyUpdateMsg | TimeoutMsg;

export type ConnState = "connecting" | "open" | "reconnecting";

export interface ClientAction {
  kind: PromptKind;
  yes?: boolean;
  tile?: string;
  chowPos?: "LEFT" | "MIDDLE" | "RIGHT";
  promptId?: number;
}

/* ---------- post-game review (issues #40/#41) ---------- */

export interface GameOutcome {
  drawn: boolean;
  isSelfWin: boolean;
  winningTile: string | null;
  loserSeat: number | null;
  winners: { seat: number; score: number }[];
}

export interface GameListItem {
  id: string;
  roomId: string;
  startedAt: string;
  finishedAt: string | null;
  seats: Seat[];
  outcome: GameOutcome | null;
  mySeat: number | null;
  myMoney: number | null;
}

export interface ReviewDecision {
  seq: number;
  kind: PromptKind;
  contextTile: string | null;
  hand: string[];
  chosen: string;
  chosenProb: number;
  best: string;
  bestProb: number;
  gap: number;
  value: number;
  agree: boolean;
  /** The engine played this turn for you after the prompt expired (#42). */
  timedOut: boolean;
  /** Grounded reason the champion differed, when one can be computed (#44). */
  why?: string | null;
  bucket?: "shape" | "tempo" | "safety" | "accept" | "other" | null;
}

export interface ReviewSummary {
  decisions: number;
  agreements: number;
  /** null when every turn was auto-played — no decisions to score. */
  agreementRate: number | null;
  meanGap: number;
  timedOut: number;
}

export interface GameReview {
  gameId: string;
  seat: number;
  playerName: string;
  startedAt: string;
  outcome: GameOutcome | null;
  seatMoney: number;
  summary: ReviewSummary;
  decisions: ReviewDecision[];
}

export interface PlayerGameAgreement {
  gameId: string;
  startedAt: string;
  agreementRate: number | null;
  decisions: number;
  timedOut: number;
}

export interface PlayerStats {
  name: string;
  games: number;
  totalMoney: number;
  moneyPerGame: number;
  winRate: number;
  selfWinRate: number;
  dealInRate: number;
  drawRate: number;
  reviewedGames: number;
  agreementRate: number | null;
  timedOutTurns: number;
  agreementByGame: PlayerGameAgreement[];
}
