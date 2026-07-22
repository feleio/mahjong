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

export interface Prompt {
  type: "prompt";
  kind: PromptKind;
  seat: number;
  tile: string | null;
  score: number | null;
  selfKongTiles: string[] | null;
  chowPositions: string[] | null;
  handTiles: string[] | null;
}

export interface LobbyMsg {
  type: "lobby";
  room: Room;
}

export interface EndMsg { type: "end" }
export interface ErrorMsg { type: "error"; message: string }
export interface ReadyUpdateMsg { type: "ready_update"; readySeats: number[] }

export type WsMessage = GameSnapshot | Prompt | LobbyMsg | EndMsg | ErrorMsg | ReadyUpdateMsg;

export interface ClientAction {
  kind: PromptKind;
  yes?: boolean;
  tile?: string;
  chowPos?: "LEFT" | "MIDDLE" | "RIGHT";
}
