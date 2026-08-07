"use client";

// Client for the Scala `server/` (http4s): REST for rooms, one WebSocket per
// room for play. This replaces the Socket.IO client that talked to the retired
// Node backend (issue #47).
//
// Room membership is a per-room {seat, playerId} pair the server hands out on
// join, so it is kept in localStorage per room — the server has no notion of a
// long-lived user identity.

const API =
  process.env.NEXT_PUBLIC_API_BASE ?? "http://localhost:8080";
export const WS_BASE =
  process.env.NEXT_PUBLIC_WS_BASE ?? "ws://localhost:8080";

async function check<T>(res: Response): Promise<T> {
  if (res.ok) return (await res.json()) as T;
  const body = await res.text();
  let msg = body;
  try {
    const parsed = JSON.parse(body) as { error?: string };
    if (parsed.error) msg = parsed.error;
  } catch {
    /* not json — use the raw body */
  }
  throw new Error(msg || `request failed (${res.status})`);
}

function post<T>(path: string, body: unknown): Promise<T> {
  return fetch(`${API}${path}`, {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  }).then(check<T>);
}

function patch<T>(path: string, body: unknown): Promise<T> {
  return fetch(`${API}${path}`, {
    method: "PATCH",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify(body),
  }).then(check<T>);
}

/* ---------- server wire shapes (only what this app consumes) ---------- */

// The server's public projections (issue #51). Credentials — the host id and
// each seat's player id — are never published: yours arrives once, in the
// create/join response, and is kept in localStorage from there on.

export interface ServerSeat {
  index: number;
  kind: string;
  name: string;
  occupied: boolean; // someone holds this seat (replaces "playerId != null")
}

export interface ServerRoom {
  id: string;
  name: string;
  hostSeat: number; // which seat runs the room (replaces the host's id)
  seats: ServerSeat[];
  status: "waiting" | "playing" | "finished";
  createdAt: string;
  code: string;
  balances: number[];
  gamesPlayed: number;
}

export interface ServerPlayerView {
  seat: number;
  name: string;
  kind: string;
  fixedGroups: { kind: "kong" | "pong" | "chow"; tiles: string[] }[];
  handCount: number;
  handTiles?: string[] | null;
}

export interface ServerEvent {
  kind: string;
  seat: number | null;
  tile: string | null;
  fromSeat: number | null;
  position: string | null;
}

export interface ServerSnapshot {
  type: "snapshot";
  roomId: string;
  yourSeat: number | null;
  curPlayer: number;
  dealerSeat: number;
  remainingTiles: number;
  players: ServerPlayerView[];
  discards: { seat: number; tile: string }[];
  lastEvent: string | null;
  event: ServerEvent | null;
  pendingDiscard: { seat: number; tile: string } | null;
  balances: number[];
  gamesPlayed: number;
  winners: { seat: number; score: number }[];
  winningTile: string | null;
  loserSeat: number | null;
  balanceDelta: number[];
  isFinished: boolean;
  selfWin: boolean;
}

export interface ServerCoachHint {
  probs: Record<string, number>;
  top: string;
  value: number;
  oppTenpai?: { seat: number; p: number }[] | null;
  dangerByTile?: Record<string, number> | null;
}

export interface ServerPrompt {
  type: "prompt";
  kind: string;
  seat: number;
  tile: string | null;
  score: number | null;
  selfKongTiles: string[] | null;
  chowPositions: string[] | null;
  handTiles: string[] | null;
  coach?: ServerCoachHint | null;
  timeoutMs?: number | null;
  promptId?: number;
}

export type ServerFrame =
  | ServerSnapshot
  | ServerPrompt
  | { type: "waiting"; seat: number; decision: string }
  | { type: "timeout"; seat: number; kind: string }
  | { type: "lobby"; room: ServerRoom }
  | { type: "end" }
  | { type: "error"; message: string }
  | { type: "ready_update"; readySeats: number[] };

/* ---------- REST ---------- */

// No listRooms: the server deliberately has no room listing — a room's code or
// id is the capability to reach it (issue #51).

export const getRoom = (idOrCode: string): Promise<ServerRoom> =>
  fetch(`${API}/api/rooms/${encodeURIComponent(idOrCode)}`).then(check<ServerRoom>);

export const createRoom = (name: string, hostName: string) =>
  post<{ room: ServerRoom; hostPlayerId: string }>("/api/rooms", { name, hostName });

export const joinRoom = (roomId: string, name: string, seatIndex?: number) =>
  post<{ room: ServerRoom; seat: number; playerId: string }>(
    `/api/rooms/${roomId}/join`,
    { name, seatIndex },
  );

export const setSeat = (roomId: string, hostPlayerId: string, seatIndex: number, kind: string) =>
  patch<ServerRoom>(`/api/rooms/${roomId}/seat`, { hostPlayerId, seatIndex, kind });

export const startGame = (roomId: string, hostPlayerId: string) =>
  post<ServerRoom>(`/api/rooms/${roomId}/start`, { hostPlayerId });

export const markReady = (roomId: string, playerId: string) =>
  post<{ readySeats: number[] }>(`/api/rooms/${roomId}/ready`, { playerId });

export const startNextGame = (roomId: string, hostPlayerId: string) =>
  post<ServerRoom>(`/api/rooms/${roomId}/start-next`, { hostPlayerId });

/* ---------- review (issues #40/#41) ---------- */

export const listGames = (player: string, limit = 50) =>
  fetch(`${API}/api/games?player=${encodeURIComponent(player)}&limit=${limit}`).then(check<GameListItem[]>);

export const getReview = (gameId: string, seat: number) =>
  fetch(`${API}/api/games/${gameId}/review?seat=${seat}`).then(check<GameReview>);

export const getPlayerStats = (name: string) =>
  fetch(`${API}/api/players/${encodeURIComponent(name)}/stats`).then(check<PlayerStats>);

export interface GameOutcomeWire {
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
  seats: ServerSeat[];
  outcome: GameOutcomeWire | null;
  mySeat: number | null;
  myMoney: number | null;
}

export interface ReviewDecisionWire {
  seq: number;
  kind: string;
  contextTile: string | null;
  hand: string[];
  chosen: string;
  chosenProb: number;
  best: string;
  bestProb: number;
  gap: number;
  value: number;
  agree: boolean;
  timedOut: boolean;
  why?: string | null;
  bucket?: string | null;
}

export interface GameReview {
  gameId: string;
  seat: number;
  playerName: string;
  startedAt: string;
  outcome: GameOutcomeWire | null;
  seatMoney: number;
  summary: {
    decisions: number;
    agreements: number;
    agreementRate: number | null;
    meanGap: number;
    timedOut: number;
  };
  decisions: ReviewDecisionWire[];
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
  agreementByGame: {
    gameId: string;
    startedAt: string;
    agreementRate: number | null;
    decisions: number;
    timedOut: number;
  }[];
}

/* ---------- per-room membership ---------- */

export interface RoomCreds {
  playerId: string;
  seat: number;
  isHost: boolean;
  name: string;
}

const credsKey = (roomId: string) => `mj:room:${roomId}`;

export function saveCreds(roomId: string, c: RoomCreds): void {
  if (typeof window === "undefined") return;
  window.localStorage.setItem(credsKey(roomId), JSON.stringify(c));
}

export function loadCreds(roomId: string): RoomCreds | null {
  if (typeof window === "undefined") return null;
  const raw = window.localStorage.getItem(credsKey(roomId));
  if (!raw) return null;
  try {
    return JSON.parse(raw) as RoomCreds;
  } catch {
    return null;
  }
}

export function loadName(): string {
  if (typeof window === "undefined") return "";
  return window.localStorage.getItem("mj_name") ?? "";
}

export function saveName(n: string): void {
  if (typeof window === "undefined") return;
  window.localStorage.setItem("mj_name", n);
}

/** WebSocket URL for a room, authenticated as a seat when we have credentials. */
export function wsUrl(roomId: string, creds: RoomCreds | null): string {
  const qs = new URLSearchParams();
  if (creds) {
    qs.set("seat", String(creds.seat));
    qs.set("player", creds.playerId);
  }
  return `${WS_BASE}/ws/rooms/${roomId}${qs.toString() ? `?${qs}` : ""}`;
}
