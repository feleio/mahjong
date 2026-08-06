import {
  CreateRoomResp,
  GameListItem,
  GameReview,
  JoinResp,
  PlayerStats,
  Room,
  SeatKind,
} from "./types";

const API =
  process.env.NEXT_PUBLIC_API_BASE ?? "http://localhost:8080";
export const WS_BASE =
  process.env.NEXT_PUBLIC_WS_BASE ?? "ws://localhost:8080";

async function check(res: Response): Promise<any> {
  if (res.ok) return res.json();
  const body = await res.text();
  throw new Error(`${res.status}: ${body}`);
}

export async function listRooms(): Promise<Room[]> {
  return check(await fetch(`${API}/api/rooms`));
}

export async function getRoom(id: string): Promise<Room> {
  return check(await fetch(`${API}/api/rooms/${id}`));
}

export async function createRoom(
  name: string,
  hostName: string,
): Promise<CreateRoomResp> {
  return check(
    await fetch(`${API}/api/rooms`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ name, hostName }),
    }),
  );
}

export async function joinRoom(
  roomId: string,
  name: string,
  seatIndex?: number,
): Promise<JoinResp> {
  return check(
    await fetch(`${API}/api/rooms/${roomId}/join`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ name, seatIndex }),
    }),
  );
}

export async function setSeat(
  roomId: string,
  hostPlayerId: string,
  seatIndex: number,
  kind: SeatKind,
): Promise<Room> {
  return check(
    await fetch(`${API}/api/rooms/${roomId}/seat`, {
      method: "PATCH",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ hostPlayerId, seatIndex, kind }),
    }),
  );
}

export async function startGame(
  roomId: string,
  hostPlayerId: string,
): Promise<Room> {
  return check(
    await fetch(`${API}/api/rooms/${roomId}/start`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ hostPlayerId }),
    }),
  );
}

export async function markReady(
  roomId: string,
  playerId: string,
): Promise<void> {
  await check(
    await fetch(`${API}/api/rooms/${roomId}/ready`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ playerId }),
    }),
  );
}

export async function startNextGame(
  roomId: string,
  hostPlayerId: string,
): Promise<Room> {
  return check(
    await fetch(`${API}/api/rooms/${roomId}/start-next`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ hostPlayerId }),
    }),
  );
}

/* ---------- post-game review (issues #40/#41) ---------- */

export async function listGames(
  player?: string,
  limit = 50,
): Promise<GameListItem[]> {
  const qs = new URLSearchParams({ limit: String(limit) });
  if (player) qs.set("player", player);
  return check(await fetch(`${API}/api/games?${qs}`));
}

export async function getReview(
  gameId: string,
  seat: number,
): Promise<GameReview> {
  return check(await fetch(`${API}/api/games/${gameId}/review?seat=${seat}`));
}

export async function getPlayerStats(name: string): Promise<PlayerStats> {
  return check(
    await fetch(`${API}/api/players/${encodeURIComponent(name)}/stats`),
  );
}
