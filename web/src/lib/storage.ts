// Per-room credentials saved in localStorage so the seat survives refresh.

interface RoomCreds {
  playerId: string;
  seat: number;
  isHost: boolean;
  name: string;
}

const KEY = (roomId: string) => `mahjong:room:${roomId}`;

export function saveCreds(roomId: string, c: RoomCreds): void {
  if (typeof window === "undefined") return;
  window.localStorage.setItem(KEY(roomId), JSON.stringify(c));
}

export function loadCreds(roomId: string): RoomCreds | null {
  if (typeof window === "undefined") return null;
  const raw = window.localStorage.getItem(KEY(roomId));
  if (!raw) return null;
  try { return JSON.parse(raw) as RoomCreds; } catch { return null; }
}

export function clearCreds(roomId: string): void {
  if (typeof window === "undefined") return;
  window.localStorage.removeItem(KEY(roomId));
}

export function loadName(): string {
  if (typeof window === "undefined") return "";
  return window.localStorage.getItem("mahjong:name") ?? "";
}

export function saveName(n: string): void {
  if (typeof window === "undefined") return;
  window.localStorage.setItem("mahjong:name", n);
}

export type { RoomCreds };
