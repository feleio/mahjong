// Translation between this app's tile integers and the server's wire names.
//
// The UI works in integers 0–33 (see tiles.ts) because that is what every
// component renders from; `server/` works in names like "D5" / "HW_E" because
// that is what its engine and its recorded games use. Neither encoding is
// wrong, so they meet here and nowhere else — map by NAME, never by assuming
// the two integer orderings agree, since they are maintained independently.

import type { Seat } from "./types";

/** Index = this app's tile int; value = the server's wire name. */
const WIRE: readonly string[] = [
  "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", // 0–8   dots 筒
  "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", // 9–17  bamboo 索
  "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", // 18–26 characters 萬
  "HW_E", "HW_S", "HW_W", "HW_N",                        // 27–30 winds 東南西北
  "HD_R", "HD_G", "HD_B",                                // 31–33 dragons 中發白
];

const INT_BY_WIRE: Record<string, number> = Object.fromEntries(
  WIRE.map((name, i) => [name, i]),
);

export function tileToWire(tile: number): string {
  const w = WIRE[tile];
  if (w === undefined) throw new Error(`tile out of range: ${tile}`);
  return w;
}

/** Returns null for an unknown name rather than guessing a tile. */
export function tileFromWire(wire: string | null | undefined): number | null {
  if (!wire) return null;
  const t = INT_BY_WIRE[wire];
  return t === undefined ? null : t;
}

export function tilesFromWire(wires: readonly string[] | null | undefined): number[] {
  return (wires ?? []).map(tileFromWire).filter((t): t is number => t !== null);
}

/** Chow position: this app uses 0/1/2, the server LEFT/MIDDLE/RIGHT. */
const CHOW_WIRE = ["LEFT", "MIDDLE", "RIGHT"] as const;
export type ChowPos = 0 | 1 | 2;

export function chowToWire(pos: ChowPos): string {
  return CHOW_WIRE[pos];
}

export function chowFromWire(wire: string | null | undefined): ChowPos | null {
  const i = CHOW_WIRE.indexOf((wire ?? "") as (typeof CHOW_WIRE)[number]);
  return i < 0 ? null : (i as ChowPos);
}

/** Seat kinds on the server; the lobby offers these where it used to say "bot". */
export type SeatKind =
  | "human"
  | "open"
  | "ai_chicken"
  | "ai_random"
  | "ai_first_felix"
  | "ai_3point_chicken"
  | "ai_champion";

export const BOT_KINDS: SeatKind[] = [
  "ai_champion",
  "ai_first_felix",
  "ai_3point_chicken",
  "ai_chicken",
  "ai_random",
];

export const BOT_LABEL: Record<string, string> = {
  ai_champion: "Champion",
  ai_first_felix: "Felix",
  ai_3point_chicken: "3-point",
  ai_chicken: "Chicken",
  ai_random: "Random",
};

export function isBot(kind: string): boolean {
  return kind.startsWith("ai_");
}

export function asSeat(n: number): Seat {
  return (((n % 4) + 4) % 4) as Seat;
}
