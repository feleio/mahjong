// Tile encoding helpers. Tiles are integers 0–33:
// 0–8 Dots (筒) 1–9, 9–17 Bamboo (索) 1–9, 18–26 Characters (萬) 1–9,
// 27–30 Winds E S W N (東南西北), 31–33 Dragons Red Green White (中發白).

export type TileSuit = "dots" | "bamboo" | "characters" | "wind" | "dragon";

export const CHINESE_NUMERALS = [
  "一",
  "二",
  "三",
  "四",
  "五",
  "六",
  "七",
  "八",
  "九",
] as const;

export const WIND_CHARS = ["東", "南", "西", "北"] as const;
export const WIND_EN = ["East", "South", "West", "North"] as const;
export const DRAGON_CHARS = ["中", "發", "白"] as const;

export function tileSuit(tile: number): TileSuit {
  if (tile <= 8) return "dots";
  if (tile <= 17) return "bamboo";
  if (tile <= 26) return "characters";
  if (tile <= 30) return "wind";
  return "dragon";
}

/** 1-9 rank for suited tiles. */
export function tileRank(tile: number): number {
  return (tile % 9) + 1;
}

/** Short human label, e.g. "5筒", "3索", "七萬", "東", "中". */
export function tileLabel(tile: number): string {
  const suit = tileSuit(tile);
  switch (suit) {
    case "dots":
      return `${tileRank(tile)}筒`;
    case "bamboo":
      return `${tileRank(tile)}索`;
    case "characters":
      return `${CHINESE_NUMERALS[tile - 18]}萬`;
    case "wind":
      return WIND_CHARS[tile - 27];
    case "dragon":
      return DRAGON_CHARS[tile - 31];
  }
}

/** Seat wind relative to the dealer (dealer is always East). */
export function seatWindIndex(seat: number, dealerSeat: number): number {
  return (seat - dealerSeat + 4) % 4;
}

export function seatWindChar(seat: number, dealerSeat: number): string {
  return WIND_CHARS[seatWindIndex(seat, dealerSeat)];
}

export function seatWindEn(seat: number, dealerSeat: number): string {
  return WIND_EN[seatWindIndex(seat, dealerSeat)];
}

/**
 * Tiles of a chow run given the claimed tile and position id.
 * 0 = LEFT (claimed tile is lowest), 1 = MIDDLE, 2 = RIGHT.
 */
export function chowRun(tile: number, position: 0 | 1 | 2): number[] {
  switch (position) {
    case 0:
      return [tile, tile + 1, tile + 2];
    case 1:
      return [tile - 1, tile, tile + 1];
    case 2:
      return [tile - 2, tile - 1, tile];
  }
}
