// Tiny helpers for displaying tiles. Wire format examples: "D5" "B9" "C2" "HW_E" "HD_R".
import type { CSSProperties } from "react";

export function tileLabel(t: string): string {
  if (t.startsWith("D")) return `${t.slice(1)}🟠`;
  if (t.startsWith("B")) return `${t.slice(1)}🟢`;
  if (t.startsWith("C")) return `${t.slice(1)}🔴`;
  if (t === "HW_E") return "East";
  if (t === "HW_S") return "South";
  if (t === "HW_W") return "West";
  if (t === "HW_N") return "North";
  if (t === "HD_R") return "Red";
  if (t === "HD_G") return "Green";
  if (t === "HD_B") return "White";
  return t;
}

export function tileSuitColor(t: string): string {
  if (t.startsWith("D")) return "#d97706";
  if (t.startsWith("B")) return "#15803d";
  if (t.startsWith("C")) return "#b91c1c";
  return "#1f2937";
}

// ── Sprite sheet: /tiles.jpg (658×623) ────────────────────────────────────
// Layout: 4 rows × (9, 9, 9, 7) tiles
//   Row 0 – Characters (C1–C9)
//   Row 1 – Dots       (D1–D9)
//   Row 2 – Bamboo     (B1–B9)
//   Row 3 – Honors     HW_E HW_S HW_W HW_N HD_R HD_B HD_G
const SPR = { w: 658, h: 623, tw: 68, th: 140 };
const col = (i: number) => 3 + i * 73;  // x = 3 + i*(68+5)
const row = (j: number) => 8 + j * 155; // y = 8 + j*(140+15)

const SPRITE: Record<string, [number, number]> = {
  C1:[col(0),row(0)], C2:[col(1),row(0)], C3:[col(2),row(0)],
  C4:[col(3),row(0)], C5:[col(4),row(0)], C6:[col(5),row(0)],
  C7:[col(6),row(0)], C8:[col(7),row(0)], C9:[col(8),row(0)],

  D1:[col(0),row(1)], D2:[col(1),row(1)], D3:[col(2),row(1)],
  D4:[col(3),row(1)], D5:[col(4),row(1)], D6:[col(5),row(1)],
  D7:[col(6),row(1)], D8:[col(7),row(1)], D9:[col(8),row(1)],

  B1:[col(0),row(2)], B2:[col(1),row(2)], B3:[col(2),row(2)],
  B4:[col(3),row(2)], B5:[col(4),row(2)], B6:[col(5),row(2)],
  B7:[col(6),row(2)], B8:[col(7),row(2)], B9:[col(8),row(2)],

  HW_E:[col(0),row(3)], HW_S:[col(1),row(3)], HW_W:[col(2),row(3)],
  HW_N:[col(3),row(3)], HD_R:[col(4),row(3)], HD_B:[col(5),row(3)],
  HD_G:[col(6),row(3)],
};

/**
 * Returns inline CSS to show the tile image from the sprite sheet.
 * displayW / displayH are the rendered tile box dimensions in px.
 */
export function tileSpriteStyle(tile: string, displayW: number, displayH: number): CSSProperties | null {
  const pos = SPRITE[tile];
  if (!pos) return null;
  const [sx, sy] = pos;
  const scale  = displayW / SPR.tw;
  const yShift = (SPR.th * scale - displayH) / 2; // vertical centering crop
  return {
    backgroundImage:    "url('/tiles.jpg')",
    backgroundSize:     `${Math.round(SPR.w * scale)}px ${Math.round(SPR.h * scale)}px`,
    backgroundPosition: `${-Math.round(sx * scale)}px ${-Math.round(sy * scale + yShift)}px`,
    backgroundRepeat:   "no-repeat",
    padding:            0,
  };
}
