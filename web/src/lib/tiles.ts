// Tiny helpers for displaying tiles. Wire format examples: "D5" "B9" "C2" "HW_E" "HD_R".

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
