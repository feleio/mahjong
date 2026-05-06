"use client";

import { tileLabel, tileSuitColor } from "@/lib/tiles";

interface Props {
  tile: string;
  small?: boolean;
  faceDown?: boolean;
  onClick?: () => void;
  highlight?: boolean;
}

export default function Tile({ tile, small, faceDown, onClick, highlight }: Props) {
  if (faceDown) {
    return (
      <div className={`tile ${small ? "small" : ""} face-down`}>?</div>
    );
  }
  return (
    <div
      className={`tile ${small ? "small" : ""} ${onClick ? "action" : ""}`}
      onClick={onClick}
      style={{
        color: tileSuitColor(tile),
        outline: highlight ? "2px solid #fde047" : undefined,
      }}
      title={tile}
    >
      {tileLabel(tile)}
    </div>
  );
}
