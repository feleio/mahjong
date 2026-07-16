"use client";

import { tileLabel, tileSpriteStyle } from "@/lib/tiles";

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
      <div className={`tile ${small ? "small" : ""} face-down`} />
    );
  }

  const displayW = small ? 22 : 38;
  const displayH = small ? 30 : 56;
  const sprite = tileSpriteStyle(tile, displayW, displayH);

  return (
    <div
      className={`tile ${small ? "small" : ""} ${onClick ? "action" : ""}`}
      onClick={onClick}
      style={{
        outline: highlight ? "2px solid #fde047" : undefined,
        ...(sprite ?? {}),
      }}
      title={tileLabel(tile)}
    >
      {!sprite && tileLabel(tile)}
    </div>
  );
}
