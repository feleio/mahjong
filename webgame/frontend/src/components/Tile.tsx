"use client";

import { CHINESE_NUMERALS, tileRank, tileSuit, WIND_CHARS } from "@/lib/tiles";

export type TileSize = "sm" | "md" | "lg";

const FRAME: Record<TileSize, string> = {
  sm: "w-6 h-8 rounded-[4px] border",
  md: "w-9 h-12 rounded-md border",
  lg: "w-12 h-16 rounded-lg border-2",
};

const NUM_TEXT: Record<TileSize, string> = {
  sm: "text-[11px] leading-none",
  md: "text-lg leading-none",
  lg: "text-2xl leading-none",
};

const CJK_TEXT: Record<TileSize, string> = {
  sm: "text-[9px] leading-none",
  md: "text-sm leading-none",
  lg: "text-lg leading-none",
};

const BIG_CJK: Record<TileSize, string> = {
  sm: "text-sm leading-none",
  md: "text-xl leading-none",
  lg: "text-3xl leading-none",
};

const WHITE_BOX: Record<TileSize, string> = {
  sm: "w-3.5 h-5 rounded-[2px] border",
  md: "w-5 h-8 rounded-[3px] border-2",
  lg: "w-7 h-11 rounded-[4px] border-[3px]",
};

function TileFace({ tile, size }: { tile: number; size: TileSize }) {
  const suit = tileSuit(tile);

  if (suit === "dots") {
    return (
      <span className="flex flex-col items-center justify-center gap-[0.1em]">
        <span className={`${NUM_TEXT[size]} font-bold text-blue-700`}>
          {tileRank(tile)}
        </span>
        <span className={`${CJK_TEXT[size]} font-semibold text-blue-700`}>筒</span>
      </span>
    );
  }
  if (suit === "bamboo") {
    return (
      <span className="flex flex-col items-center justify-center gap-[0.1em]">
        <span className={`${NUM_TEXT[size]} font-bold text-emerald-700`}>
          {tileRank(tile)}
        </span>
        <span className={`${CJK_TEXT[size]} font-semibold text-emerald-700`}>索</span>
      </span>
    );
  }
  if (suit === "characters") {
    return (
      <span className="flex flex-col items-center justify-center gap-[0.1em]">
        <span className={`${CJK_TEXT[size]} font-bold text-red-700`}>
          {CHINESE_NUMERALS[tile - 18]}
        </span>
        <span className={`${CJK_TEXT[size]} font-bold text-red-700`}>萬</span>
      </span>
    );
  }
  if (suit === "wind") {
    return (
      <span className={`${BIG_CJK[size]} font-bold text-neutral-900`}>
        {WIND_CHARS[tile - 27]}
      </span>
    );
  }
  // dragons: 中 red, 發 green, 白 blue outline box
  if (tile === 31) {
    return <span className={`${BIG_CJK[size]} font-bold text-red-600`}>中</span>;
  }
  if (tile === 32) {
    return <span className={`${BIG_CJK[size]} font-bold text-emerald-700`}>發</span>;
  }
  return <span className={`${WHITE_BOX[size]} border-blue-600`} aria-hidden />;
}

export function Tile({
  tile,
  size = "md",
  selected = false,
  highlighted = false,
  onClick,
  className = "",
}: {
  tile: number;
  size?: TileSize;
  selected?: boolean;
  highlighted?: boolean;
  onClick?: () => void;
  className?: string;
}) {
  const base = `${FRAME[size]} shrink-0 flex items-center justify-center select-none
    bg-gradient-to-b from-[#fffdf5] to-[#f0ead8] border-[#c9bfa3]
    shadow-[0_2px_3px_rgba(0,0,0,0.45),inset_0_1px_0_rgba(255,255,255,0.9)]`;
  const sel = selected
    ? "-translate-y-2 ring-2 ring-amber-400 shadow-[0_6px_10px_rgba(0,0,0,0.5)]"
    : "";
  const hl = highlighted && !selected ? "ring-2 ring-amber-300/80" : "";
  const inter = onClick
    ? "cursor-pointer transition-transform duration-100 active:scale-95"
    : "";

  if (onClick) {
    return (
      <button
        type="button"
        onClick={onClick}
        className={`${base} ${sel} ${hl} ${inter} ${className}`}
      >
        <TileFace tile={tile} size={size} />
      </button>
    );
  }
  return (
    <span className={`${base} ${sel} ${hl} ${className}`}>
      <TileFace tile={tile} size={size} />
    </span>
  );
}

export function TileBack({
  size = "md",
  className = "",
}: {
  size?: TileSize;
  className?: string;
}) {
  return (
    <span
      className={`${FRAME[size]} shrink-0 select-none border-[#0e4a38]
        bg-gradient-to-b from-[#1e7a5c] to-[#14543f]
        shadow-[0_2px_3px_rgba(0,0,0,0.45),inset_0_1px_0_rgba(255,255,255,0.15)]
        ${className}`}
      aria-hidden
    />
  );
}
