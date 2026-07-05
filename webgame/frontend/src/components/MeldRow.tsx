"use client";

import type { MeldGroup } from "@/lib/types";
import { Tile, type TileSize } from "./Tile";

export function MeldRow({
  melds,
  size = "sm",
  className = "",
}: {
  melds: MeldGroup[];
  size?: TileSize;
  className?: string;
}) {
  if (melds.length === 0) return null;
  return (
    <div className={`flex flex-wrap items-center gap-2 ${className}`}>
      {melds.map((meld, i) => (
        <div
          key={i}
          className="flex gap-[2px] rounded bg-black/20 p-[3px]"
          title={meld.kind}
        >
          {meld.tiles.map((t, j) => (
            <Tile key={j} tile={t} size={size} />
          ))}
        </div>
      ))}
    </div>
  );
}
