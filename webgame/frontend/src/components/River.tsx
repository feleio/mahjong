"use client";

import type { GameView } from "@/lib/types";
import { Tile, type TileSize } from "./Tile";

/**
 * Chronological discard river as a wrapping grid. The pending (claimable)
 * discard is rendered at the end with a pulsing ring.
 */
export function River({
  view,
  size = "sm",
  className = "",
}: {
  view: GameView;
  size?: TileSize;
  className?: string;
}) {
  return (
    <div className={`flex flex-wrap content-start gap-1 ${className}`}>
      {view.discards.map((d, i) => (
        <Tile key={i} tile={d.tile} size={size} />
      ))}
      {view.pendingDiscard && (
        <span className="relative inline-flex">
          <Tile tile={view.pendingDiscard.tile} size={size} />
          <span className="pointer-events-none absolute -inset-1 animate-ping rounded-md ring-2 ring-amber-400" />
          <span className="pointer-events-none absolute -inset-0.5 rounded-md ring-2 ring-amber-400" />
        </span>
      )}
      {view.discards.length === 0 && !view.pendingDiscard && (
        <span className="text-xs text-emerald-100/40">No discards yet</span>
      )}
    </div>
  );
}
