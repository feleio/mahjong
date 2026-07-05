"use client";

import { useState } from "react";
import { Tile } from "./Tile";

/**
 * Your hand. During a discard decision each tile is tappable:
 * first tap selects (raise + highlight), second tap on the same tile discards.
 * The just-drawn tile is rendered separated at the right end with a marker.
 */
export function Hand({
  tiles,
  lastDrawnTile,
  canDiscard,
  onDiscard,
}: {
  tiles: number[];
  lastDrawnTile: number | null;
  canDiscard: boolean;
  onDiscard: (tile: number) => void;
}) {
  const [selected, setSelected] = useState<number | null>(null); // index into render list

  // Reset selection whenever the hand or discard-ability changes
  // (render-time state adjustment; see react.dev "you might not need an effect").
  const handKey = `${tiles.join(",")}|${canDiscard}`;
  const [prevHandKey, setPrevHandKey] = useState(handKey);
  if (prevHandKey !== handKey) {
    setPrevHandKey(handKey);
    setSelected(null);
  }

  // Separate one instance of the drawn tile to the right end.
  let main = tiles;
  let drawn: number | null = null;
  if (lastDrawnTile !== null) {
    const idx = tiles.indexOf(lastDrawnTile);
    if (idx >= 0) {
      main = [...tiles.slice(0, idx), ...tiles.slice(idx + 1)];
      drawn = lastDrawnTile;
    }
  }

  const render = (tile: number, key: number, isDrawn: boolean) => {
    const isSelected = selected === key;
    return (
      <div key={key} className="relative pt-3 pb-1">
        <Tile
          tile={tile}
          size="lg"
          selected={isSelected}
          highlighted={isDrawn}
          onClick={
            canDiscard
              ? () => {
                  if (isSelected) {
                    setSelected(null);
                    onDiscard(tile);
                  } else {
                    setSelected(key);
                  }
                }
              : undefined
          }
        />
        {isDrawn && (
          <span className="absolute left-1/2 top-0 h-1.5 w-1.5 -translate-x-1/2 rounded-full bg-amber-300" />
        )}
      </div>
    );
  };

  return (
    <div className="flex items-end gap-1 overflow-x-auto px-2 [-webkit-overflow-scrolling:touch]">
      {main.map((t, i) => render(t, i, false))}
      {drawn !== null && (
        <>
          <div className="w-3 shrink-0" />
          {render(drawn, main.length + 1000, true)}
        </>
      )}
    </div>
  );
}
