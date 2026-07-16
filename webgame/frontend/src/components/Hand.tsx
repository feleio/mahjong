"use client";

import { useState } from "react";
import { Tile } from "./Tile";

/**
 * Your hand. During a discard decision each tile is tappable:
 * first tap selects (raise + highlight), second tap on the same tile discards.
 * The just-drawn tile is rendered separated at the right end with a marker.
 *
 * When `coach` is set (AI-coach mode, discard decisions only) each tile shows
 * the model's discard probability as a heat-colored percentage badge; the
 * model's top pick gets a matching ring.
 *
 * When `danger` is set (v4 danger coach models) each tile additionally gets
 * a red bar above it whose intensity is the RELATIVE deal-in risk (max over
 * opponents of p(tenpai)·p(waits on tile)) — a second, visually distinct
 * channel from the green/red discard heat below.
 */
export function Hand({
  tiles,
  lastDrawnTile,
  canDiscard,
  onDiscard,
  coach,
  danger,
}: {
  tiles: number[];
  lastDrawnTile: number | null;
  canDiscard: boolean;
  onDiscard: (tile: number) => void;
  coach?: Record<string, number> | null;
  danger?: Record<string, number> | null;
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

  // Coach heat scale: probability relative to the model's top pick drives the
  // hue (dim red → bright green). Only meaningful during a discard decision.
  const showCoach = canDiscard && coach && Object.keys(coach).length > 0;
  const maxProb = showCoach ? Math.max(...Object.values(coach)) : 1;
  const heat = (p: number) => {
    const s = maxProb > 0 ? p / maxProb : 0; // 0..1 relative strength
    return `hsl(${Math.round(10 + 125 * s)}, 85%, ${Math.round(42 + 18 * s)}%)`;
  };

  // Danger channel: relative deal-in risk across the tiles in hand.
  const showDanger = canDiscard && danger && Object.keys(danger).length > 0;
  const maxDanger = showDanger
    ? Math.max(...tiles.map((t) => danger[String(t)] ?? 0), 1e-9)
    : 1;

  const render = (tile: number, key: number, isDrawn: boolean) => {
    const isSelected = selected === key;
    const prob = showCoach ? (coach[String(tile)] ?? 0) : null;
    const isTopPick = prob !== null && maxProb > 0 && prob === maxProb;
    const risk = showDanger ? (danger[String(tile)] ?? 0) : null;
    const riskRel = risk !== null ? risk / maxDanger : 0;
    return (
      <div key={key} className="relative pt-3 pb-1">
        {risk !== null && riskRel > 0.1 && (
          <span
            title={`Coach: deal-in risk ~${(risk * 100).toFixed(1)}% (relative heat — an opponent may be waiting on this tile)`}
            className="absolute inset-x-1 top-1 h-1 rounded-full bg-red-500"
            style={{ opacity: 0.25 + 0.75 * riskRel }}
          />
        )}
        <div
          className="rounded-lg"
          style={isTopPick ? { boxShadow: `0 0 0 2px ${heat(prob!)}` } : undefined}
        >
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
        </div>
        {isDrawn && (
          <span className="absolute left-1/2 top-0 h-1.5 w-1.5 -translate-x-1/2 rounded-full bg-amber-300" />
        )}
        {prob !== null && (
          <span
            className="pointer-events-none absolute inset-x-0 -bottom-3.5 text-center text-[10px] font-semibold tabular-nums"
            style={{ color: heat(prob) }}
          >
            {prob >= 0.005 ? `${Math.round(prob * 100)}%` : "·"}
          </span>
        )}
      </div>
    );
  };

  return (
    <div
      className={`flex items-end gap-1 overflow-x-auto px-2 [-webkit-overflow-scrolling:touch] ${showCoach ? "pb-4" : ""}`}
    >
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
