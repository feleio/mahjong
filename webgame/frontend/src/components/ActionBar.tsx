"use client";

import { useEffect, useState } from "react";
import type { Decision } from "@/lib/types";
import { chowRun, tileLabel } from "@/lib/tiles";
import { Tile } from "./Tile";

function Countdown({ deadlineTs, receivedTs }: { deadlineTs: number | null; receivedTs: number }) {
  const [now, setNow] = useState(() => Date.now());

  useEffect(() => {
    if (deadlineTs === null) return;
    const id = setInterval(() => setNow(Date.now()), 250);
    return () => clearInterval(id);
  }, [deadlineTs]);

  // Untimed decision (no time limit): render nothing.
  if (deadlineTs === null) return null;

  const total = Math.max(deadlineTs - receivedTs, 1);
  const left = Math.max(deadlineTs - now, 0);
  const pct = Math.min(100, Math.max(0, (left / total) * 100));
  const urgent = left < 5000;

  return (
    <div className="h-1.5 w-full overflow-hidden rounded-full bg-black/40">
      <div
        className={`h-full rounded-full transition-[width] duration-200 ease-linear ${urgent ? "bg-red-500" : "bg-amber-400"}`}
        style={{ width: `${pct}%` }}
      />
    </div>
  );
}

const BTN =
  "rounded-lg px-4 py-2 text-sm font-semibold shadow-md transition-colors disabled:opacity-50";
const BTN_GOLD = `${BTN} bg-amber-400 text-emerald-950 hover:bg-amber-300`;
const BTN_DARK = `${BTN} bg-white/10 text-emerald-50 hover:bg-white/20`;

/** AI-coach percentage badge next to an action label. */
function CoachPct({ p }: { p: number | undefined }) {
  if (p === undefined) return null;
  const strong = p >= 0.5;
  return (
    <span
      className={`ml-1 rounded px-1 text-[10px] font-bold tabular-nums ${
        strong ? "bg-emerald-500/30 text-emerald-200" : "bg-black/30 text-emerald-100/60"
      }`}
    >
      {Math.round(p * 100)}%
    </span>
  );
}

/**
 * Renders the buttons for the current Decision. `onAct` emits game:action
 * and clears the decision in the store. When `coach` is set, buttons carry
 * the model's probability for that action.
 */
export function ActionBar({
  decision,
  receivedTs,
  onAct,
  coach,
}: {
  decision: Decision;
  receivedTs: number;
  onAct: (action: boolean | number | null) => void;
  coach?: Record<string, number> | null;
}) {
  const { decision: kind, context } = decision;
  const cp = (key: string): number | undefined => coach?.[key];

  let body: React.ReactNode;
  switch (kind) {
    case "discard":
      body = (
        <p className="text-sm font-medium text-amber-200">
          Your turn — tap a tile, tap again to discard 打出
        </p>
      );
      break;

    case "self_win":
    case "win":
      body = (
        <div className="flex flex-wrap items-center gap-2">
          <span className="text-sm text-emerald-100/90">
            {kind === "self_win" ? "Self-draw win!" : `Win on ${context.tile !== undefined ? tileLabel(context.tile) : "discard"}!`}
            {context.score !== undefined && (
              <span className="ml-1 text-amber-300">({context.score} 番)</span>
            )}
          </span>
          <button className={BTN_GOLD} onClick={() => onAct(true)}>
            食糊 Win
            <CoachPct p={cp("accept")} />
          </button>
          <button className={BTN_DARK} onClick={() => onAct(false)}>
            過 Pass
            <CoachPct p={cp("pass")} />
          </button>
        </div>
      );
      break;

    case "pong":
    case "kong":
      body = (
        <div className="flex flex-wrap items-center gap-2">
          {context.tile !== undefined && <Tile tile={context.tile} size="sm" />}
          <button className={BTN_GOLD} onClick={() => onAct(true)}>
            {kind === "pong" ? "碰 Pong" : "槓 Kong"}
            <CoachPct p={cp("accept")} />
          </button>
          <button className={BTN_DARK} onClick={() => onAct(false)}>
            過 Pass
            <CoachPct p={cp("pass")} />
          </button>
        </div>
      );
      break;

    case "chow": {
      const tile = context.tile;
      const positions = context.positions ?? [];
      body = (
        <div className="flex flex-wrap items-center gap-2">
          <span className="text-sm text-emerald-100/90">上 Chow:</span>
          {tile !== undefined &&
            positions.map((p) => (
              <button
                key={p}
                onClick={() => onAct(p)}
                className="flex items-center gap-[2px] rounded-lg bg-white/10 p-1.5 ring-1 ring-white/20 transition-colors hover:bg-amber-400/20 hover:ring-amber-400"
              >
                {chowRun(tile, p).map((t, i) => (
                  <Tile key={i} tile={t} size="sm" highlighted={t === tile} />
                ))}
                <CoachPct p={cp(String(p))} />
              </button>
            ))}
          <button className={BTN_DARK} onClick={() => onAct(null)}>
            過 Pass
            <CoachPct p={cp("pass")} />
          </button>
        </div>
      );
      break;
    }

    case "self_kong": {
      const tiles = context.validTiles ?? [];
      body = (
        <div className="flex flex-wrap items-center gap-2">
          <span className="text-sm text-emerald-100/90">槓 Kong:</span>
          {tiles.map((t) => (
            <button
              key={t}
              onClick={() => onAct(t)}
              className="rounded-lg bg-white/10 p-1.5 ring-1 ring-white/20 transition-colors hover:bg-amber-400/20 hover:ring-amber-400"
            >
              <Tile tile={t} size="sm" />
              <CoachPct p={cp(String(t))} />
            </button>
          ))}
          <button className={BTN_DARK} onClick={() => onAct(null)}>
            過 Pass
            <CoachPct p={cp("pass")} />
          </button>
        </div>
      );
      break;
    }
  }

  return (
    <div className="flex flex-col gap-2 rounded-xl border border-amber-400/40 bg-black/40 px-3 py-2.5 backdrop-blur">
      {body}
      <Countdown deadlineTs={decision.deadlineTs} receivedTs={receivedTs} />
    </div>
  );
}
