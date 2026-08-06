"use client";

import { useState, useEffect } from "react";
import { ClientAction, CoachHint, GameSnapshot, PlayerView, Prompt, Room } from "@/lib/types";
import Tile from "./Tile";

interface Props {
  snap:      GameSnapshot;
  room:      Room;
  yourSeat:  number | null;
  prompt?:   Prompt | null;
  onAct?:    (a: ClientAction) => void;
  coachOn?:  boolean;
  onToggleCoach?: () => void;
  lastHint?: CoachHint | null;   // most recent hint, survives between prompts
}

export default function GameTable({ snap, room, yourSeat, prompt, onAct, coachOn, onToggleCoach, lastHint }: Props) {
  const me = yourSeat !== null ? snap.players.find((p) => p.seat === yourSeat) : null;
  const [picked, setPicked] = useState<string | null>(null);
  const isDiscardTurn = prompt?.kind === "discard";

  useEffect(() => { setPicked(null); }, [prompt]);

  /* coach overlays: live discard hint only during your discard prompt */
  const discardHint = coachOn && isDiscardTurn ? prompt?.coach ?? null : null;
  const dangerByTile = discardHint?.dangerByTile ?? null;
  const maxDanger = dangerByTile
    ? Math.max(...Object.values(dangerByTile), 1e-9)
    : null;
  const tenpaiBySeat = new Map<number, number>(
    coachOn && lastHint?.oppTenpai ? lastHint.oppTenpai.map((o) => [o.seat, o.p]) : [],
  );

  return (
    <div className="card">
      <div className="row" style={{ justifyContent: "space-between" }}>
        <div className="row tight">
          <h3 style={{ margin: 0 }}>Table</h3>
          {onToggleCoach && (
            <button
              className={`coach-toggle ${coachOn ? "on" : "ghost"}`}
              onClick={onToggleCoach}
              title="Champion coach: shows what the strongest net would play"
            >
              🎓 Coach {coachOn ? "ON" : "off"}
            </button>
          )}
          {coachOn && lastHint && <ValueChip value={lastHint.value} />}
        </div>
        <span className="event">
          {snap.remainingTiles} tiles left · turn: seat {snap.curPlayer + 1} ({room.seats[snap.curPlayer]?.name})
        </span>
      </div>
      {snap.lastEvent && <div className="event">last: {snap.lastEvent}</div>}

      <div className="row" style={{ alignItems: "stretch", marginTop: 12 }}>
        {snap.players.filter(p => p.seat !== yourSeat).map((p) => (
          <SeatBlock
            key={p.seat}
            p={p}
            isTurn={p.seat === snap.curPlayer}
            isFinished={snap.isFinished}
            tenpaiP={tenpaiBySeat.get(p.seat)}
          />
        ))}
      </div>

      <div className="card" style={{ background: "var(--panel-2)", marginTop: 16 }}>
        <div className="label">Discards</div>
        <div className="discards" style={{ marginTop: 6 }}>
          {snap.discards.map((d, i) => (
            <Tile key={i} tile={d.tile} small />
          ))}
        </div>
      </div>

      {me && (
        <div className="card" style={{ marginTop: 16 }}>
          <div className="row" style={{ justifyContent: "space-between" }}>
            <div className="label">Your hand</div>
            {isDiscardTurn && <span className="event">Your turn — pick a tile to discard</span>}
          </div>
          {discardHint && (
            <div className="event coach-legend">
              🎓 <span className="jade">green %</span> = how likely the champion is to discard it
              {dangerByTile && <> · <span className="cinnabar">red bar</span> = relative deal-in risk</>}
            </div>
          )}
          <div className="hand" style={{ marginTop: 6 }}>
            {(me.handTiles ?? []).map((t, i) => {
              const prob = discardHint ? discardHint.probs[t] : undefined;
              const danger = dangerByTile && maxDanger ? (dangerByTile[t] ?? 0) / maxDanger : null;
              const isTop = discardHint?.top === t;
              return (
                <div key={i} className={`coach-tile ${isTop ? "top" : ""}`}>
                  <Tile
                    tile={t}
                    onClick={isDiscardTurn ? () => setPicked(t) : undefined}
                    highlight={picked === t}
                  />
                  {prob !== undefined && (
                    <span className="coach-prob" style={{ opacity: 0.45 + 0.55 * Math.min(1, prob * 3) }}>
                      {Math.round(prob * 100)}%
                    </span>
                  )}
                  {danger !== null && (
                    <span className="danger-bar" title="relative deal-in risk">
                      <span style={{ width: `${Math.round(danger * 100)}%` }} />
                    </span>
                  )}
                </div>
              );
            })}
          </div>
          {me.fixedGroups.length > 0 && (
            <div className="row tight" style={{ marginTop: 8 }}>
              <span className="event">groups:</span>
              {me.fixedGroups.flatMap((g, gi) => g.tiles.map((t, ti) => (
                <Tile key={`${gi}-${ti}`} tile={t} />
              )))}
            </div>
          )}
          <div className="row" style={{ marginTop: 8 }}>
            <button
              disabled={!isDiscardTurn || !picked}
              onClick={() => picked && onAct?.({ kind: "discard", tile: picked })}
            >
              Discard {picked ?? "…"}
            </button>
          </div>
        </div>
      )}
    </div>
  );
}

/** Champion value head for your seat, roughly money-scaled. Display-only. */
function ValueChip({ value }: { value: number }) {
  const clamped = Math.max(-10, Math.min(10, value));
  const pct = ((clamped + 10) / 20) * 100;
  return (
    <span className="value-chip" title="Champion's assessment of your seat (value head, money scale)">
      eval {value >= 0 ? "+" : ""}{value.toFixed(1)}
      <span className="value-track"><span className="value-fill" style={{ width: `${pct}%` }} /></span>
    </span>
  );
}

function SeatBlock({ p, isTurn, isFinished, tenpaiP }: {
  p: PlayerView; isTurn: boolean; isFinished: boolean; tenpaiP?: number;
}) {
  const tenpaiClass = tenpaiP === undefined ? "" : tenpaiP >= 0.6 ? "hot" : tenpaiP >= 0.3 ? "warm" : "cool";
  return (
    <div className={`seat ${isTurn ? "turn" : ""}`} style={{ flex: 1 }}>
      <div className="row" style={{ justifyContent: "space-between" }}>
        <div className="label">Seat {p.seat + 1}</div>
        {tenpaiP !== undefined && (
          <span className={`tenpai-badge ${tenpaiClass}`} title="Champion's estimate that this opponent is ready to win (tenpai)">
            ⚠ {Math.round(tenpaiP * 100)}%
          </span>
        )}
      </div>
      <div style={{ fontSize: 16, fontWeight: 600 }}>{p.name}</div>
      <div className="event">{p.kind === "human" ? "Human" : p.kind}</div>

      <div className="row tight" style={{ marginTop: 8 }}>
        <span className="event">hand:</span>
        {isFinished && p.handTiles ? (
          p.handTiles.map((t, i) => <Tile key={i} tile={t} small />)
        ) : (
          Array.from({ length: p.handCount }).map((_, i) => (
            <Tile key={i} tile="?" small faceDown />
          ))
        )}
      </div>

      {p.fixedGroups.length > 0 && (
        <div className="row tight" style={{ marginTop: 6 }}>
          <span className="event">groups:</span>
          {p.fixedGroups.flatMap((g, gi) => g.tiles.map((t, ti) => (
            <Tile key={`${gi}-${ti}`} tile={t} small />
          )))}
        </div>
      )}
    </div>
  );
}
