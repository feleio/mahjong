"use client";

import { useEffect, useState } from "react";
import type {
  Decision,
  GameEvent,
  GameView,
  GameWaitingPayload,
  RoomState,
  Seat,
} from "@/lib/types";
import { seatWindChar, seatWindEn, tileLabel, WIND_CHARS } from "@/lib/tiles";
import { ActionBar } from "./ActionBar";
import { Hand } from "./Hand";
import { MeldRow } from "./MeldRow";
import { OpponentPanel } from "./OpponentPanel";
import { River } from "./River";
import { WaitingIndicator } from "./WaitingIndicator";

function describeEvent(ev: GameEvent, seatName: (s: number) => string): string {
  switch (ev.kind) {
    case "draw":
      return ev.tile !== null
        ? `You drew ${tileLabel(ev.tile)}`
        : `${seatName(ev.seat)} drew a tile`;
    case "discard":
      return `${seatName(ev.seat)} discarded ${tileLabel(ev.tile)}`;
    case "pong":
      return `${seatName(ev.seat)} 碰 pong ${tileLabel(ev.tile)}`;
    case "kong":
      return `${seatName(ev.seat)} 槓 kong ${tileLabel(ev.tile)}`;
    case "chow":
      return `${seatName(ev.seat)} 上 chow ${tileLabel(ev.tile)}`;
  }
}

export function GameTable({
  room,
  view,
  decision,
  decisionReceivedTs,
  waiting,
  lastEvent,
  lastEventId,
  onAction,
}: {
  room: RoomState | null;
  view: GameView;
  decision: Decision | null;
  decisionReceivedTs: number;
  waiting: GameWaitingPayload | null;
  lastEvent: GameEvent | null;
  lastEventId: number;
  onAction: (action: boolean | number | null) => void;
}) {
  const you = view.yourSeat;
  const rightSeat = ((you + 1) % 4) as Seat; // next in turn order
  const topSeat = ((you + 2) % 4) as Seat;
  const leftSeat = ((you + 3) % 4) as Seat;

  const seatName = (s: number) => room?.seats[s]?.name ?? `Seat ${s}`;
  const yourInfo = room?.seats[you];
  const isYourTurn = view.curSeat === you;
  const canDiscard = decision?.decision === "discard";

  // AI-coach mode: show the model's probabilities on your decisions.
  // Off by default (hints spoil the game unless asked for); persisted.
  // Initialised in an effect, not the useState initialiser, to avoid an
  // SSR/client hydration mismatch on the toggle label.
  const [showCoach, setShowCoach] = useState(false);
  useEffect(() => {
    setShowCoach(localStorage.getItem("mj-coach") === "1");
  }, []);
  const toggleCoach = () => {
    setShowCoach((v) => {
      localStorage.setItem("mj-coach", v ? "0" : "1");
      return !v;
    });
  };
  const coachProbs = showCoach ? (decision?.coach?.probs ?? null) : null;

  const opponent = (seat: Seat, compact: boolean) => (
    <OpponentPanel
      seatInfo={room?.seats[seat]}
      seat={seat}
      dealerSeat={view.dealerSeat}
      isTurn={view.curSeat === seat}
      handCount={view.handCounts[seat] ?? 0}
      melds={view.melds[seat] ?? []}
      lastEvent={lastEvent}
      lastEventId={lastEventId}
      compact={compact}
    />
  );

  const centerInfo = (
    <div className="flex items-center justify-center gap-3 text-xs text-emerald-100/70">
      <span className="rounded-full border border-white/15 bg-black/30 px-2.5 py-1">
        牆 Wall: <span className="font-semibold tabular-nums text-emerald-50">{view.remaining}</span>
      </span>
      <span className="rounded-full border border-white/15 bg-black/30 px-2.5 py-1">
        莊 Dealer: {WIND_CHARS[0]} {seatName(view.dealerSeat)}
      </span>
    </div>
  );

  return (
    <div className="flex min-h-0 flex-1 flex-col">
      {/* status strip */}
      <div className="flex items-center justify-between gap-2 border-b border-white/10 bg-black/30 px-3 py-1.5 text-xs text-emerald-100/70">
        <span className="font-mono font-bold tracking-widest text-amber-300">
          {room?.code ?? ""}
        </span>
        <span className="truncate">
          Turn:{" "}
          <span className={view.curSeat === you ? "font-semibold text-amber-300" : "font-semibold text-emerald-50"}>
            {seatWindChar(view.curSeat, view.dealerSeat)}{" "}
            {seatWindEn(view.curSeat, view.dealerSeat)} · {seatName(view.curSeat)}
          </span>
        </span>
        <span className="tabular-nums">牆 {view.remaining}</span>
        <span className="hidden tabular-nums sm:inline">
          Game #{(room?.gamesPlayed ?? 0) + 1}
        </span>
        <button
          onClick={toggleCoach}
          title="Show the AI model's suggested action probabilities on your decisions"
          className={`rounded-full border px-2 py-0.5 text-[11px] font-semibold transition-colors ${
            showCoach
              ? "border-emerald-400/60 bg-emerald-500/20 text-emerald-200"
              : "border-white/15 bg-black/30 text-emerald-100/50 hover:text-emerald-100/80"
          }`}
        >
          🎓 Coach {showCoach ? "ON" : "OFF"}
        </button>
      </div>

      {/* ===== desktop layout ===== */}
      <div className="hidden min-h-0 flex-1 grid-cols-[minmax(11rem,1fr)_minmax(0,2.5fr)_minmax(11rem,1fr)] grid-rows-[auto_minmax(0,1fr)] gap-3 p-3 md:grid">
        <div className="col-start-2 row-start-1 flex justify-center">
          <div className="min-w-64">{opponent(topSeat, false)}</div>
        </div>
        <div className="col-start-1 row-start-2 flex items-center">
          <div className="w-full">{opponent(leftSeat, false)}</div>
        </div>
        <div className="col-start-2 row-start-2 flex min-h-0 flex-col rounded-2xl border border-white/10 bg-[#0a3527]/70 p-3 shadow-[inset_0_0_40px_rgba(0,0,0,0.4)]">
          {centerInfo}
          <div className="mt-2 min-h-0 flex-1 overflow-y-auto">
            <River view={view} size="sm" className="justify-center" />
          </div>
        </div>
        <div className="col-start-3 row-start-2 flex items-center">
          <div className="w-full">{opponent(rightSeat, false)}</div>
        </div>
      </div>

      {/* ===== mobile layout ===== */}
      <div className="flex min-h-0 flex-1 flex-col gap-2 p-2 md:hidden">
        <div className="grid grid-cols-3 gap-1.5">
          {opponent(leftSeat, true)}
          {opponent(topSeat, true)}
          {opponent(rightSeat, true)}
        </div>
        {centerInfo}
        <div className="min-h-0 flex-1 overflow-y-auto rounded-xl border border-white/10 bg-[#0a3527]/70 p-2">
          <River view={view} size="sm" />
        </div>
      </div>

      {/* ===== bottom: you (shared) ===== */}
      <div className="border-t border-amber-400/20 bg-black/30 px-2 pb-[max(env(safe-area-inset-bottom),0.5rem)] pt-2 backdrop-blur">
        {/* event feed line */}
        {lastEvent && (
          <p
            key={lastEventId}
            className="mb-1 animate-pulse truncate px-1 text-center text-xs text-emerald-100/60"
          >
            {describeEvent(lastEvent, seatName)}
          </p>
        )}

        <div className="mx-auto flex w-full max-w-3xl flex-col gap-1.5">
          {/* decision / waiting */}
          {decision ? (
            <ActionBar
              decision={decision}
              receivedTs={decisionReceivedTs}
              onAct={onAction}
              coach={decision.decision !== "discard" ? coachProbs : null}
            />
          ) : waiting && waiting.seat !== you ? (
            <WaitingIndicator waiting={waiting} room={room} />
          ) : null}

          {/* your identity + melds */}
          <div className="flex items-center gap-2 px-1 text-sm">
            {view.dealerSeat === you && (
              <span className="flex h-5 w-5 items-center justify-center rounded-full bg-red-700 text-[11px] font-bold text-amber-100">
                莊
              </span>
            )}
            <span className="rounded bg-white/10 px-1 text-[11px] text-emerald-100/80">
              {seatWindChar(you, view.dealerSeat)}
            </span>
            <span className={`font-semibold ${isYourTurn ? "text-amber-300" : "text-emerald-50"}`}>
              {yourInfo?.name ?? "You"}
            </span>
            {isYourTurn && (
              <span className="text-xs font-medium text-amber-300/90">— your turn</span>
            )}
            <MeldRow melds={view.melds[you] ?? []} size="sm" className="ml-auto" />
          </div>

          {/* your hand */}
          <Hand
            tiles={view.hand}
            lastDrawnTile={view.lastDrawnTile}
            canDiscard={canDiscard}
            onDiscard={(tile) => onAction(tile)}
            coach={canDiscard ? coachProbs : null}
          />
        </div>
      </div>
    </div>
  );
}
