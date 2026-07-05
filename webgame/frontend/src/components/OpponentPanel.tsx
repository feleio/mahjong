"use client";

import type { GameEvent, MeldGroup, SeatInfo } from "@/lib/types";
import { seatWindChar } from "@/lib/tiles";
import { MeldRow } from "./MeldRow";
import { TileBack } from "./Tile";

export function OpponentPanel({
  seatInfo,
  seat,
  dealerSeat,
  isTurn,
  handCount,
  melds,
  lastEvent,
  lastEventId,
  compact = false,
}: {
  seatInfo: SeatInfo | undefined;
  seat: number;
  dealerSeat: number;
  isTurn: boolean;
  handCount: number;
  melds: MeldGroup[];
  lastEvent: GameEvent | null;
  lastEventId: number;
  compact?: boolean;
}) {
  const isDealer = seat === dealerSeat;
  const name = seatInfo?.name ?? `Seat ${seat}`;
  const justDiscarded =
    lastEvent !== null && lastEvent.kind === "discard" && lastEvent.seat === seat;

  return (
    <div
      // Re-key on each new discard by this seat so the CSS flash re-triggers.
      key={justDiscarded ? `flash-${lastEventId}` : "steady"}
      className={`rounded-xl border px-2.5 py-2 transition-shadow duration-300
        ${isTurn ? "border-amber-400/80 bg-black/30 shadow-[0_0_16px_rgba(251,191,36,0.35)]" : "border-white/10 bg-black/20"}
        ${justDiscarded ? "discard-flash" : ""}`}
    >
      <div className="flex items-center gap-1.5 text-sm">
        {isDealer && (
          <span className="flex h-5 w-5 items-center justify-center rounded-full bg-red-700 text-[11px] font-bold text-amber-100">
            莊
          </span>
        )}
        <span className="rounded bg-white/10 px-1 text-[11px] text-emerald-100/80">
          {seatWindChar(seat, dealerSeat)}
        </span>
        <span
          className={`truncate font-medium ${isTurn ? "text-amber-200" : "text-emerald-50"}`}
        >
          {name}
        </span>
        {seatInfo?.isBot && (
          <span className="rounded bg-white/10 px-1 text-[10px] uppercase tracking-wide text-emerald-100/60">
            Bot
          </span>
        )}
        {seatInfo && !seatInfo.isBot && (
          <span
            className={`h-2 w-2 shrink-0 rounded-full ${seatInfo.connected ? "bg-emerald-400" : "bg-red-500"}`}
            title={seatInfo.connected ? "connected" : "disconnected"}
          />
        )}
      </div>

      <div className="mt-1.5 flex items-center gap-1">
        {compact ? (
          <span className="flex items-center gap-1 text-xs text-emerald-100/80">
            <TileBack size="sm" /> × {handCount}
          </span>
        ) : (
          Array.from({ length: handCount }).map((_, i) => (
            <TileBack key={i} size="sm" className="-ml-2 first:ml-0" />
          ))
        )}
      </div>

      <MeldRow melds={melds} size="sm" className="mt-1.5" />
    </div>
  );
}
