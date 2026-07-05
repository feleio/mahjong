"use client";

import type { GameWaitingPayload, RoomState } from "@/lib/types";

const DECISION_LABEL: Record<string, string> = {
  discard: "discarding",
  self_win: "deciding on a win",
  win: "deciding on a win",
  self_kong: "deciding on a kong",
  kong: "deciding on a kong",
  pong: "deciding on a pong",
  chow: "deciding on a chow",
};

export function WaitingIndicator({
  waiting,
  room,
}: {
  waiting: GameWaitingPayload;
  room: RoomState | null;
}) {
  const name = room?.seats[waiting.seat]?.name ?? `Seat ${waiting.seat}`;
  const what = DECISION_LABEL[waiting.decision] ?? waiting.decision;
  return (
    <div className="flex items-center gap-2 rounded-xl border border-white/10 bg-black/30 px-3 py-2 text-sm text-emerald-100/80">
      <span className="h-2 w-2 animate-pulse rounded-full bg-amber-400" />
      Waiting for {name} ({what})…
    </div>
  );
}
