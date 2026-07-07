"use client";

import type { GameOverInfo } from "@/lib/store";
import type { RoomState } from "@/lib/types";
import { tileLabel } from "@/lib/tiles";
import { MeldRow } from "./MeldRow";
import { Tile } from "./Tile";

export function GameOverModal({
  gameOver,
  room,
  isHost,
  starting,
  onNextRound,
  onBackToLobby,
}: {
  gameOver: GameOverInfo;
  room: RoomState | null;
  isHost: boolean;
  starting: boolean;
  onNextRound: () => void;
  onBackToLobby: () => void;
}) {
  const { winnersInfo, balances, view } = gameOver;
  const seatName = (seat: number) => room?.seats[seat]?.name ?? `Seat ${seat}`;

  return (
    <div className="fixed inset-0 z-50 flex items-center justify-center bg-black/70 p-4 backdrop-blur-sm">
      <div className="max-h-[90dvh] w-full max-w-lg overflow-y-auto rounded-2xl border border-amber-400/30 bg-[#0d3327] p-5 shadow-2xl">
        {winnersInfo ? (
          <>
            <h2 className="text-center text-3xl font-bold text-amber-300">
              食糊&nbsp;
              <span className="text-lg font-medium text-amber-100/80">Win!</span>
            </h2>
            <div className="mt-3 flex items-center justify-center gap-2 text-emerald-50">
              <span className="text-sm text-emerald-100/70">Winning tile</span>
              <Tile tile={winnersInfo.winningTile} size="md" highlighted />
              <span className="text-sm text-emerald-100/70">
                {tileLabel(winnersInfo.winningTile)}
                {winnersInfo.isSelfWin ? " · 自摸 self-draw" : ""}
              </span>
            </div>
            <div className="mt-3 space-y-1 text-center">
              {winnersInfo.winners.map((w) => (
                <p key={w.seat} className="text-emerald-50">
                  <span className="font-semibold text-amber-200">{seatName(w.seat)}</span>{" "}
                  wins with <span className="font-semibold">{w.score} 番 faan</span>
                </p>
              ))}
              {winnersInfo.loserSeat !== null && (
                <p className="text-sm text-emerald-100/60">
                  Discarded by {seatName(winnersInfo.loserSeat)}
                </p>
              )}
            </div>
          </>
        ) : (
          <h2 className="text-center text-3xl font-bold text-emerald-100">
            流局&nbsp;
            <span className="text-lg font-medium text-emerald-100/70">Draw — wall exhausted</span>
          </h2>
        )}

        {/* balances */}
        <div className="mt-5 overflow-hidden rounded-xl border border-white/10">
          {([0, 1, 2, 3] as const).map((seat) => {
            const delta = balances[seat] ?? 0;
            const total = room?.seats[seat]?.balance;
            const isWinner = winnersInfo?.winners.some((w) => w.seat === seat) ?? false;
            return (
              <div
                key={seat}
                className={`flex items-center justify-between px-3 py-2 text-sm ${isWinner ? "bg-amber-400/10" : "odd:bg-black/20"}`}
              >
                <span className="text-emerald-50">
                  {seatName(seat)}
                  {seat === view.yourSeat && (
                    <span className="ml-1 text-xs text-amber-300">(You)</span>
                  )}
                </span>
                <span className="flex items-baseline gap-2 tabular-nums">
                  <span
                    className={
                      delta > 0
                        ? "font-semibold text-emerald-300"
                        : delta < 0
                          ? "font-semibold text-red-400"
                          : "text-emerald-100/50"
                    }
                  >
                    {delta > 0 ? `+${delta}` : delta}
                  </span>
                  {total !== undefined && (
                    <span className="text-xs text-emerald-100/60">total {total}</span>
                  )}
                </span>
              </div>
            );
          })}
        </div>

        {/* revealed hands */}
        <div className="mt-5 space-y-3">
          {([0, 1, 2, 3] as const).map((seat) => {
            const revealed =
              view.hands?.[seat] ?? (seat === view.yourSeat ? view.hand : null);
            const melds = view.melds[seat] ?? [];
            if (!revealed && melds.length === 0) return null;
            return (
              <div key={seat}>
                <p className="mb-1 text-xs uppercase tracking-wide text-emerald-100/50">
                  {seatName(seat)}
                </p>
                <div className="flex flex-wrap items-center gap-2">
                  {revealed && (
                    <div className="flex flex-wrap gap-[2px]">
                      {revealed.map((t, i) => (
                        <Tile key={i} tile={t} size="sm" />
                      ))}
                    </div>
                  )}
                  <MeldRow melds={melds} size="sm" />
                </div>
              </div>
            );
          })}
        </div>

        {/* actions */}
        <div className="mt-6 flex flex-col items-center gap-2">
          {isHost ? (
            <button
              onClick={onNextRound}
              disabled={starting}
              className="w-full rounded-xl bg-amber-400 py-3 text-base font-bold text-emerald-950 shadow-lg transition-colors hover:bg-amber-300 disabled:opacity-50"
            >
              {starting ? "Starting…" : "Next round 再嚟一局"}
            </button>
          ) : (
            <p className="text-sm text-emerald-100/70">Waiting for the host to start the next round…</p>
          )}
          <button
            onClick={onBackToLobby}
            className="text-sm text-emerald-100/60 underline-offset-2 hover:text-emerald-100 hover:underline"
          >
            Back to lobby
          </button>
        </div>
      </div>
    </div>
  );
}
