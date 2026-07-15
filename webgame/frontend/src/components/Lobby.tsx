"use client";

import { useState } from "react";
import type { RoomState } from "@/lib/types";
import { WIND_CHARS, WIND_EN } from "@/lib/tiles";

export function Lobby({
  room,
  isHost,
  starting,
  onAddBot,
  onRemoveBot,
  onSetTimeLimit,
  onStart,
}: {
  room: RoomState;
  isHost: boolean;
  starting: boolean;
  onAddBot: () => void;
  onRemoveBot: (seat: number) => void;
  onSetTimeLimit: (enabled: boolean) => void;
  onStart: () => void;
}) {
  const [copied, setCopied] = useState(false);
  const filledSeats = room.seats.filter((s) => !s.empty).length;

  const copyCode = async () => {
    try {
      await navigator.clipboard.writeText(room.code);
      setCopied(true);
      setTimeout(() => setCopied(false), 1500);
    } catch {
      // clipboard unavailable; ignore
    }
  };

  return (
    <div className="mx-auto flex w-full max-w-2xl flex-1 flex-col items-center px-4 py-8">
      {/* room code */}
      <p className="text-sm uppercase tracking-widest text-emerald-100/60">Room code</p>
      <button
        onClick={copyCode}
        title="Click to copy"
        className="mt-1 rounded-2xl border border-amber-400/30 bg-black/30 px-6 py-3 font-mono text-5xl font-bold tracking-[0.3em] text-amber-300 transition-colors hover:border-amber-400/70 sm:text-6xl"
      >
        {room.code}
      </button>
      <p className="mt-2 h-5 text-sm text-emerald-100/60">
        {copied ? "Copied!" : "Click to copy · share this code with friends"}
      </p>

      {/* seats */}
      <div className="mt-6 grid w-full grid-cols-1 gap-3 sm:grid-cols-2">
        {room.seats.map((seat) => {
          const isYou = seat.userId !== null && seat.userId === room.youUserId;
          const isSeatHost = seat.userId !== null && seat.userId === room.hostUserId;
          return (
            <div
              key={seat.seat}
              className={`relative rounded-xl border p-4 ${
                isYou
                  ? "border-amber-400/70 bg-amber-400/10"
                  : "border-white/10 bg-black/25"
              }`}
            >
              <div className="flex items-center gap-2 text-xs text-emerald-100/60">
                <span className="rounded bg-white/10 px-1.5 py-0.5 font-semibold text-emerald-100/90">
                  {WIND_CHARS[seat.seat]} {WIND_EN[seat.seat]}
                </span>
                {isSeatHost && <span className="text-amber-300">(Host)</span>}
                {isYou && <span className="font-semibold text-amber-300">You</span>}
              </div>
              <div className="mt-2 flex items-center gap-2">
                {seat.empty ? (
                  <span className="italic text-emerald-100/40">Waiting…</span>
                ) : (
                  <>
                    {!seat.isBot && (
                      <span
                        className={`h-2.5 w-2.5 rounded-full ${seat.connected ? "bg-emerald-400" : "bg-red-500"}`}
                        title={seat.connected ? "connected" : "disconnected"}
                      />
                    )}
                    <span className="truncate font-semibold text-emerald-50">
                      {seat.name}
                    </span>
                    {seat.isBot && (
                      <span className="rounded bg-white/10 px-1.5 py-0.5 text-[10px] font-semibold uppercase tracking-wide text-emerald-100/70">
                        Bot
                      </span>
                    )}
                  </>
                )}
              </div>
              {!seat.empty && (
                <p className="mt-1 text-xs tabular-nums text-emerald-100/60">
                  Balance: {seat.balance}
                </p>
              )}
              {isHost && seat.isBot && (
                <button
                  onClick={() => onRemoveBot(seat.seat)}
                  title="Remove bot"
                  className="absolute right-2 top-2 flex h-6 w-6 items-center justify-center rounded-full bg-white/10 text-emerald-100/70 transition-colors hover:bg-red-500/80 hover:text-white"
                >
                  ✕
                </button>
              )}
            </div>
          );
        })}
      </div>

      {/* controls */}
      <div className="mt-8 flex w-full flex-col items-center gap-3">
        {isHost ? (
          <>
            <div className="flex gap-3">
              <button
                onClick={onAddBot}
                disabled={filledSeats >= 4}
                className="rounded-xl border border-white/20 bg-white/10 px-5 py-2.5 font-semibold text-emerald-50 transition-colors hover:bg-white/20 disabled:opacity-40"
              >
                + Add bot
              </button>
              <button
                onClick={onStart}
                disabled={filledSeats < 1 || starting}
                className="rounded-xl bg-amber-400 px-8 py-2.5 font-bold text-emerald-950 shadow-lg transition-colors hover:bg-amber-300 disabled:opacity-40"
              >
                {starting ? "Starting…" : "開局 Start game"}
              </button>
            </div>
            <label
              className="flex cursor-pointer items-center gap-2 text-sm text-emerald-100/80"
              title="Off: think as long as you like (great with the AI coach). On: 45s to discard, 20s to claim."
            >
              <input
                type="checkbox"
                checked={room.enforceTimeLimit}
                onChange={(e) => onSetTimeLimit(e.target.checked)}
                className="h-4 w-4 accent-amber-400"
              />
              ⏱ Turn time limit
              <span className="text-emerald-100/50">
                {room.enforceTimeLimit ? "(45s / 20s)" : "(off — untimed)"}
              </span>
            </label>
            <p className="text-xs text-emerald-100/50">
              Empty seats are auto-filled with bots when the game starts.
            </p>
          </>
        ) : (
          <p className="flex items-center gap-2 text-emerald-100/70">
            <span className="h-2 w-2 animate-pulse rounded-full bg-amber-400" />
            Waiting for the host to start…
          </p>
        )}
        <p className="text-xs text-emerald-100/40">
          Games played in this room: {room.gamesPlayed}
        </p>
      </div>
    </div>
  );
}
