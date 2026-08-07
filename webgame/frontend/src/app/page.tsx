"use client";

import { Suspense, useState } from "react";
import Link from "next/link";
import { useRouter, useSearchParams } from "next/navigation";
import { getName, setName as persistName } from "@/lib/identity";
import { createRoom as createRoomApi, getRoom, saveCreds, saveName } from "@/lib/server";
import { storeSetRoom } from "@/lib/store";
import { Tile } from "@/components/Tile";

function Landing() {
  const router = useRouter();
  const searchParams = useSearchParams();

  // This component renders inside a Suspense boundary and is never
  // prerendered (useSearchParams), so client-only lazy init is safe.
  const [name, setName] = useState(() => getName());
  const [joinCode, setJoinCode] = useState(() => {
    const join = searchParams.get("join");
    return join ? join.toUpperCase().replace(/[^A-Z0-9]/g, "").slice(0, 6) : "";
  });
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState<"create" | "join" | null>(null);

  const requireName = (): boolean => {
    if (!name.trim()) {
      setError("Please enter your name first.");
      return false;
    }
    persistName(name);
    saveName(name.trim());
    return true;
  };

  const createRoom = async () => {
    if (!requireName()) return;
    setBusy("create");
    setError(null);
    try {
      const res = await createRoomApi(`${name.trim()}'s table`, name.trim());
      // the creator holds seat 0 and is the host
      saveCreds(res.room.id, {
        playerId: res.hostPlayerId,
        seat: 0,
        isHost: true,
        name: name.trim(),
      });
      storeSetRoom(res.room, {
        playerId: res.hostPlayerId,
        seat: 0,
        isHost: true,
        name: name.trim(),
      });
      router.push(`/room/${res.room.code}`);
    } catch (e) {
      setError(e instanceof Error ? e.message : "Failed to reach the server.");
    } finally {
      setBusy(null);
    }
  };

  const joinRoom = async () => {
    if (!requireName()) return;
    const code = joinCode.trim().toUpperCase();
    if (code.length !== 6) {
      setError("Room codes are 6 characters.");
      return;
    }
    setBusy("join");
    setError(null);
    try {
      // just check it exists here; the room page claims the seat
      const room = await getRoom(code);
      router.push(`/room/${room.code || code}`);
    } catch (e) {
      setError(e instanceof Error ? e.message : "No room with that code.");
    } finally {
      setBusy(null);
    }
  };

  return (
    <main className="flex flex-1 flex-col items-center justify-center px-4 py-10">
      {/* decorative tiles */}
      <div className="mb-6 flex gap-2" aria-hidden>
        <Tile tile={31} size="lg" className="-rotate-6" />
        <Tile tile={32} size="lg" className="translate-y-[-6px]" />
        <Tile tile={33} size="lg" className="rotate-6" />
      </div>

      <h1 className="text-center text-5xl font-bold tracking-tight text-amber-300 drop-shadow-[0_2px_8px_rgba(0,0,0,0.6)] sm:text-6xl">
        香港麻雀
      </h1>
      <p className="mt-2 text-center text-lg text-emerald-100/80">
        Hong Kong Mahjong — play with friends & bots
      </p>

      <div className="mt-10 w-full max-w-sm space-y-5">
        <label className="block">
          <span className="mb-1 block text-sm font-medium text-emerald-100/80">
            Your name
          </span>
          <input
            value={name}
            onChange={(e) => setName(e.target.value)}
            maxLength={24}
            placeholder="e.g. 小明"
            className="w-full rounded-xl border border-white/15 bg-black/30 px-4 py-3 text-emerald-50 placeholder:text-emerald-100/30 focus:border-amber-400/70 focus:outline-none"
          />
        </label>

        <button
          onClick={createRoom}
          disabled={busy !== null}
          className="w-full rounded-xl bg-amber-400 py-3.5 text-lg font-bold text-emerald-950 shadow-lg transition-colors hover:bg-amber-300 disabled:opacity-50"
        >
          {busy === "create" ? "Creating…" : "開房 Create room"}
        </button>

        <div className="flex items-center gap-3 text-xs uppercase tracking-widest text-emerald-100/40">
          <span className="h-px flex-1 bg-white/10" />
          or
          <span className="h-px flex-1 bg-white/10" />
        </div>

        <div className="flex gap-2">
          <input
            value={joinCode}
            onChange={(e) =>
              setJoinCode(
                e.target.value
                  .toUpperCase()
                  .replace(/[^A-Z0-9]/g, "")
                  .slice(0, 6),
              )
            }
            onKeyDown={(e) => {
              if (e.key === "Enter") joinRoom();
            }}
            placeholder="ROOM CODE"
            className="min-w-0 flex-1 rounded-xl border border-white/15 bg-black/30 px-4 py-3 font-mono text-lg uppercase tracking-[0.25em] text-emerald-50 placeholder:tracking-normal placeholder:text-emerald-100/30 focus:border-amber-400/70 focus:outline-none"
          />
          <button
            onClick={joinRoom}
            disabled={busy !== null || joinCode.length !== 6}
            className="shrink-0 rounded-xl border border-amber-400/50 bg-black/30 px-5 font-semibold text-amber-300 transition-colors hover:bg-amber-400/10 disabled:opacity-40"
          >
            {busy === "join" ? "…" : "入房 Join"}
          </button>
        </div>

        {error && (
          <p className="rounded-lg border border-red-400/40 bg-red-950/50 px-3 py-2 text-sm text-red-200">
            {error}
          </p>
        )}
      </div>
      <Link
        href="/review"
        className="mt-6 text-sm text-emerald-100/60 underline-offset-4 hover:text-amber-300 hover:underline"
      >
        🎓 覆盤 Review my games
      </Link>

    </main>
  );
}

export default function Page() {
  return (
    <Suspense>
      <Landing />
    </Suspense>
  );
}
