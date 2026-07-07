"use client";

import { useCallback, useEffect, useRef, useState } from "react";
import { useParams, useRouter } from "next/navigation";
import { getName } from "@/lib/identity";
import { ensureConnected, emitAck } from "@/lib/socket";
import {
  bindSocketToStore,
  storeClearDecision,
  storeClearGame,
  storeReset,
  storeSync,
  useStore,
} from "@/lib/store";
import type { PlainAck, RoomAck, StateAck } from "@/lib/types";
import { GameOverModal } from "@/components/GameOverModal";
import { GameTable } from "@/components/GameTable";
import { Lobby } from "@/components/Lobby";

export default function RoomPage() {
  const params = useParams<{ code: string }>();
  const code = (params.code ?? "").toUpperCase();
  const router = useRouter();
  const state = useStore();

  const [toast, setToast] = useState<string | null>(null);
  const [starting, setStarting] = useState(false);
  const toastTimer = useRef<ReturnType<typeof setTimeout> | null>(null);

  const showToast = useCallback((msg: string) => {
    setToast(msg);
    if (toastTimer.current) clearTimeout(toastTimer.current);
    toastTimer.current = setTimeout(() => setToast(null), 4000);
  }, []);

  useEffect(() => {
    return () => {
      if (toastTimer.current) clearTimeout(toastTimer.current);
    };
  }, []);

  // ---- connect / join / sync ----
  useEffect(() => {
    if (!code) return;
    if (!getName()) {
      // No identity yet — go set a name, then come back.
      router.replace(`/?join=${code}`);
      return;
    }

    bindSocketToStore();
    const socket = ensureConnected();
    let cancelled = false;

    const sync = async () => {
      try {
        const joinRes = await emitAck<RoomAck>("room:join", { code });
        if (cancelled) return;
        if (!joinRes.ok) {
          showToast(joinRes.error);
          router.replace("/");
          return;
        }
        const stateRes = await emitAck<StateAck>("room:state");
        if (cancelled) return;
        if (stateRes.ok) {
          storeSync(stateRes.room, stateRes.game, stateRes.decision);
        } else {
          showToast(stateRes.error);
        }
      } catch (e) {
        if (!cancelled) {
          showToast(e instanceof Error ? e.message : "Connection failed.");
        }
      }
    };

    // Initial sync (also runs immediately if already connected) and
    // re-sync on every (re)connect.
    if (socket.connected) void sync();
    const onConnect = () => void sync();
    socket.on("connect", onConnect);

    return () => {
      cancelled = true;
      socket.off("connect", onConnect);
    };
  }, [code, router, showToast]);

  // ---- room closed ----
  useEffect(() => {
    if (state.roomClosed !== null) {
      const reason = state.roomClosed;
      storeReset();
      alert(`Room closed: ${reason}`);
      router.replace("/");
    }
  }, [state.roomClosed, router]);

  const room = state.room;
  const isHost = room !== null && room.youUserId === room.hostUserId;

  // ---- actions ----
  const hostAction = useCallback(
    async (event: string, payload: object = {}) => {
      try {
        const res = await emitAck<PlainAck>(event, payload);
        if (!res.ok) showToast(res.error);
        return res.ok;
      } catch (e) {
        showToast(e instanceof Error ? e.message : "Request failed.");
        return false;
      }
    },
    [showToast],
  );

  const startGame = useCallback(async () => {
    setStarting(true);
    await hostAction("room:start");
    setStarting(false);
  }, [hostAction]);

  const onGameAction = useCallback(
    (action: boolean | number | null) => {
      const decision = state.decision;
      if (!decision) return;
      storeClearDecision();
      emitAck<PlainAck>("game:action", {
        requestId: decision.requestId,
        action,
      })
        .then((res) => {
          if (!res.ok) showToast(res.error);
        })
        .catch(() => showToast("Failed to send action."));
    },
    [state.decision, showToast],
  );

  // ---- render ----
  if (!room) {
    return (
      <main className="flex flex-1 flex-col items-center justify-center gap-3">
        <span className="h-8 w-8 animate-spin rounded-full border-2 border-amber-400 border-t-transparent" />
        <p className="text-emerald-100/70">Joining room {code}…</p>
      </main>
    );
  }

  const showTable =
    state.view !== null && (room.status === "playing" || state.gameOver !== null);

  return (
    <main className="flex min-h-0 flex-1 flex-col">
      {showTable && state.view ? (
        <GameTable
          room={room}
          view={state.view}
          decision={state.decision}
          decisionReceivedTs={state.decisionReceivedTs}
          waiting={state.waiting}
          lastEvent={state.lastEvent}
          lastEventId={state.lastEventId}
          onAction={onGameAction}
        />
      ) : (
        <Lobby
          room={room}
          isHost={isHost}
          starting={starting}
          onAddBot={() => void hostAction("room:addBot")}
          onRemoveBot={(seat) => void hostAction("room:removeBot", { seat })}
          onStart={() => void startGame()}
        />
      )}

      {state.gameOver && (
        <GameOverModal
          gameOver={state.gameOver}
          room={room}
          isHost={isHost}
          starting={starting}
          onNextRound={() => void startGame()}
          onBackToLobby={storeClearGame}
        />
      )}

      {/* connection banner */}
      {!state.connected && (
        <div className="fixed left-1/2 top-3 z-[60] -translate-x-1/2 rounded-full border border-red-400/50 bg-red-950/90 px-4 py-1.5 text-sm text-red-100 shadow-lg">
          Reconnecting…
        </div>
      )}

      {/* toast */}
      {toast && (
        <div className="fixed bottom-24 left-1/2 z-[60] w-max max-w-[90vw] -translate-x-1/2 rounded-xl border border-white/15 bg-black/85 px-4 py-2 text-sm text-emerald-50 shadow-xl">
          {toast}
        </div>
      )}
    </main>
  );
}
