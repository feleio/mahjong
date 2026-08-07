"use client";

import { useCallback, useEffect, useRef, useState } from "react";
import { useParams, useRouter } from "next/navigation";
import { getName } from "@/lib/identity";
import {
  getRoom,
  joinRoom,
  loadCreds,
  saveCreds,
  setSeat,
  startGame as startGameApi,
  startNextGame,
  markReady,
  type ServerRoom,
} from "@/lib/server";
import {
  connectRoom,
  disconnectRoom,
  sendAction,
  storeClearGame,
  storeDismissAutoPlayed,
  storeReset,
  storeSetRoom,
  useStore,
} from "@/lib/store";
import { BOT_KINDS } from "@/lib/wire";
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
  const [roomId, setRoomId] = useState<string | null>(null);
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

  // ---- resolve the room, take a seat, connect ----
  useEffect(() => {
    if (!code) return;
    if (!getName()) {
      // No identity yet — go set a name, then come back.
      router.replace(`/?join=${code}`);
      return;
    }

    let cancelled = false;

    const enter = async () => {
      try {
        // the URL carries the short code; everything else keys off the room id
        const room: ServerRoom = await getRoom(code);
        if (cancelled) return;
        setRoomId(room.id);

        // claim a seat once per room, then reuse it across reloads
        let creds = loadCreds(room.id);
        if (!creds && room.status === "waiting" && room.seats.some((s) => s.kind === "open")) {
          const joined = await joinRoom(room.id, getName());
          if (cancelled) return;
          creds = {
            playerId: joined.playerId,
            seat: joined.seat,
            isHost: joined.room.hostSeat === joined.seat,
            name: getName(),
          };
          saveCreds(room.id, creds);
        }
        const fresh = await getRoom(room.id);
        if (cancelled) return;
        storeSetRoom(fresh, loadCreds(room.id));
        connectRoom(room.id);
      } catch (e) {
        if (!cancelled) {
          showToast(e instanceof Error ? e.message : "Could not join that room.");
          router.replace("/");
        }
      }
    };

    void enter();
    return () => {
      cancelled = true;
      disconnectRoom();
      storeReset();
    };
  }, [code, router, showToast]);

  // The server decides at connect time whether a game is running, so the
  // socket has to be re-established when the room leaves or enters the lobby.
  const roomStatus = state.room?.status;
  useEffect(() => {
    if (!roomId) return;
    connectRoom(roomId);
  }, [roomId, roomStatus]);

  // ---- room closed ----
  useEffect(() => {
    if (state.roomClosed !== null) {
      const reason = state.roomClosed;
      storeReset();
      router.replace("/");
      showToast(`Room closed: ${reason}`);
    }
  }, [state.roomClosed, router, showToast]);

  const room = state.room;
  const creds = roomId ? loadCreds(roomId) : null;
  const isHost = creds?.isHost ?? false;

  const refreshRoom = useCallback(async () => {
    if (!roomId) return;
    try {
      storeSetRoom(await getRoom(roomId), loadCreds(roomId));
    } catch {
      /* the socket will resync */
    }
  }, [roomId]);

  // ---- host actions ----
  const seatBot = useCallback(
    async (seat: number, kind: string) => {
      if (!roomId || !creds) return;
      try {
        await setSeat(roomId, creds.playerId, seat, kind);
        await refreshRoom();
      } catch (e) {
        showToast(e instanceof Error ? e.message : "Could not change that seat.");
      }
    },
    [roomId, creds, refreshRoom, showToast],
  );

  const addBot = useCallback(() => {
    const open = room?.seats.find((s) => s.empty);
    if (!open) return;
    // strongest first: playing the champion is the point of the app
    void seatBot(open.seat, BOT_KINDS[0]);
  }, [room, seatBot]);

  const removeBot = useCallback((seat: number) => void seatBot(seat, "open"), [seatBot]);

  const startGame = useCallback(async () => {
    if (!roomId || !creds) return;
    setStarting(true);
    try {
      // the first game starts the room; later ones need everyone ready
      if (state.room?.gamesPlayed && state.gameOver) {
        await markReady(roomId, creds.playerId);
        await startNextGame(roomId, creds.playerId);
      } else {
        await startGameApi(roomId, creds.playerId);
      }
      storeClearGame();
      await refreshRoom();
    } catch (e) {
      showToast(e instanceof Error ? e.message : "Could not start the game.");
    } finally {
      setStarting(false);
    }
  }, [roomId, creds, state.room?.gamesPlayed, state.gameOver, refreshRoom, showToast]);

  const onGameAction = useCallback(
    (action: boolean | number | null) => {
      const decision = state.decision;
      if (!decision) return;
      if (!sendAction(decision.decision, action)) {
        showToast("Not connected — your move was not sent.");
      }
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
          onAddBot={addBot}
          onRemoveBot={removeBot}
          onSetTimeLimit={() => showToast("The turn clock is always on with this server.")}
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

      {/* the engine moved for you because the clock ran out (#42) */}
      {state.autoPlayed && (
        <button
          onClick={storeDismissAutoPlayed}
          className="fixed left-1/2 top-3 z-[60] -translate-x-1/2 rounded-full border border-red-400/50 bg-red-950/90 px-4 py-1.5 text-sm text-red-100 shadow-lg"
        >
          Time ran out on your {state.autoPlayed.replace("_", "-")} — the engine played for you.
          That turn is left out of your coaching stats. Tap to dismiss.
        </button>
      )}

      {/* connection banner */}
      {state.conn !== "open" && (
        <div className="fixed left-1/2 top-3 z-[60] -translate-x-1/2 rounded-full border border-amber-400/50 bg-amber-950/90 px-4 py-1.5 text-sm text-amber-100 shadow-lg">
          {state.conn === "connecting" ? "Connecting…" : "Reconnecting…"}
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
