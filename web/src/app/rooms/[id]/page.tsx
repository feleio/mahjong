"use client";

import { useEffect, useMemo, useRef, useState } from "react";
import { useParams, useRouter } from "next/navigation";
import { getRoom, setSeat, startGame, markReady, startNextGame, WS_BASE } from "@/lib/api";
import {
  AI_KINDS,
  ClientAction,
  CoachHint,
  ConnState,
  GameSnapshot,
  Prompt,
  PromptKind,
  Room,
  SeatKind,
  WsMessage,
  seatLabel,
} from "@/lib/types";
import { loadCreds, RoomCreds } from "@/lib/storage";
import GameTable from "@/components/GameTable";
import GameOverModal from "@/components/GameOverModal";
import PromptPanel from "@/components/PromptPanel";
import SeatConfig from "@/components/SeatConfig";

export default function RoomPage() {
  const router = useRouter();
  const params = useParams<{ id: string }>();
  const roomId = params.id;

  const [room,  setRoom]  = useState<Room | null>(null);
  const [creds, setCreds] = useState<RoomCreds | null>(null);
  const [snap,  setSnap]  = useState<GameSnapshot | null>(null);
  const [prompt,setPrompt]= useState<Prompt | null>(null);
  const [error,      setError]      = useState<string | null>(null);
  const [busy,       setBusy]       = useState(false);
  const [readySeats, setReadySeats] = useState<number[]>([]);
  const [myReady,    setMyReady]    = useState(false);
  const wsRef                           = useRef<WebSocket | null>(null);
  const [connState,  setConnState]  = useState<ConnState>("connecting");
  const [promptAt,   setPromptAt]   = useState<number | null>(null);
  const [autoPlayed, setAutoPlayed] = useState<PromptKind | null>(null);

  /* coach: champion hints on your prompts (issue #37). Off by default so
     first-time players aren't overwhelmed; persisted across sessions. */
  const [coachOn,  setCoachOn]  = useState(false);
  const [lastHint, setLastHint] = useState<CoachHint | null>(null);

  useEffect(() => {
    setCoachOn(window.localStorage.getItem("mahjong:coach") === "on");
  }, []);

  function toggleCoach() {
    setCoachOn((on) => {
      window.localStorage.setItem("mahjong:coach", on ? "off" : "on");
      return !on;
    });
  }

  useEffect(() => {
    setCreds(loadCreds(roomId));
    refresh();
  }, [roomId]);

  async function refresh() {
    try { setRoom(await getRoom(roomId)); }
    catch (e: any) { setError(String(e.message ?? e)); }
  }

  /* Connect the game websocket, and keep it connected.
     The socket must be re-established when the room's status changes: the
     server decides at connect time whether a game runner exists, so a socket
     opened in the lobby would never receive snapshots.
     Reconnection retries with backoff (a bare retry-on-close spins into a hot
     loop while the server is down) and reports its state to the player, so a
     dropped connection never looks like an empty table — issue #43. */
  useEffect(() => {
    if (!roomId || !room) return;

    let cancelled = false;
    let ws: WebSocket | null = null;
    let keepalive: ReturnType<typeof setInterval> | null = null;
    let retryTimer: ReturnType<typeof setTimeout> | null = null;
    let attempt = 0;

    const connect = () => {
      if (cancelled) return;
      if (keepalive) clearInterval(keepalive);
      setConnState(attempt === 0 ? "connecting" : "reconnecting");

      const qs = new URLSearchParams();
      if (creds) {
        qs.set("seat",   String(creds.seat));
        qs.set("player", creds.playerId);
      }
      const url = `${WS_BASE}/ws/rooms/${roomId}${qs.toString() ? `?${qs}` : ""}`;
      ws = new WebSocket(url);
      wsRef.current = ws;

      ws.onopen = () => {
        if (cancelled) return;
        attempt = 0;
        setConnState("open");
        setError(null);
        // the room may have moved on while we were disconnected
        refresh();
      };

      ws.onmessage = (ev) => {
        try {
          const msg = JSON.parse(ev.data) as WsMessage;
          switch (msg.type) {
            case "snapshot":
              setSnap(msg);
              break;
            case "prompt":
              setPrompt(msg);
              setPromptAt(Date.now());
              setAutoPlayed(null);
              if (msg.coach) setLastHint(msg.coach);
              break;
            case "timeout":
              // the engine just played a default for this seat (#42)
              if (creds && msg.seat === creds.seat) {
                setPrompt(null);
                setPromptAt(null);
                setAutoPlayed(msg.kind);
              }
              break;
            case "lobby":
              setRoom(msg.room);
              break;
            case "end":
              setPrompt(null);
              setPromptAt(null);
              break;
            case "ready_update":
              setReadySeats(msg.readySeats);
              break;
            case "error":
              setError(msg.message);
              break;
          }
        } catch {
          /* ignore non-json frames */
        }
      };

      ws.onclose = () => {
        const closed = ws;
        if (wsRef.current === closed) wsRef.current = null;
        if (cancelled) return;
        setConnState("reconnecting");
        const delay = Math.min(15_000, 500 * 2 ** attempt);
        attempt += 1;
        retryTimer = setTimeout(connect, delay);
      };

      /* keepalive: send a non-JSON ping every 30 s to prevent the server's
         idle-read timeout from closing an otherwise quiet connection */
      keepalive = setInterval(() => {
        if (ws?.readyState === WebSocket.OPEN) ws.send("ping");
      }, 30_000);
    };

    connect();

    return () => {
      cancelled = true;
      if (retryTimer) clearTimeout(retryTimer);
      if (keepalive) clearInterval(keepalive);
      const open = ws;
      if (open) {
        open.onclose = null;   // a deliberate teardown must not schedule a retry
        open.close();
      }
      if (wsRef.current === open) wsRef.current = null;
    };
  }, [roomId, room?.status, creds?.playerId]);

  /* re-poll the room while waiting (cheap) */
  useEffect(() => {
    if (!room || room.status !== "waiting") return;
    const t = setInterval(refresh, 2500);
    return () => clearInterval(t);
  }, [room?.status]);

  useEffect(() => {
    if (snap && !snap.isFinished) {
      setMyReady(false);
      setReadySeats([]);
    }
  }, [snap?.isFinished]);

  async function changeSeat(idx: number, kind: SeatKind) {
    if (!creds?.isHost || !room) return;
    setBusy(true); setError(null);
    try {
      const updated = await setSeat(roomId, creds.playerId, idx, kind);
      setRoom(updated);
    } catch (e: any) {
      setError(String(e.message ?? e));
    } finally {
      setBusy(false);
    }
  }

  async function start() {
    if (!creds?.isHost) return;
    setBusy(true); setError(null);
    try {
      const updated = await startGame(roomId, creds.playerId);
      setRoom(updated);
    } catch (e: any) {
      setError(String(e.message ?? e));
    } finally {
      setBusy(false);
    }
  }

  async function handleReady() {
    if (!creds) return;
    setBusy(true); setError(null);
    try {
      await markReady(roomId, creds.playerId);
      setMyReady(true);
    } catch (e: any) {
      setError(String(e.message ?? e));
    } finally {
      setBusy(false);
    }
  }

  async function handleStartNext() {
    if (!creds?.isHost) return;
    setBusy(true); setError(null);
    try {
      const updated = await startNextGame(roomId, creds.playerId);
      setRoom(updated);
      setSnap(null);
      setPrompt(null);
    } catch (e: any) {
      setError(String(e.message ?? e));
    } finally {
      setBusy(false);
    }
  }

  function send(action: ClientAction) {
    const ws = wsRef.current;
    if (!ws || ws.readyState !== WebSocket.OPEN) {
      setError("Not connected — your move was not sent. Reconnecting…");
      return;
    }
    ws.send(JSON.stringify(action));
    setPrompt(null);
    setPromptAt(null);
  }

  const isFull = useMemo(
    () => room?.seats.every(s => s.kind !== "open") ?? false,
    [room],
  );

  if (!room) {
    return (
      <div className="container">
        <p>Loading room…</p>
        {error && <div className="card" style={{ color: "var(--danger)" }}>{error}</div>}
      </div>
    );
  }

  return (
    <div className="container">
      <div className="row" style={{ justifyContent: "space-between", marginBottom: 12 }}>
        <h1>{room.name}</h1>
        <button className="ghost" onClick={() => router.push("/")}>← Lobby</button>
      </div>
      <p className="event">
        Room <code>{room.id}</code> · status: <strong>{room.status}</strong>
        {creds && <> · you are seat {creds.seat + 1}{creds.isHost ? " (host)" : ""}</>}
      </p>

      {connState !== "open" && (
        <div className="conn-banner">
          <span className="conn-dot" />
          {connState === "connecting"
            ? "Connecting to the table…"
            : "Connection lost — reconnecting. Your game is still running on the server."}
        </div>
      )}

      {autoPlayed && (
        <div className="autoplay-banner">
          You ran out of time on your {autoPlayed.replace("_", "-")} decision, so the engine
          played a default for you. That turn is marked as auto-played and is left out of your
          coaching stats.
          <button className="ghost" style={{ marginLeft: 10 }} onClick={() => setAutoPlayed(null)}>
            Dismiss
          </button>
        </div>
      )}

      {room.status === "waiting" && (
        <SeatConfig
          room={room}
          isHost={creds?.isHost ?? false}
          busy={busy}
          onChangeSeat={changeSeat}
          onStart={start}
          isFull={isFull}
        />
      )}

      {(room.status === "playing" || room.status === "finished") && snap && (
        <>
          <GameTable
            snap={snap}
            room={room}
            yourSeat={creds?.seat ?? null}
            prompt={creds && prompt?.seat === creds.seat ? prompt : null}
            onAct={send}
            coachOn={coachOn}
            onToggleCoach={creds ? toggleCoach : undefined}
            lastHint={lastHint}
            promptAt={promptAt}
          />
          {prompt && creds && prompt.seat === creds.seat && (
            <PromptPanel prompt={prompt} onAct={send} coachOn={coachOn} promptAt={promptAt} />
          )}
          {snap.isFinished && (
            <GameOverModal
              snap={snap}
              room={room}
              yourSeat={creds?.seat ?? null}
              isHost={creds?.isHost ?? false}
              readySeats={readySeats}
              myReady={myReady}
              busy={busy}
              onReady={handleReady}
              onStartNext={handleStartNext}
            />
          )}
        </>
      )}

      {error && <div className="card" style={{ color: "var(--danger)" }}>{error}</div>}
    </div>
  );
}
