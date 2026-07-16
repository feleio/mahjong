"use client";

import { useEffect, useMemo, useRef, useState } from "react";
import { useParams, useRouter } from "next/navigation";
import { getRoom, setSeat, startGame, markReady, startNextGame, WS_BASE } from "@/lib/api";
import {
  AI_KINDS,
  ClientAction,
  GameSnapshot,
  Prompt,
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
  const [wsEpoch, setWsEpoch]           = useState(0);

  useEffect(() => {
    setCreds(loadCreds(roomId));
    refresh();
  }, [roomId]);

  async function refresh() {
    try { setRoom(await getRoom(roomId)); }
    catch (e: any) { setError(String(e.message ?? e)); }
  }

  /* connect WS once we know whether we have credentials */
  useEffect(() => {
    if (!roomId || !room) return;
    if (wsRef.current) return;

    const qs = new URLSearchParams();
    if (creds) {
      qs.set("seat",   String(creds.seat));
      qs.set("player", creds.playerId);
    }
    const url = `${WS_BASE}/ws/rooms/${roomId}${qs.toString() ? `?${qs}` : ""}`;
    const ws = new WebSocket(url);
    wsRef.current = ws;

    ws.onmessage = (ev) => {
      try {
        const msg = JSON.parse(ev.data) as WsMessage;
        switch (msg.type) {
          case "snapshot":
            setSnap(msg);
            break;
          case "prompt":
            setPrompt(msg);
            break;
          case "lobby":
            setRoom(msg.room);
            break;
          case "end":
            setPrompt(null);
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
    ws.onerror = () => setError("WebSocket error");
    ws.onclose = () => {
      if (wsRef.current === ws) {
        wsRef.current = null;
        setWsEpoch(e => e + 1);
      }
    };

    /* keepalive: send a non-JSON ping every 30 s to prevent the server's
       idle-read timeout from closing an otherwise quiet connection */
    const keepalive = setInterval(() => {
      if (ws.readyState === WebSocket.OPEN) ws.send("ping");
    }, 30_000);

    return () => {
      clearInterval(keepalive);
      ws.close();
      if (wsRef.current === ws) wsRef.current = null;
    };
  }, [roomId, room?.status, creds?.playerId, wsEpoch]);

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
    if (!ws || ws.readyState !== WebSocket.OPEN) return;
    ws.send(JSON.stringify(action));
    setPrompt(null);
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
          />
          {prompt && creds && prompt.seat === creds.seat && (
            <PromptPanel prompt={prompt} onAct={send} />
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
