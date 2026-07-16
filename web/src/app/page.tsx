"use client";

import { useEffect, useState } from "react";
import { useRouter } from "next/navigation";
import { createRoom, joinRoom, listRooms } from "@/lib/api";
import { Room } from "@/lib/types";
import { loadName, saveCreds, saveName } from "@/lib/storage";

export default function LobbyPage() {
  const router = useRouter();
  const [rooms, setRooms] = useState<Room[]>([]);
  const [name, setName]   = useState("");
  const [roomName, setRoomName] = useState("My Mahjong Table");
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy]   = useState(false);

  useEffect(() => {
    setName(loadName());
    refresh();
    const t = setInterval(refresh, 4000);
    return () => clearInterval(t);
  }, []);

  async function refresh() {
    try { setRooms(await listRooms()); }
    catch (e: any) { setError(String(e.message ?? e)); }
  }

  async function handleCreate() {
    setError(null);
    if (!name.trim()) return setError("Please enter your name first");
    setBusy(true);
    try {
      saveName(name.trim());
      const resp = await createRoom(roomName.trim() || "Mahjong Table", name.trim());
      saveCreds(resp.room.id, {
        playerId: resp.hostPlayerId,
        seat: 0,
        isHost: true,
        name: name.trim(),
      });
      router.push(`/rooms/${resp.room.id}`);
    } catch (e: any) {
      setError(String(e.message ?? e));
    } finally {
      setBusy(false);
    }
  }

  async function handleJoin(room: Room) {
    setError(null);
    if (!name.trim()) return setError("Please enter your name first");
    setBusy(true);
    try {
      saveName(name.trim());
      const resp = await joinRoom(room.id, name.trim());
      saveCreds(resp.room.id, {
        playerId: resp.playerId,
        seat: resp.seat,
        isHost: false,
        name: name.trim(),
      });
      router.push(`/rooms/${resp.room.id}`);
    } catch (e: any) {
      setError(String(e.message ?? e));
    } finally {
      setBusy(false);
    }
  }

  return (
    <div className="container">
      <h1>🀄 Mahjong</h1>
      <p>Create a room and let others join, or jump into one waiting below.</p>

      <div className="card">
        <h2>Your name</h2>
        <input
          value={name}
          placeholder="e.g. Alice"
          onChange={(e) => setName(e.target.value)}
          style={{ minWidth: 240 }}
        />
      </div>

      <div className="card">
        <h2>Create a new room</h2>
        <div className="row">
          <input
            value={roomName}
            onChange={(e) => setRoomName(e.target.value)}
            style={{ minWidth: 260 }}
          />
          <button disabled={busy} onClick={handleCreate}>Create &amp; host</button>
        </div>
        <p className="event" style={{ marginTop: 8 }}>
          As host you can fill the other three seats with humans or AI opponents.
        </p>
      </div>

      <div className="card">
        <div className="row" style={{ justifyContent: "space-between" }}>
          <h2>Open rooms</h2>
          <button className="ghost" onClick={refresh}>Refresh</button>
        </div>
        {rooms.length === 0 && <p className="event">No rooms yet — create one.</p>}
        {rooms.map((r) => (
          <div key={r.id} className="row" style={{ justifyContent: "space-between", padding: "8px 0" }}>
            <div>
              <strong>{r.name}</strong>{" "}
              <span className="event">
                ({r.seats.filter(s => s.kind !== "open").length}/4 — {r.status})
              </span>
            </div>
            <div className="row">
              <button
                className="ghost"
                onClick={() => router.push(`/rooms/${r.id}`)}>
                Watch
              </button>
              <button
                disabled={busy || r.status !== "waiting" || !r.seats.some(s => s.kind === "open")}
                onClick={() => handleJoin(r)}>
                Join
              </button>
            </div>
          </div>
        ))}
      </div>

      {error && <div className="card" style={{ color: "var(--danger)" }}>{error}</div>}
    </div>
  );
}
