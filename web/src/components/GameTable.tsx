"use client";

import { GameSnapshot, PlayerView, Room } from "@/lib/types";
import Tile from "./Tile";

interface Props {
  snap: GameSnapshot;
  room: Room;
  yourSeat: number | null;
}

export default function GameTable({ snap, room, yourSeat }: Props) {
  const me = yourSeat !== null ? snap.players.find((p) => p.seat === yourSeat) : null;

  return (
    <div className="card">
      <div className="row" style={{ justifyContent: "space-between" }}>
        <h3>Table</h3>
        <span className="event">
          {snap.remainingTiles} tiles left · turn: seat {snap.curPlayer + 1} ({room.seats[snap.curPlayer]?.name})
        </span>
      </div>
      {snap.lastEvent && <div className="event">last: {snap.lastEvent}</div>}

      <div className="row" style={{ alignItems: "stretch", marginTop: 12 }}>
        {snap.players.map((p) => (
          <SeatBlock
            key={p.seat}
            p={p}
            you={p.seat === yourSeat}
            isTurn={p.seat === snap.curPlayer}
          />
        ))}
      </div>

      <div className="card" style={{ background: "var(--panel-2)", marginTop: 16 }}>
        <div className="label">Discards</div>
        <div className="discards" style={{ marginTop: 6 }}>
          {snap.discards.map((d, i) => (
            <Tile key={i} tile={d.tile} small />
          ))}
        </div>
      </div>

      {me && (
        <div className="card" style={{ marginTop: 16 }}>
          <div className="label">Your hand</div>
          <div className="hand" style={{ marginTop: 6 }}>
            {(me.handTiles ?? []).map((t, i) => (
              <Tile key={i} tile={t} />
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

function SeatBlock({ p, you, isTurn }: { p: PlayerView; you: boolean; isTurn: boolean }) {
  return (
    <div className={`seat ${you ? "you" : ""} ${isTurn ? "turn" : ""}`} style={{ flex: 1 }}>
      <div className="label">Seat {p.seat + 1}{you ? " · you" : ""}</div>
      <div style={{ fontSize: 16, fontWeight: 600 }}>{p.name}</div>
      <div className="event">{p.kind === "human" ? "Human" : p.kind}</div>

      <div className="row tight" style={{ marginTop: 8 }}>
        <span className="event">hand:</span>
        {!you ? (
          Array.from({ length: p.handCount }).map((_, i) => (
            <Tile key={i} tile="?" small faceDown />
          ))
        ) : (
          (p.handTiles ?? []).map((t, i) => <Tile key={i} tile={t} small />)
        )}
      </div>

      {p.fixedGroups.length > 0 && (
        <div className="row tight" style={{ marginTop: 6 }}>
          <span className="event">groups:</span>
          {p.fixedGroups.flatMap((g, gi) => g.tiles.map((t, ti) => (
            <Tile key={`${gi}-${ti}`} tile={t} small />
          )))}
        </div>
      )}
    </div>
  );
}
