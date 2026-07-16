"use client";

import { GameSnapshot, Room } from "@/lib/types";
import Tile from "./Tile";

interface Props {
  snap:       GameSnapshot;
  room:       Room;
  yourSeat:   number | null;
  isHost:     boolean;
  readySeats: number[];
  myReady:    boolean;
  busy:       boolean;
  onReady:    () => void;
  onStartNext: () => void;
}

export default function GameOverModal({
  snap, room, yourSeat, isHost, readySeats, myReady, busy, onReady, onStartNext
}: Props) {
  const isWinner = yourSeat !== null && snap.winners.some(w => w.seat === yourSeat);
  const isDraw   = snap.winners.length === 0;
  const isPlayer = yourSeat !== null;

  let headline: string;
  if (isDraw)         headline = "Draw — No winners";
  else if (!isPlayer) headline = "Game over";
  else if (isWinner)  headline = "You won!";
  else                headline = "You lost.";

  return (
    <div className="modal-overlay">
      <div className="modal-card" style={{ maxWidth: 620 }}>
        <h2 style={{ fontSize: 32, marginBottom: 20 }}>{headline}</h2>

        {snap.winners.map(w => {
          const isYou  = w.seat === yourSeat;
          const name   = room.seats[w.seat]?.name ?? `Seat ${w.seat + 1}`;
          const player = snap.players.find(p => p.seat === w.seat);
          return (
            <div key={w.seat} style={{ marginBottom: 20 }}>
              <div style={{ fontSize: 18, fontWeight: 600, marginBottom: 8 }}>
                {isYou ? "You" : name} won with score&nbsp;{w.score}
                {!isYou && isPlayer && (
                  <span style={{ color: "var(--muted)", fontSize: 14, marginLeft: 8 }}>
                    — you lost
                  </span>
                )}
              </div>
              {player?.handTiles?.length ? (
                <div>
                  <div className="label" style={{ marginBottom: 6 }}>
                    {isYou ? "Your" : `${name}'s`} winning hand
                  </div>
                  <div className="hand" style={{ justifyContent: "center" }}>
                    {player.handTiles.map((t, i) => <Tile key={i} tile={t} />)}
                    {player.fixedGroups.flatMap((g, gi) =>
                      g.tiles.map((t, ti) => <Tile key={`g${gi}-${ti}`} tile={t} />)
                    )}
                  </div>
                </div>
              ) : null}
            </div>
          );
        })}

        {isDraw && (
          <p style={{ color: "var(--muted)", marginBottom: 16 }}>No one won this round.</p>
        )}

        <hr style={{ border: "none", borderTop: "1px solid rgba(255,255,255,.1)", margin: "16px 0" }} />

        <div className="label" style={{ marginBottom: 10 }}>Next game</div>
        <div className="row" style={{ justifyContent: "center", marginBottom: 10 }}>
          {isPlayer && !myReady && (
            <button disabled={busy} onClick={onReady}>Ready for next game</button>
          )}
          {isPlayer && myReady && (
            <span style={{ color: "var(--accent)", fontWeight: 600 }}>You are ready ✓</span>
          )}
          {isHost && (
            <button disabled={busy || readySeats.length < 4} onClick={onStartNext}>
              Start next game
            </button>
          )}
        </div>
        <div className="row tight" style={{ justifyContent: "center" }}>
          {room.seats.map((s, i) => (
            <span key={i} className="event" style={{
              color: readySeats.includes(i) ? "var(--accent)" : "var(--muted)"
            }}>
              {s.name}{readySeats.includes(i) ? " ✓" : " …"}
            </span>
          ))}
        </div>
      </div>
    </div>
  );
}
