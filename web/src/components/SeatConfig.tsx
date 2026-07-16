"use client";

import { AI_KINDS, Room, SeatKind, seatLabel } from "@/lib/types";

interface Props {
  room: Room;
  isHost: boolean;
  busy: boolean;
  isFull: boolean;
  onChangeSeat: (seat: number, kind: SeatKind) => void;
  onStart: () => void;
}

export default function SeatConfig({
  room, isHost, busy, isFull, onChangeSeat, onStart,
}: Props) {
  return (
    <div className="card">
      <div className="row" style={{ justifyContent: "space-between" }}>
        <h2>Seats</h2>
        {isHost && (
          <button disabled={busy || !isFull} onClick={onStart}>
            {isFull ? "Start game" : `Waiting (${room.seats.filter(s => s.kind !== "open").length}/4)`}
          </button>
        )}
      </div>

      <div className="row" style={{ alignItems: "stretch", marginTop: 12 }}>
        {room.seats.map((s) => {
          const isHostSeat = s.index === 0;
          const choices: SeatKind[] = ["human", ...AI_KINDS, ...(isHostSeat ? [] : ["open"] as SeatKind[])];
          return (
            <div key={s.index} className="seat" style={{ flex: 1 }}>
              <div className="label">Seat {s.index + 1}{isHostSeat ? " (host)" : ""}</div>
              <div style={{ marginTop: 4, fontSize: 18, fontWeight: 600 }}>
                {s.name}
              </div>
              <div className="event" style={{ marginTop: 4 }}>{seatLabel[s.kind]}</div>

              {isHost && !isHostSeat && (
                <div className="row" style={{ marginTop: 10 }}>
                  <select
                    disabled={busy}
                    value={s.kind}
                    onChange={(e) => onChangeSeat(s.index, e.target.value as SeatKind)}
                  >
                    {choices.map(c => (
                      <option key={c} value={c}>{seatLabel[c]}</option>
                    ))}
                  </select>
                </div>
              )}
            </div>
          );
        })}
      </div>

      <p className="event" style={{ marginTop: 12 }}>
        {isHost
          ? "As the host, choose Human (someone has to join from the lobby) or any AI bot for each seat. The Start button activates when all four seats are filled."
          : "Waiting for the host to start the game."}
      </p>
    </div>
  );
}
