"use client";

import { useEffect, useState } from "react";

interface Props {
  /** When the prompt arrived (ms epoch), or null when it is not your turn. */
  startedAt: number | null;
  /** Budget from the server; the engine plays a default once it runs out. */
  timeoutMs?: number | null;
}

/** Countdown on your own decision, so running out of time is something you see
 * coming rather than something you discover afterwards (issue #42). */
export default function TurnTimer({ startedAt, timeoutMs }: Props) {
  const budget = timeoutMs ?? 60_000;
  const [now, setNow] = useState(() => Date.now());

  useEffect(() => {
    if (startedAt === null) return;
    setNow(Date.now());
    const t = setInterval(() => setNow(Date.now()), 500);
    return () => clearInterval(t);
  }, [startedAt]);

  if (startedAt === null) return null;

  const left = Math.max(0, budget - (now - startedAt));
  const secs = Math.ceil(left / 1000);
  const frac = Math.max(0, Math.min(1, left / budget));
  const urgent = left <= 15_000;

  return (
    <span className={`turn-timer ${urgent ? "urgent" : ""}`} title="Time left before the engine plays a default for you">
      {secs}s
      <span className="turn-timer-track">
        <span className="turn-timer-fill" style={{ width: `${frac * 100}%` }} />
      </span>
    </span>
  );
}
