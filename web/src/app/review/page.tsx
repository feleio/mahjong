"use client";

import { useEffect, useState } from "react";
import { useRouter } from "next/navigation";
import { getPlayerStats, getReview, listGames } from "@/lib/api";
import { loadName, saveName } from "@/lib/storage";
import {
  GameListItem,
  GameReview,
  PlayerStats,
  ReviewDecision,
} from "@/lib/types";
import Tile from "@/components/Tile";

/** Post-game review: where the champion disagreed with you (issues #40/#41). */
export default function ReviewPage() {
  const router = useRouter();
  const [name, setName]       = useState("");
  const [games, setGames]     = useState<GameListItem[] | null>(null);
  const [stats, setStats]     = useState<PlayerStats | null>(null);
  const [selected, setSelected] = useState<string | null>(null);
  const [review, setReview]   = useState<GameReview | null>(null);
  const [loading, setLoading] = useState(false);
  const [reviewLoading, setReviewLoading] = useState(false);
  const [error, setError]     = useState<string | null>(null);

  useEffect(() => {
    const n = loadName();
    if (n) { setName(n); load(n); }
  }, []);

  async function load(n: string) {
    setLoading(true); setError(null);
    setGames(null); setStats(null); setSelected(null); setReview(null);
    try {
      saveName(n);
      const [gs, st] = await Promise.all([listGames(n, 50), getPlayerStats(n)]);
      setGames(gs);
      setStats(st);
    } catch (e: any) {
      setError(String(e.message ?? e));
    } finally {
      setLoading(false);
    }
  }

  async function openReview(g: GameListItem) {
    if (g.mySeat === null) return;
    setSelected(g.id); setReview(null); setReviewLoading(true); setError(null);
    try {
      setReview(await getReview(g.id, g.mySeat));
    } catch (e: any) {
      setError(String(e.message ?? e));
    } finally {
      setReviewLoading(false);
    }
  }

  return (
    <div className="container">
      <div className="row" style={{ justifyContent: "space-between", marginBottom: 12 }}>
        <h1>🎓 Game review</h1>
        <button className="ghost" onClick={() => router.push("/")}>← Lobby</button>
      </div>
      <p className="event">
        Every finished game is replayed through the champion. Decisions where it
        would have played differently are listed first — that's your study list.
      </p>

      <div className="card">
        <div className="row">
          <input
            placeholder="your player name"
            value={name}
            onChange={(e) => setName(e.target.value)}
            onKeyDown={(e) => e.key === "Enter" && name.trim() && load(name.trim())}
          />
          <button disabled={!name.trim() || loading} onClick={() => load(name.trim())}>
            {loading ? "Loading…" : "Load my games"}
          </button>
        </div>
      </div>

      {stats && <StatsHeader stats={stats} />}

      {games && (
        <div className="review-grid">
          <div>
            <div className="label" style={{ marginBottom: 8 }}>
              Your finished games ({games.length})
            </div>
            {games.length === 0 && (
              <div className="card">
                No recorded games for “{name}” yet — play some games first (they
                record automatically), then come back.
              </div>
            )}
            {games.map((g) => (
              <GameItem key={g.id} g={g} selected={selected === g.id} onClick={() => openReview(g)} />
            ))}
          </div>
          <div>
            {reviewLoading && <div className="card">Replaying the game through the champion…</div>}
            {review && <ReviewDetail review={review} />}
            {!review && !reviewLoading && games.length > 0 && (
              <div className="card event">Pick a game on the left to see where the champion disagreed.</div>
            )}
          </div>
        </div>
      )}

      {error && <div className="card" style={{ color: "var(--danger)" }}>{error}</div>}
    </div>
  );
}

function money(n: number | null | undefined): JSX.Element {
  const v = n ?? 0;
  const cls = v > 0 ? "pos" : v < 0 ? "neg" : "zero";
  return <span className={`money ${cls}`}>{v > 0 ? `+$${v}` : v < 0 ? `-$${-v}` : "$0"}</span>;
}

function StatsHeader({ stats }: { stats: PlayerStats }) {
  const pct = (x: number) => `${Math.round(x * 100)}%`;
  // agreementByGame is newest first; show oldest → newest for the trend
  const series = [...stats.agreementByGame].reverse();
  return (
    <div className="card">
      <div className="label" style={{ marginBottom: 8 }}>Your progress</div>
      <div className="stat-tiles">
        <div className="stat-tile"><div className="label">games</div><div className="big">{stats.games}</div></div>
        <div className="stat-tile"><div className="label">money / game</div>
          <div className="big">{money(Math.round(stats.moneyPerGame * 100) / 100)}</div>
          <div className="event">total {money(stats.totalMoney)}</div>
        </div>
        <div className="stat-tile"><div className="label">champion agreement</div>
          <div className="big">{stats.agreementRate !== null ? pct(stats.agreementRate) : "—"}</div>
          <div className="event">
            last {stats.reviewedGames} games
            {stats.timedOutTurns > 0 && <> · {stats.timedOutTurns} auto-played turn{stats.timedOutTurns === 1 ? "" : "s"} excluded</>}
          </div>
        </div>
        <div className="stat-tile"><div className="label">win rate</div><div className="big">{pct(stats.winRate)}</div></div>
        <div className="stat-tile"><div className="label">deal-in rate</div><div className="big">{pct(stats.dealInRate)}</div></div>
        {series.length > 1 && (
          <div className="stat-tile">
            <div className="label">agreement trend →</div>
            <div className="agree-spark" title="champion-agreement per game, oldest to newest">
              {series.map((a) => (
                <span key={a.gameId} className="bar" style={{ height: `${Math.max(5, a.agreementRate * 100)}%` }}
                  title={`${new Date(a.startedAt).toLocaleDateString()}: ${Math.round(a.agreementRate * 100)}% of ${a.decisions}`} />
              ))}
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

function GameItem({ g, selected, onClick }: { g: GameListItem; selected: boolean; onClick: () => void }) {
  const when = new Date(g.startedAt).toLocaleString();
  const opponents = g.seats.filter((s) => s.index !== g.mySeat).map((s) => s.name).join(", ");
  const result = g.outcome?.drawn
    ? "drawn"
    : g.outcome?.winners.some((w) => w.seat === g.mySeat)
      ? (g.outcome.isSelfWin ? "you self-won" : "you won")
      : g.outcome?.loserSeat === g.mySeat
        ? "you dealt in"
        : "no change";
  return (
    <div className={`game-item ${selected ? "selected" : ""}`} onClick={onClick}>
      <div className="row" style={{ justifyContent: "space-between" }}>
        <span className="event">{when}</span>
        {money(g.myMoney)}
      </div>
      <div style={{ fontSize: 13, marginTop: 2 }}>{result}</div>
      <div className="event">vs {opponents}</div>
    </div>
  );
}

function ReviewDetail({ review }: { review: GameReview }) {
  const s = review.summary;
  const played = review.decisions.filter((d) => !d.timedOut);
  const disagreements = played.filter((d) => !d.agree);
  const agreements = played.filter((d) => d.agree);
  const timedOut = review.decisions.filter((d) => d.timedOut);
  const [showAgreements, setShowAgreements] = useState(false);
  const [showTimedOut, setShowTimedOut] = useState(false);
  return (
    <div className="card">
      <div className="row" style={{ justifyContent: "space-between" }}>
        <h3 style={{ margin: 0 }}>Review · {new Date(review.startedAt).toLocaleString()}</h3>
        {money(review.seatMoney)}
      </div>
      <p className="event" style={{ marginTop: 6 }}>
        You agreed with the champion on <strong>{s.agreements}/{s.decisions}</strong> decisions
        ({Math.round(s.agreementRate * 100)}%). {disagreements.length > 0
          ? "Biggest disagreements first — these are the moves to study."
          : "Perfect agreement — the champion would have played every move the same way."}
        {s.timedOut > 0 && (
          <> {s.timedOut} turn{s.timedOut === 1 ? " was" : "s were"} played by the engine after
            your prompt expired; {s.timedOut === 1 ? "it is" : "they are"} left out of this score.</>
        )}
      </p>

      {disagreements.map((d) => <DecisionRow key={d.seq} d={d} />)}

      {agreements.length > 0 && (
        <>
          <button className="ghost" style={{ marginTop: 8 }} onClick={() => setShowAgreements(!showAgreements)}>
            {showAgreements ? "Hide" : "Show"} {agreements.length} agreed decisions
          </button>
          {showAgreements && agreements.map((d) => <DecisionRow key={d.seq} d={d} />)}
        </>
      )}

      {timedOut.length > 0 && (
        <>
          <button className="ghost" style={{ marginTop: 8 }} onClick={() => setShowTimedOut(!showTimedOut)}>
            {showTimedOut ? "Hide" : "Show"} {timedOut.length} auto-played turn{timedOut.length === 1 ? "" : "s"}
          </button>
          {showTimedOut && timedOut.map((d) => <DecisionRow key={d.seq} d={d} />)}
        </>
      )}
    </div>
  );
}

const KIND_LABEL: Record<string, string> = {
  discard: "Discard", win: "Win?", self_win: "Self-win?", kong: "Kong?",
  self_kong: "Self-kong?", pong: "Pong?", chow: "Chow?",
};

function actionLabel(kind: string, key: string): JSX.Element {
  if (key === "pass") return <em>pass</em>;
  if (key === "accept") return <em>take it</em>;
  if (kind === "discard" || kind === "self_kong") return <Tile tile={key} small />;
  return <em>{key}</em>; // chow position
}

const BUCKET_LABEL: Record<string, string> = {
  shape: "shape", tempo: "tempo", safety: "safety", accept: "claim", other: "close call",
};

function DecisionRow({ d }: { d: ReviewDecision }) {
  const cls = d.timedOut ? "timedout" : d.agree ? "" : "disagree";
  return (
    <div className={`decision ${cls}`} style={{ marginTop: 8 }}>
      <div className="row" style={{ justifyContent: "space-between" }}>
        <strong>
          {KIND_LABEL[d.kind] ?? d.kind}{d.contextTile && <> on <Tile tile={d.contextTile} small /></>}
          {d.timedOut && <span className="autoplay-tag">auto-played</span>}
          {!d.timedOut && d.bucket && !d.agree && (
            <span className="bucket-tag">{BUCKET_LABEL[d.bucket] ?? d.bucket}</span>
          )}
        </strong>
        <span className="event">champion eval {d.value >= 0 ? "+" : ""}{d.value.toFixed(1)}</span>
      </div>
      <div className="row tight" style={{ marginTop: 6 }}>
        <span className="event">hand:</span>
        {d.hand.map((t, i) => <Tile key={i} tile={t} small />)}
      </div>
      <div className="row" style={{ marginTop: 6, gap: 16 }}>
        <span>
          {d.timedOut ? "engine played" : "you"}: {actionLabel(d.kind, d.chosen)}{" "}
          <span className="event">({Math.round(d.chosenProb * 100)}%)</span>
        </span>
        {!d.agree && (
          <span>champion: {actionLabel(d.kind, d.best)} <span className="event">({Math.round(d.bestProb * 100)}%)</span></span>
        )}
        {d.agree && !d.timedOut && <span className="event">champion agrees</span>}
      </div>
      {d.why && !d.timedOut && <div className="why">{d.why}</div>}
    </div>
  );
}
