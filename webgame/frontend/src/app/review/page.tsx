"use client";

// Post-game review: every finished game replayed through the champion, with
// the decisions it would have played differently listed first (issues #40/#41,
// ported into this app by #47).

import { Suspense, useCallback, useEffect, useRef, useState } from "react";
import Link from "next/link";
import { getName } from "@/lib/identity";
import {
  getPlayerStats,
  getReview,
  listGames,
  type GameListItem,
  type GameReview,
  type PlayerStats,
  type ReviewDecisionWire,
} from "@/lib/server";
import { tileFromWire } from "@/lib/wire";
import { Tile } from "@/components/Tile";

const KIND_LABEL: Record<string, string> = {
  discard: "打出 Discard",
  win: "食糊 Win?",
  self_win: "自摸 Self-draw?",
  kong: "槓 Kong?",
  self_kong: "暗槓 Self-kong?",
  pong: "碰 Pong?",
  chow: "上 Chow?",
};

const BUCKET_LABEL: Record<string, string> = {
  shape: "shape",
  tempo: "tempo",
  safety: "safety",
  accept: "claim",
  other: "close call",
};

function Money({ n }: { n: number | null | undefined }) {
  const v = n ?? 0;
  const cls = v > 0 ? "text-emerald-300" : v < 0 ? "text-red-400" : "text-emerald-100/50";
  return (
    <span className={`tabular-nums font-semibold ${cls}`}>
      {v > 0 ? `+$${v}` : v < 0 ? `-$${-v}` : "$0"}
    </span>
  );
}

/** Action keys are tile names for discards, words for claims. */
function ActionLabel({ kind, action }: { kind: string; action: string }) {
  if (action === "pass") return <em className="text-emerald-100/70">過 pass</em>;
  if (action === "accept") return <em className="text-amber-300">take it</em>;
  const tile = tileFromWire(action);
  if (tile !== null && (kind === "discard" || kind === "self_kong")) {
    return <Tile tile={tile} size="sm" />;
  }
  return <em className="text-amber-300">{action}</em>;
}

function StatTile({ label, value, sub }: { label: string; value: string; sub?: string }) {
  return (
    <div className="rounded-xl border border-white/10 bg-black/20 px-4 py-3">
      <div className="text-[11px] uppercase tracking-wide text-emerald-100/50">{label}</div>
      <div className="text-2xl font-bold tabular-nums text-emerald-50">{value}</div>
      {sub && <div className="text-[11px] text-emerald-100/50">{sub}</div>}
    </div>
  );
}

function StatsHeader({ stats }: { stats: PlayerStats }) {
  const pct = (x: number) => `${Math.round(x * 100)}%`;
  const series = [...stats.agreementByGame].reverse(); // oldest → newest
  return (
    <div className="mb-4 rounded-2xl border border-white/10 bg-[#0a3527]/70 p-4">
      <div className="mb-3 text-[11px] uppercase tracking-wide text-emerald-100/50">
        Your progress
      </div>
      <div className="flex flex-wrap gap-3">
        <StatTile label="games" value={String(stats.games)} />
        <StatTile
          label="money / game"
          value={`${stats.moneyPerGame >= 0 ? "+" : ""}${stats.moneyPerGame.toFixed(1)}`}
          sub={`total ${stats.totalMoney >= 0 ? "+" : ""}${stats.totalMoney}`}
        />
        <StatTile
          label="champion agreement"
          value={stats.agreementRate !== null ? pct(stats.agreementRate) : "—"}
          sub={
            stats.timedOutTurns > 0
              ? `last ${stats.reviewedGames} games · ${stats.timedOutTurns} auto-played excluded`
              : `last ${stats.reviewedGames} games`
          }
        />
        <StatTile label="win rate" value={pct(stats.winRate)} />
        <StatTile label="deal-in rate" value={pct(stats.dealInRate)} />
        {series.length > 1 && (
          <div className="rounded-xl border border-white/10 bg-black/20 px-4 py-3">
            <div className="text-[11px] uppercase tracking-wide text-emerald-100/50">
              agreement trend →
            </div>
            <div className="mt-2 flex h-10 items-end gap-1">
              {series.map((a) => (
                <span
                  key={a.gameId}
                  className="w-3 rounded-t bg-emerald-400"
                  style={{
                    height: `${Math.max(6, (a.agreementRate ?? 0) * 100)}%`,
                    opacity: a.agreementRate === null ? 0.3 : 1,
                  }}
                  title={
                    a.agreementRate === null
                      ? `${new Date(a.startedAt).toLocaleDateString()}: no decisions you made`
                      : `${new Date(a.startedAt).toLocaleDateString()}: ${Math.round(
                          a.agreementRate * 100,
                        )}% of ${a.decisions}`
                  }
                />
              ))}
            </div>
          </div>
        )}
      </div>
    </div>
  );
}

function GameCard({
  g,
  selected,
  onClick,
}: {
  g: GameListItem;
  selected: boolean;
  onClick: () => void;
}) {
  const result = g.outcome?.drawn
    ? "流局 drawn"
    : g.outcome?.winners.some((w) => w.seat === g.mySeat)
      ? g.outcome.isSelfWin
        ? "自摸 you self-won"
        : "食糊 you won"
      : g.outcome?.loserSeat === g.mySeat
        ? "you dealt in"
        : "no change";
  const opponents = g.seats
    .filter((s) => s.index !== g.mySeat)
    .map((s) => s.name)
    .join(", ");
  return (
    <button
      onClick={onClick}
      className={`mb-2 w-full rounded-xl border px-3 py-2 text-left transition ${
        selected
          ? "border-amber-400 bg-black/30"
          : "border-white/10 bg-black/20 hover:border-amber-400/40"
      }`}
    >
      <div className="flex items-center justify-between">
        <span className="text-xs text-emerald-100/60">
          {new Date(g.startedAt).toLocaleString()}
        </span>
        <Money n={g.myMoney} />
      </div>
      <div className="text-sm text-emerald-50">{result}</div>
      <div className="text-[11px] text-emerald-100/40">vs {opponents}</div>
    </button>
  );
}

function DecisionRow({ d }: { d: ReviewDecisionWire }) {
  const border = d.timedOut
    ? "border-l-emerald-100/25"
    : d.agree
      ? "border-l-emerald-400"
      : "border-l-red-400";
  return (
    <div
      className={`mb-2 rounded-xl border border-white/10 border-l-4 bg-black/20 p-3 ${border} ${
        d.timedOut ? "opacity-70" : ""
      }`}
    >
      <div className="flex items-center justify-between">
        <div className="flex items-center gap-2 text-sm font-semibold text-emerald-50">
          {KIND_LABEL[d.kind] ?? d.kind}
          {d.contextTile && tileFromWire(d.contextTile) !== null && (
            <Tile tile={tileFromWire(d.contextTile)!} size="sm" />
          )}
          {d.timedOut && (
            <span className="rounded-full bg-white/10 px-2 py-0.5 text-[10px] uppercase tracking-wide text-emerald-100/60">
              auto-played
            </span>
          )}
          {!d.timedOut && !d.agree && d.bucket && (
            <span className="rounded-full bg-emerald-400/20 px-2 py-0.5 text-[10px] uppercase tracking-wide text-emerald-300">
              {BUCKET_LABEL[d.bucket] ?? d.bucket}
            </span>
          )}
        </div>
        <span className="text-[11px] tabular-nums text-emerald-100/50">
          champion eval {d.value >= 0 ? "+" : ""}
          {d.value.toFixed(1)}
        </span>
      </div>

      <div className="mt-2 flex flex-wrap items-center gap-1">
        <span className="mr-1 text-[11px] text-emerald-100/40">hand:</span>
        {d.hand.map((t, i) => {
          const tile = tileFromWire(t);
          return tile === null ? null : <Tile key={i} tile={tile} size="sm" />;
        })}
      </div>

      <div className="mt-2 flex flex-wrap items-center gap-4 text-sm text-emerald-50">
        <span className="flex items-center gap-1">
          {d.timedOut ? "engine played:" : "you:"}{" "}
          <ActionLabel kind={d.kind} action={d.chosen} />
          <span className="tabular-nums text-[11px] text-emerald-100/50">
            ({Math.round(d.chosenProb * 100)}%)
          </span>
        </span>
        {!d.agree && (
          <span className="flex items-center gap-1">
            champion: <ActionLabel kind={d.kind} action={d.best} />
            <span className="tabular-nums text-[11px] text-emerald-100/50">
              ({Math.round(d.bestProb * 100)}%)
            </span>
          </span>
        )}
        {d.agree && !d.timedOut && (
          <span className="text-[11px] text-emerald-300">champion agrees</span>
        )}
      </div>

      {d.why && !d.timedOut && (
        <div className="mt-2 rounded-lg bg-black/25 px-3 py-2 text-[13px] leading-relaxed text-emerald-100/90">
          {d.why}
        </div>
      )}
    </div>
  );
}

function ReviewDetail({ review }: { review: GameReview }) {
  const s = review.summary;
  const played = review.decisions.filter((d) => !d.timedOut);
  const disagreements = played.filter((d) => !d.agree);
  const agreements = played.filter((d) => d.agree);
  const timedOut = review.decisions.filter((d) => d.timedOut);
  const [showAgreed, setShowAgreed] = useState(false);
  const [showAuto, setShowAuto] = useState(false);

  return (
    <div className="rounded-2xl border border-white/10 bg-[#0a3527]/70 p-4">
      <div className="flex items-center justify-between">
        <h2 className="text-lg font-semibold text-amber-300">
          {new Date(review.startedAt).toLocaleString()}
        </h2>
        <Money n={review.seatMoney} />
      </div>
      <p className="mt-1 text-[13px] leading-relaxed text-emerald-100/70">
        You agreed with the champion on{" "}
        <strong className="text-emerald-50">
          {s.agreements}/{s.decisions}
        </strong>{" "}
        decisions
        {s.agreementRate !== null && <> ({Math.round(s.agreementRate * 100)}%)</>}.{" "}
        {disagreements.length > 0
          ? "Biggest disagreements first — these are the moves to study."
          : s.decisions === 0
            ? "You did not make any decisions in this game."
            : "Perfect agreement — it would have played every move the same way."}
        {s.timedOut > 0 && (
          <>
            {" "}
            {s.timedOut} turn{s.timedOut === 1 ? " was" : "s were"} played by the engine after
            your prompt expired; {s.timedOut === 1 ? "it is" : "they are"} left out of this
            score.
          </>
        )}
      </p>

      <div className="mt-3">
        {disagreements.map((d) => (
          <DecisionRow key={`${d.seq}-${d.kind}`} d={d} />
        ))}
      </div>

      {agreements.length > 0 && (
        <>
          <button
            onClick={() => setShowAgreed((v) => !v)}
            className="mb-2 rounded-lg border border-white/15 px-3 py-1 text-xs text-emerald-100/80 hover:border-amber-400/50"
          >
            {showAgreed ? "Hide" : "Show"} {agreements.length} agreed decisions
          </button>
          {showAgreed && agreements.map((d) => <DecisionRow key={`${d.seq}-${d.kind}`} d={d} />)}
        </>
      )}

      {timedOut.length > 0 && (
        <>
          <button
            onClick={() => setShowAuto((v) => !v)}
            className="mb-2 ml-2 rounded-lg border border-white/15 px-3 py-1 text-xs text-emerald-100/80 hover:border-amber-400/50"
          >
            {showAuto ? "Hide" : "Show"} {timedOut.length} auto-played turn
            {timedOut.length === 1 ? "" : "s"}
          </button>
          {showAuto && timedOut.map((d) => <DecisionRow key={`${d.seq}-${d.kind}`} d={d} />)}
        </>
      )}
    </div>
  );
}

function ReviewPageInner() {
  const [name, setName] = useState("");
  const [games, setGames] = useState<GameListItem[] | null>(null);
  const [stats, setStats] = useState<PlayerStats | null>(null);
  const [selected, setSelected] = useState<string | null>(null);
  const [review, setReview] = useState<GameReview | null>(null);
  const [loading, setLoading] = useState(false);
  const [reviewLoading, setReviewLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const reviewReq = useRef(0);

  const load = useCallback(async (who: string) => {
    setLoading(true);
    setError(null);
    setGames(null);
    setStats(null);
    setSelected(null);
    setReview(null);
    try {
      const [gs, st] = await Promise.all([listGames(who, 50), getPlayerStats(who)]);
      setGames(gs);
      setStats(st);
    } catch (e) {
      setError(e instanceof Error ? e.message : "Could not load your games.");
    } finally {
      setLoading(false);
    }
  }, []);

  useEffect(() => {
    const n = getName();
    if (n) {
      setName(n);
      void load(n);
    }
  }, [load]);

  // A review replays a whole game through the champion, so it can take a
  // moment; without sequencing, a slow first response can land under a
  // different game's selection.
  const openReview = useCallback(async (g: GameListItem) => {
    if (g.mySeat === null) return;
    const req = ++reviewReq.current;
    setSelected(g.id);
    setReview(null);
    setReviewLoading(true);
    setError(null);
    try {
      const r = await getReview(g.id, g.mySeat);
      if (req === reviewReq.current) setReview(r);
    } catch (e) {
      if (req === reviewReq.current) {
        setError(e instanceof Error ? e.message : "Could not review that game.");
      }
    } finally {
      if (req === reviewReq.current) setReviewLoading(false);
    }
  }, []);

  return (
    <main className="mx-auto w-full max-w-6xl flex-1 px-4 py-6">
      <div className="mb-4 flex items-center justify-between">
        <h1 className="text-2xl font-bold text-amber-300">🎓 覆盤 Review</h1>
        <Link
          href="/"
          className="rounded-lg border border-white/15 px-3 py-1.5 text-sm text-emerald-100/80 hover:border-amber-400/50"
        >
          ← Lobby
        </Link>
      </div>
      <p className="mb-4 text-[13px] text-emerald-100/60">
        Every finished game is replayed through the champion. The decisions it would have
        played differently come first — that is your study list.
      </p>

      <div className="mb-4 flex flex-wrap gap-2">
        <input
          value={name}
          onChange={(e) => setName(e.target.value)}
          onKeyDown={(e) => e.key === "Enter" && name.trim() && void load(name.trim())}
          placeholder="your player name"
          className="rounded-lg border border-white/15 bg-black/30 px-3 py-2 text-sm text-emerald-50 outline-none placeholder:text-emerald-100/30 focus:border-amber-400/60"
        />
        <button
          disabled={!name.trim() || loading}
          onClick={() => void load(name.trim())}
          className="rounded-lg bg-amber-400 px-4 py-2 text-sm font-semibold text-emerald-950 disabled:opacity-40"
        >
          {loading ? "Loading…" : "Load my games"}
        </button>
      </div>

      {stats && <StatsHeader stats={stats} />}

      {games && (
        <div className="grid gap-4 md:grid-cols-[320px_1fr]">
          <div>
            <div className="mb-2 text-[11px] uppercase tracking-wide text-emerald-100/50">
              Your finished games ({games.length})
            </div>
            {games.length === 0 && (
              <div className="rounded-xl border border-white/10 bg-black/20 p-3 text-sm text-emerald-100/70">
                No recorded games for “{name}” yet — play a few (they record
                automatically), then come back.
              </div>
            )}
            {games.map((g) => (
              <GameCard
                key={g.id}
                g={g}
                selected={selected === g.id}
                onClick={() => void openReview(g)}
              />
            ))}
          </div>
          <div>
            {reviewLoading && (
              <div className="rounded-2xl border border-white/10 bg-black/20 p-4 text-sm text-emerald-100/70">
                Replaying the game through the champion…
              </div>
            )}
            {review && <ReviewDetail review={review} />}
            {!review && !reviewLoading && games.length > 0 && (
              <div className="rounded-2xl border border-white/10 bg-black/20 p-4 text-sm text-emerald-100/60">
                Pick a game on the left to see where the champion disagreed.
              </div>
            )}
          </div>
        </div>
      )}

      {error && (
        <div className="mt-4 rounded-xl border border-red-400/40 bg-red-950/60 p-3 text-sm text-red-100">
          {error}
        </div>
      )}
    </main>
  );
}

export default function ReviewPage() {
  return (
    <Suspense fallback={<main className="flex-1 p-6 text-emerald-100/70">Loading…</main>}>
      <ReviewPageInner />
    </Suspense>
  );
}
