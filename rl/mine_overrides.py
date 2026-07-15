"""
mine_overrides.py — human-readable analysis of WHERE the search teacher
overrides the raw net, from ExIt shards (exit_datagen2/3/4 format).

The ~14% of discard decisions where the paired search overrides the NN are
exactly the skill the net hasn't absorbed yet — and the paired ΔQ prices
each override in $/game. This tool decodes the v3/v4 obs back to
human-legible features and buckets the overrides:

  tempo    : override IMPROVES post-discard shanten (net missed a faster line)
  accept   : same shanten, override has materially higher ukeire
  safety   : override is shanten-WORSE — the search pays tempo, i.e. it is
             folding/reshaping for danger or value the net doesn't see
  shape    : same shanten, similar ukeire — subtle value/safety shaping

Per bucket: count, share, mean/median claimed gain. Plus tile-class shifts
(does the override discard honors/terminals where the net discarded middle
tiles?), game-phase split, and the N largest-gain concrete examples with
decoded hands.

NOTE: claimed gains are argmax-selected estimates → upward-biased
(winner's curse). Treat them as ordering, not calibrated $ values.

Usage:
    rl/venv/bin/python rl/mine_overrides.py --data-dir rl/data/exit_mix1 \
        --top-examples 12
"""

import argparse
import sys
from collections import defaultdict
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

# ── v3 obs offsets (see env.encode_state) ─────────────────────────────────────
HAND, SH_AFTER, UK_AFTER, UNSEEN = 0, 587, 621, 689
REMAINING = 578

SUIT = ["D", "B", "C"]
HONOR = ["E", "S", "W", "N", "Rd", "Gn", "Wh"]


def tname(t: int) -> str:
    return f"{t % 9 + 1}{SUIT[t // 9]}" if t < 27 else HONOR[t - 27]


def hand_str(counts: np.ndarray) -> str:
    return " ".join(tname(t) * 1 for t in range(34) for _ in range(int(counts[t])))


def tile_class(t: int) -> str:
    if t >= 27:
        return "honor"
    return "terminal" if t % 9 in (0, 8) else ("edge" if t % 9 in (1, 7) else "middle")


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir", default="rl/data/exit_mix1")
    p.add_argument("--top-examples", type=int, default=12)
    p.add_argument("--max-shards", type=int, default=0, help="0 = all")
    args = p.parse_args()

    shards = sorted(Path(args.data_dir).glob("shard_*.npz"))
    if args.max_shards:
        shards = shards[: args.max_shards]
    if not shards:
        raise SystemExit(f"no shards in {args.data_dir}")

    rows = []          # (gain, d_sh, d_uk, nn_t, ch_t, remaining, unseen_nn, hand, sh_nn, sh_ch, uk_nn, uk_ch)
    n_disc = n_sw = 0
    for path in shards:
        z = np.load(path)
        if "discard_obs" not in z.files:
            continue
        obs = z["discard_obs"]; act = z["discard_act"]
        cands = z["discard_cands"]; mean_d = z["discard_mean_d"]
        sw = z["discard_switched"]
        n_disc += len(act); n_sw += int(sw.sum())
        for i in np.where(sw)[0]:
            c, m = cands[i], mean_d[i]
            valid = c >= 0
            # NN choice: the candidate whose paired diff vs itself is exactly 0
            base = int(np.argmin(np.abs(np.where(valid, m, np.inf))))
            nn_t, ch_t = int(c[base]), int(act[i])
            ci = int(np.where(c == ch_t)[0][0])
            gain = float(m[ci])
            o = obs[i]
            sh = 9.0 - o[SH_AFTER:SH_AFTER + 34] * 10.0     # shanten after discarding t
            uk = o[UK_AFTER:UK_AFTER + 34] * 60.0
            rows.append((gain,
                         float(sh[ch_t] - sh[nn_t]),
                         float(uk[ch_t] - uk[nn_t]),
                         nn_t, ch_t,
                         float(o[REMAINING] * 136),
                         float(o[UNSEEN + nn_t] * 4),
                         (o[HAND:HAND + 34] * 4).round().astype(int),
                         float(sh[nn_t]), float(sh[ch_t]),
                         float(uk[nn_t]), float(uk[ch_t])))

    print(f"{len(shards)} shards | {n_disc:,} searched discard decisions | "
          f"{n_sw:,} overrides ({n_sw / max(1, n_disc):.1%})")
    if not rows:
        return

    def bucket(r):
        _, d_sh, d_uk = r[0], r[1], r[2]
        if d_sh < -0.25:
            return "tempo"
        if d_sh > 0.25:
            return "safety"
        return "accept" if d_uk > 2.0 else "shape"

    by = defaultdict(list)
    for r in rows:
        by[bucket(r)].append(r)

    print(f"\n{'bucket':8s} {'n':>6s} {'share':>6s} {'mean$':>7s} {'med$':>6s}  meaning")
    desc = {"tempo": "search found a strictly FASTER discard (net missed shanten)",
            "accept": "same speed, materially higher tile acceptance",
            "safety": "search PAYS shanten — folding/danger/value the net can't see",
            "shape": "same speed & similar acceptance — subtle shaping"}
    for b in ("tempo", "accept", "shape", "safety"):
        g = np.array([r[0] for r in by[b]]) if by[b] else np.array([0.0])
        print(f"{b:8s} {len(by[b]):>6,} {len(by[b])/len(rows):>6.1%} "
              f"{g.mean():>7.2f} {np.median(g):>6.2f}  {desc[b]}")

    # tile-class shift: what the net wanted to throw vs what the search threw
    shift = defaultdict(int)
    for r in rows:
        shift[(tile_class(r[3]), tile_class(r[4]))] += 1
    print("\ntile-class shifts (net wanted → search chose), top 8:")
    for (a, b), n in sorted(shift.items(), key=lambda kv: -kv[1])[:8]:
        print(f"  {a:8s} → {b:8s} {n:>6,} ({n/len(rows):.1%})")

    # phase split
    early = [r[0] for r in rows if r[5] > 55]
    late  = [r[0] for r in rows if r[5] <= 30]
    mid   = [r[0] for r in rows if 30 < r[5] <= 55]
    print(f"\nphase: early(rem>55) {len(early):,} overrides, mean ${np.mean(early or [0]):.2f}"
          f" | mid {len(mid):,}, ${np.mean(mid or [0]):.2f}"
          f" | late(rem<=30) {len(late):,}, ${np.mean(late or [0]):.2f}")

    # dead-tile discipline: how often was the net's keep/throw around unseen counts
    low_live = [r for r in rows if r[6] <= 1.0]
    print(f"\noverrides where the net's discard had ≤1 live copy left: "
          f"{len(low_live):,} ({len(low_live)/len(rows):.1%})")

    print(f"\n─ top {args.top_examples} largest-gain overrides "
          f"(claimed $, upward-biased) ─")
    for r in sorted(rows, key=lambda r: -r[0])[: args.top_examples]:
        gain, d_sh, d_uk, nn_t, ch_t, rem, _, hand, sh_nn, sh_ch, uk_nn, uk_ch = r
        print(f"  +${gain:5.1f} [{bucket(r):6s}] rem={rem:3.0f}  "
              f"hand: {hand_str(hand)}")
        print(f"          net: {tname(nn_t):3s} (sh {sh_nn:.0f}, uk {uk_nn:.0f})"
              f"  → search: {tname(ch_t):3s} (sh {sh_ch:.0f}, uk {uk_ch:.0f})")


if __name__ == "__main__":
    main()
