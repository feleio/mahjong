"""
elo_ladder.py — trustworthy strength ranking over a pool of net checkpoints.

Head-to-head "money" duels are matchup-dependent (intransitive) in a 4-player
imperfect-info game, so single pairwise numbers have misled promotion twice.
This fits ONE consistent 1-D rating from the full paired round-robin.

Method
──────
For every unordered pair (X, Y) of nets, on the SAME seeds, play both
  config A: seat0 = X, seats1-3 = Y   -> money_X = X's reward as lone-vs-3Y
  config B: seat0 = Y, seats1-3 = X   -> money_Y = Y's reward as lone-vs-3X
The paired advantage a_XY = mean(money_X - money_Y) is antisymmetric and, in a
transitive world, equals r_X - r_Y. We fit ratings r (in $/game units, mean 0)
by weighted least squares on all a_XY (a weighted graph-Laplacian solve):
minimise  sum_{i<j} w_ij ( a_ij - (r_i - r_j) )^2 ,  w_ij = 1/SE_ij^2.

The weighted RMS residual of a_ij - (r_i - r_j) is the INTRANSITIVITY: how much
the pairwise results deviate from any single ranking. Bootstrap over seeds gives
per-rating CIs.

Ratings are on a money scale: r_X - r_Y ~ expected $/game X makes exploiting a
table of Y minus the reverse. FF/Chicken anchors are reported as a separate
calibration (each net's $/game as lone-vs-3anchor) — folding heuristics in as
full players needs a heuristic-seat-0 env (follow-up).

Usage
─────
    rl/venv/bin/python rl/elo_ladder.py \
        --players r3=rl/checkpoints/exit_v3_r3_soft/exit_final.pt \
                  sp1b=rl/checkpoints/exit_sp1b_soft/exit_final.pt \
                  D=rl/checkpoints/exit_mix_soft/exit_final.pt \
                  imit=rl/checkpoints/imitation_v3d/imitation_epoch10.pt \
        --n-games 1000 --n-workers 3 --out rl/checkpoints/elo.md
"""

import argparse
import itertools
import sys
import time
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

_W = {}


def _worker_init(jar, players):
    import torch
    torch.set_num_threads(1)
    from model import MahjongAgent
    from self_play import TrueSelfPlayEnv
    _W["agents"] = {name: MahjongAgent.load(path, device="cpu")
                    for name, path in players}
    _W["env"] = TrueSelfPlayEnv(jar)


def _lone_money(seat_assign, seed):
    """Play one game with the given seat->player assignment; return seat-0 money."""
    from env import encode_state, get_action_mask
    env = _W["env"]
    msg = env.reset(seed=seed)
    while True:
        if msg["type"] == "game_over":
            return float(msg["rewards"]["0"])
        seat = int(msg["seat_id"])
        agent = _W["agents"][seat_assign[seat]]
        obs = encode_state(msg["state"], version=3, context=msg.get("context"))
        mask = get_action_mask(msg["decision"], msg.get("context", {}))
        action = agent.select_action(obs, msg["decision"], mask, deterministic=True)
        msg = env.step(msg["decision"], int(action))


def _play_pair(task):
    """task = (X, Y, seed) -> (X, Y, seed, moneyX_lone_vs_3Y, moneyY_lone_vs_3X)"""
    X, Y, seed = task
    mX = _lone_money({0: X, 1: Y, 2: Y, 3: Y}, seed)
    mY = _lone_money({0: Y, 1: X, 2: X, 3: X}, seed)
    return (X, Y, seed, mX, mY)


def fit_ratings(names, adv, weight):
    """Weighted least-squares ratings (mean 0) from antisymmetric advantage adv[i][j]."""
    n = len(names)
    L = np.zeros((n, n))
    b = np.zeros(n)
    for i in range(n):
        for j in range(n):
            if i == j:
                continue
            w = weight[i][j]
            L[i, i] += w
            L[i, j] -= w
            b[i] += w * adv[i][j]
    # enforce mean-zero: solve (L + 1/n * J) r = b  (regularises the null space)
    L += np.ones((n, n)) / n
    r = np.linalg.solve(L, b)
    r -= r.mean()
    return r


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--players", nargs="+", required=True,
                   help="name=path entries")
    p.add_argument("--n-games", type=int, default=1000, help="paired games per pair")
    p.add_argument("--n-workers", type=int, default=3)
    p.add_argument("--seed-offset", type=int, default=50_000_000)
    p.add_argument("--boot", type=int, default=500, help="bootstrap resamples")
    p.add_argument("--out", default="rl/checkpoints/elo.md")
    args = p.parse_args()

    players = [tuple(kv.split("=", 1)) for kv in args.players]
    names = [n for n, _ in players]
    idx = {n: i for i, n in enumerate(names)}
    pairs = list(itertools.combinations(names, 2))
    seeds = list(range(args.seed_offset, args.seed_offset + args.n_games))

    tasks = [(X, Y, s) for (X, Y) in pairs for s in seeds]
    print(f"Ladder over {len(names)} nets, {len(pairs)} pairs x {args.n_games} "
          f"paired games = {len(tasks)*2} self-play games")
    sys.stdout.flush()

    # collect per-seed advantage samples per pair
    diffs = {pair: [] for pair in pairs}
    t0 = time.time()
    done = 0
    with ProcessPoolExecutor(max_workers=args.n_workers,
                             initializer=_worker_init,
                             initargs=(args.jar, players)) as pool:
        for (X, Y, seed, mX, mY) in pool.map(_play_pair, tasks, chunksize=8):
            diffs[(X, Y)].append(mX - mY)
            done += 1
            if done % 2000 == 0:
                print(f"  [{done}/{len(tasks)}] {time.time()-t0:.0f}s")
                sys.stdout.flush()

    n = len(names)
    adv = np.zeros((n, n)); se = np.ones((n, n)); weight = np.zeros((n, n))
    samples = {}
    for (X, Y), d in diffs.items():
        d = np.array(d); i, j = idx[X], idx[Y]
        a = d.mean(); s = d.std(ddof=1) / np.sqrt(len(d)) + 1e-9
        adv[i][j] = a;  adv[j][i] = -a
        se[i][j] = s;   se[j][i] = s
        weight[i][j] = weight[j][i] = 1.0 / s**2
        samples[(X, Y)] = d

    r = fit_ratings(names, adv, weight)

    # bootstrap CIs + intransitivity
    boot = np.zeros((args.boot, n))
    for b in range(args.boot):
        adv_b = np.zeros((n, n))
        for (X, Y), d in samples.items():
            i, j = idx[X], idx[Y]
            bs = np.random.choice(d, size=len(d), replace=True).mean()
            adv_b[i][j] = bs; adv_b[j][i] = -bs
        boot[b] = fit_ratings(names, adv_b, weight)
    lo = np.percentile(boot, 2.5, axis=0)
    hi = np.percentile(boot, 97.5, axis=0)

    # intransitivity: weighted RMS residual of adv - (r_i - r_j)
    num = den = 0.0
    for i in range(n):
        for j in range(i + 1, n):
            resid = adv[i][j] - (r[i] - r[j])
            num += weight[i][j] * resid**2
            den += weight[i][j]
    intransitivity = np.sqrt(num / den)

    order = np.argsort(-r)
    lines = []
    lines.append("# Strength ladder (money-scale rating, $/game, mean 0)\n")
    lines.append(f"Paired round-robin, {args.n_games} games/pair. "
                 f"Rating diff ~ expected $/game one net exploits a table of the "
                 f"other, minus the reverse.\n")
    lines.append(f"**Intransitivity (weighted RMS residual): "
                 f"${intransitivity:.2f}/game** — how far pairwise results "
                 f"deviate from a single ranking (0 = perfectly transitive).\n")
    lines.append("| rank | net | rating | 95% CI |")
    lines.append("|------|-----|--------|--------|")
    for rank, i in enumerate(order, 1):
        lines.append(f"| {rank} | {names[i]} | {r[i]:+.2f} | "
                     f"[{lo[i]:+.2f}, {hi[i]:+.2f}] |")
    lines.append("\n## Pairwise advantage matrix (row = lone, col = table of; "
                 "$/game, row−col direction)\n")
    lines.append("| |" + "|".join(names) + "|")
    lines.append("|" + "---|" * (n + 1))
    for i in range(n):
        row = [f"**{names[i]}**"]
        for j in range(n):
            row.append("—" if i == j else f"{adv[i][j]:+.2f}±{se[i][j]:.2f}")
        lines.append("|" + "|".join(row) + "|")
    out = "\n".join(lines) + "\n"

    print("\n" + out)
    Path(args.out).write_text(out)
    print(f"written -> {args.out}")


if __name__ == "__main__":
    main()
