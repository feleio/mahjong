"""
league_eval.py — Head-to-head evaluation between two checkpoints.

Plays 1-vs-3 tables in BOTH directions on the same seeds (paired):
  config X: seat0 = A, seats 1-3 = B
  config Y: seat0 = B, seats 1-3 = A
A is better than B if A's lone-seat money in X exceeds B's lone-seat money
in Y (same seeds, symmetric roles). All seats play deterministic argmax.

Rewards come from the self-play server path, which (post scorebonus fix)
returns true zero-sum payouts.

Usage
─────
    rl/venv/bin/python rl/league_eval.py \
        --a rl/checkpoints/ppo_a/ppo_final.pt \
        --b rl/checkpoints/exit_v3_r3_soft/exit_final.pt \
        --n-games 1000 --n-workers 8
"""

import argparse
import sys
import time
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

_W = {}


def _worker_init(jar, ckpt_a, ckpt_b):
    import torch
    torch.set_num_threads(1)
    from model import MahjongAgent
    from self_play import TrueSelfPlayEnv

    _W["agents"] = {"A": MahjongAgent.load(ckpt_a, device="cpu"),
                    "B": MahjongAgent.load(ckpt_b, device="cpu")}
    _W["env"] = TrueSelfPlayEnv(jar)


def _play(env, seat_assign, seed):
    """seat_assign: dict seat -> agent key. Returns per-seat rewards."""
    from env import encode_state, get_action_mask
    msg = env.reset(seed=seed)
    while True:
        if msg["type"] == "game_over":
            return {int(k): float(v) for k, v in msg["rewards"].items()}
        seat = int(msg["seat_id"])
        agent = _W["agents"][seat_assign[seat]]
        obs = encode_state(msg["state"], version=3, context=msg.get("context"))
        mask = get_action_mask(msg["decision"], msg.get("context", {}))
        action = agent.select_action(obs, msg["decision"], mask,
                                     deterministic=True)
        msg = env.step(msg["decision"], action)


def _play_seed(seed):
    env = _W["env"]
    ra = _play(env, {0: "A", 1: "B", 2: "B", 3: "B"}, seed)
    rb = _play(env, {0: "B", 1: "A", 2: "A", 3: "A"}, seed)
    return {"seed": seed, "a_lone": ra[0], "b_lone": rb[0]}


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--a", required=True)
    p.add_argument("--b", required=True)
    p.add_argument("--n-games", type=int, default=1000)
    p.add_argument("--n-workers", type=int, default=8)
    p.add_argument("--seed-offset", type=int, default=8_000_000)
    args = p.parse_args()

    seeds = list(range(args.seed_offset, args.seed_offset + args.n_games))
    results = []
    t0 = time.time()
    with ProcessPoolExecutor(max_workers=args.n_workers,
                             initializer=_worker_init,
                             initargs=(args.jar, args.a, args.b)) as pool:
        for res in pool.map(_play_seed, seeds, chunksize=4):
            results.append(res)
            if len(results) % 250 == 0:
                print(f"[{len(results)}/{args.n_games}] {time.time()-t0:.0f}s")
                sys.stdout.flush()

    a = np.array([r["a_lone"] for r in results])
    b = np.array([r["b_lone"] for r in results])
    d = a - b
    print(f"\nA = {args.a}\nB = {args.b}\n")
    print(f"  A lone vs 3B: {a.mean():+.3f}/game  win {np.mean(a > 0):.1%}  "
          f"draw {np.mean(a == 0):.1%}")
    print(f"  B lone vs 3A: {b.mean():+.3f}/game  win {np.mean(b > 0):.1%}  "
          f"draw {np.mean(b == 0):.1%}")
    print(f"  paired advantage (A−B): {d.mean():+.3f} ± "
          f"{d.std() / np.sqrt(len(d)):.3f}")


if __name__ == "__main__":
    main()
