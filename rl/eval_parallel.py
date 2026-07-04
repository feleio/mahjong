"""
eval_parallel.py — Fast parallel evaluation of checkpoints vs a FirstFelix table.

Plays each checkpoint deterministically (no MCTS) on the same seed set so
results are directly comparable (paired by seed).

Usage
─────
    rl/venv/bin/python rl/eval_parallel.py \
        --checkpoints rl/checkpoints/imitation_v2/imitation_final.pt \
                      rl/checkpoints/exit_v1/exit_final.pt \
        --n-games 2000 --n-workers 10
"""

import argparse
import sys
import time
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

_W = {}


def _worker_init(jar, checkpoints, opponent):
    import torch
    torch.set_num_threads(1)
    from env import MahjongEnv, OBS_DIM_V3
    from model import MahjongAgent

    _W["agents"] = {c: MahjongAgent.load(c, device="cpu") for c in checkpoints}
    # obs version per checkpoint, inferred from its input dim
    _W["versions"] = {c: (3 if a.net.obs_dim == OBS_DIM_V3 else 2)
                      for c, a in _W["agents"].items()}
    # env always runs at v3 (superset); per-agent obs re-encoded from raw state
    _W["env"] = MahjongEnv(jar, opponent=opponent, obs_version=3)


def _play_seed(seed):
    from env import encode_state
    env = _W["env"]
    out = {"seed": seed}
    for ckpt, agent in _W["agents"].items():
        version = _W["versions"][ckpt]
        _, info = env.reset(seed=seed)
        while True:
            obs = encode_state(info["state"], version=version,
                               context=info["context"])
            action = agent.select_action(obs, info["decision"], info["action_mask"],
                                         deterministic=True)
            _, r, done, info = env.step(action)
            if done:
                out[ckpt] = float(r)
                break
    return out


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar",        default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--checkpoints", nargs="+", required=True)
    p.add_argument("--opponent",   default="firstfelix")
    p.add_argument("--n-games",    type=int, default=2000)
    p.add_argument("--n-workers",  type=int, default=10)
    p.add_argument("--seed-offset", type=int, default=0)
    args = p.parse_args()

    seeds = list(range(args.seed_offset, args.seed_offset + args.n_games))
    results = []
    t0 = time.time()

    with ProcessPoolExecutor(
        max_workers=args.n_workers,
        initializer=_worker_init,
        initargs=(args.jar, args.checkpoints, args.opponent),
    ) as pool:
        for res in pool.map(_play_seed, seeds, chunksize=4):
            results.append(res)
            n = len(results)
            if n % 250 == 0:
                print(f"[{n}/{args.n_games}] {time.time() - t0:.0f}s")
                sys.stdout.flush()

    print(f"\nOpponent: 3x {args.opponent}  |  {len(results)} games each\n")
    rews = {c: np.array([r[c] for r in results]) for c in args.checkpoints}
    for ckpt, rr in rews.items():
        print(f"  {ckpt}")
        print(f"    money {rr.mean():+.3f}/game  win {np.mean(rr > 0):.1%}  "
              f"draw {np.mean(rr == 0):.1%}  "
              f"avg win pay {rr[rr > 0].mean() if (rr > 0).any() else 0:.1f}")
    if len(args.checkpoints) == 2:
        a, b = args.checkpoints
        d = rews[b] - rews[a]
        print(f"\n  paired delta (B−A): {d.mean():+.3f} ± "
              f"{d.std() / np.sqrt(len(d)):.3f}")


if __name__ == "__main__":
    main()
