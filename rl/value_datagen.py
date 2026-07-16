"""
value_datagen.py — Monte-Carlo outcome data for training the value head.

The IS-MCTS teacher bootstraps a leaf value V(our-infoset) instead of rolling
to game end. That value must approximate the expected final seat-0 balance
under the SAME setup the rollout tail uses: seat 0 plays the fast student net
(greedy), the three opponents play FirstFelix. So we generate exactly those
games and log, at each of our (seat-0) DISCARD decisions, the observation and
the eventual game balance (a Monte-Carlo return; single-hand episodes → the
terminal balance, no discounting).

Only discard-state observations are logged, because that is the state type at
which the tree expands a leaf (contextTile = None, 14-tile hand).

Usage
─────
    rl/venv/bin/python rl/value_datagen.py \
        --policy rl/checkpoints/student/student48.pt \
        --opponent firstfelix --n-games 3000 --n-workers 6 \
        --out rl/data/value_v1
"""

import argparse
import sys
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

_W = {}


def _worker_init(jar, policy_ckpt, opponent):
    import torch
    torch.set_num_threads(1)
    from env import MahjongEnv
    from model import MahjongAgent
    _W["agent"] = MahjongAgent.load(policy_ckpt, device="cpu")
    _W["env"]   = MahjongEnv(jar, opponent=opponent, obs_version=3)


def _play_game(seed):
    """Play one game; return (obs_list, balance) for our discard decisions."""
    env, agent = _W["env"], _W["agent"]
    try:
        obs, info = env.reset(seed=seed)
    except AssertionError:
        return [], 0.0
    discard_obs = []
    while True:
        decision = info["decision"]
        action = agent.select_action(obs, decision, info["action_mask"],
                                     deterministic=True)
        if decision == "discard":
            discard_obs.append(obs.astype(np.float32))
        obs, reward, done, info = env.step(action)
        if done:
            return discard_obs, float(reward)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--policy", required=True, help="seat-0 policy checkpoint (.pt)")
    p.add_argument("--opponent", default="firstfelix")
    p.add_argument("--n-games", type=int, default=3000)
    p.add_argument("--n-workers", type=int, default=6)
    p.add_argument("--seed-offset", type=int, default=40_000_000)
    p.add_argument("--out", required=True, help="output shard directory")
    args = p.parse_args()

    out = Path(args.out)
    out.mkdir(parents=True, exist_ok=True)

    seeds = list(range(args.seed_offset, args.seed_offset + args.n_games))
    all_obs, all_val = [], []
    n_done = 0
    with ProcessPoolExecutor(
        max_workers=args.n_workers, initializer=_worker_init,
        initargs=(args.jar, args.policy, args.opponent),
    ) as pool:
        for obs_list, balance in pool.map(_play_game, seeds, chunksize=8):
            n_done += 1
            for o in obs_list:
                all_obs.append(o)
                all_val.append(balance)
            if n_done % 500 == 0:
                v = np.asarray(all_val)
                print(f"[{n_done}/{args.n_games}] {len(all_val)} samples  "
                      f"mean_balance={v.mean():+.2f} std={v.std():.2f}", flush=True)

    obs_arr = np.asarray(all_obs, dtype=np.float32)
    val_arr = np.asarray(all_val, dtype=np.float32)
    np.savez_compressed(out / "value_data.npz", obs=obs_arr, value=val_arr)
    print(f"\nsaved {len(val_arr)} (obs, value) samples -> {out/'value_data.npz'}")
    print(f"value: mean={val_arr.mean():+.3f} std={val_arr.std():.3f} "
          f"min={val_arr.min():.1f} max={val_arr.max():.1f}")


if __name__ == "__main__":
    main()
