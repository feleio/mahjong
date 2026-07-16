"""
exit_datagen.py — Expert Iteration data generation (single worker).

Plays games vs a 3×FirstFelix table using PairedMCTSPolicy (imitation policy
+ paired-worlds MCTS override) and records every decision:

  discard   → label = MCTS-improved choice (+ `switched` flag)
  all other → label = imitation policy argmax (self-distillation anchor)

Shards are written as .npz files. Launch N copies with different --worker-id
for parallelism; seeds are strided so workers never overlap:

    for w in $(seq 0 9); do
      nohup rl/venv/bin/python rl/exit_datagen.py \
          --worker-id $w --n-workers 10 --n-games 10000 \
          --save-dir rl/data/exit_v1 > rl/data/exit_v1/worker$w.log 2>&1 &
    done

(--n-games is the TOTAL across workers; each worker plays ~n_games/n_workers.)
"""

import argparse
import sys
import time
from collections import defaultdict
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar",         default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--checkpoint",  default="rl/checkpoints/imitation_v2/imitation_final.pt")
    p.add_argument("--worker-id",   type=int, default=0)
    p.add_argument("--n-workers",   type=int, default=1)
    p.add_argument("--n-games",     type=int, default=10_000,
                   help="TOTAL games across all workers")
    p.add_argument("--seed-offset", type=int, default=1_000_000,
                   help="keep datagen seeds disjoint from eval seeds")
    p.add_argument("--n-worlds",    type=int,   default=32)
    p.add_argument("--top-k",       type=int,   default=6)
    p.add_argument("--z-threshold", type=float, default=1.5)
    p.add_argument("--min-gain",    type=float, default=0.25)
    p.add_argument("--games-per-shard", type=int, default=100)
    p.add_argument("--save-dir",    default="rl/data/exit_v1")
    args = p.parse_args()

    import torch
    torch.set_num_threads(1)
    from env import MahjongEnv, OBS_DIM_V3
    from model import MahjongAgent
    from mcts import MCTSRolloutClient, PairedMCTSPolicy

    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    agent  = MahjongAgent.load(args.checkpoint, device="cpu")
    obs_version = 3 if agent.net.obs_dim == OBS_DIM_V3 else 2
    client = MCTSRolloutClient(jar_path=args.jar, rollout_opp="firstfelix")
    policy = PairedMCTSPolicy(
        net=agent.net, rollout_client=client,
        n_worlds=args.n_worlds, top_k=args.top_k,
        z_threshold=args.z_threshold, min_gain=args.min_gain, device="cpu",
    )
    env = MahjongEnv(args.jar, opponent="firstfelix", obs_version=obs_version)

    my_seeds = [
        args.seed_offset + g
        for g in range(args.n_games)
        if g % args.n_workers == args.worker_id
    ]
    print(f"[worker {args.worker_id}] {len(my_seeds)} games, "
          f"n_worlds={args.n_worlds} top_k={args.top_k} "
          f"z={args.z_threshold} min_gain={args.min_gain}")
    sys.stdout.flush()

    buf = defaultdict(list)   # decision -> [(obs, act, mask, switched)]
    game_rewards = []
    shard_idx = 0
    t0 = time.time()

    K = args.top_k

    def flush():
        nonlocal shard_idx, buf
        if not any(buf.values()):
            return
        out = {}
        for dec, samples in buf.items():
            out[f"{dec}_obs"]  = np.array([s["obs"] for s in samples], dtype=np.float32)
            out[f"{dec}_act"]  = np.array([s["act"] for s in samples], dtype=np.int64)
            out[f"{dec}_mask"] = np.array([s["mask"] for s in samples], dtype=bool)
            out[f"{dec}_switched"] = np.array([s["switched"] for s in samples], dtype=bool)
            if dec == "discard":
                # Q-data padded to top_k: cand=-1 means "no candidate"
                out["discard_cands"]  = np.array([s["cands"]  for s in samples], dtype=np.int64)
                out["discard_mean_d"] = np.array([s["mean_d"] for s in samples], dtype=np.float32)
                out["discard_se_d"]   = np.array([s["se_d"]   for s in samples], dtype=np.float32)
        path = save_dir / f"shard_w{args.worker_id:02d}_{shard_idx:04d}.npz"
        np.savez_compressed(path, **out)
        buf = defaultdict(list)
        shard_idx += 1

    def pad(lst, fill, k=K):
        lst = list(lst)[:k]
        return lst + [fill] * (k - len(lst))

    for gi, seed in enumerate(my_seeds):
        obs, info = env.reset(seed=seed)
        while True:
            decision = info["decision"]
            mask     = info["action_mask"]
            action, ainfo = policy.get_action(
                obs, info["state"], decision, info["context"], mask)
            sample = {
                "obs":      obs.copy(),
                "act":      int(action),
                "mask":     np.asarray(mask, dtype=bool).copy(),
                "switched": bool(ainfo.get("switched", False)),
            }
            if decision == "discard":
                sample["cands"]  = pad(ainfo.get("candidates", [action]), -1)
                sample["mean_d"] = pad(ainfo.get("mean_diffs", [0.0]), 0.0)
                sample["se_d"]   = pad(ainfo.get("se_diffs", [0.0]), 1e9)
            buf[decision].append(sample)
            obs, r, done, info = env.step(action)
            if done:
                game_rewards.append(float(r))
                break

        if (gi + 1) % args.games_per_shard == 0:
            flush()
        if (gi + 1) % 25 == 0:
            elapsed = time.time() - t0
            rr = np.array(game_rewards)
            print(f"[worker {args.worker_id}] {gi+1}/{len(my_seeds)} games "
                  f"money={rr.mean():+.2f} win%={np.mean(rr > 0):.1%} "
                  f"switch={policy.n_switches}/{policy.n_discard_decisions} "
                  f"({policy.n_switches / max(policy.n_discard_decisions, 1):.1%}) "
                  f"| {elapsed / (gi + 1):.1f}s/game")
            sys.stdout.flush()

    flush()
    rr = np.array(game_rewards)
    print(f"[worker {args.worker_id}] DONE {len(my_seeds)} games "
          f"money={rr.mean():+.2f} win%={np.mean(rr > 0):.1%} "
          f"switches={policy.n_switches}/{policy.n_discard_decisions}")
    env.close()
    client.close()


if __name__ == "__main__":
    main()
