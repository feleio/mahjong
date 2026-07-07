"""
exit_datagen2.py — Expert Iteration datagen with SELF-PLAY opponents.

Differences from exit_datagen.py (which targets 3×FirstFelix):
  * the table is self-play: seats 1-3 play the raw net (argmax),
    seat 0 plays net + paired-worlds search (the expert)
  * rollout worlds can also model everyone with the net
    (--rollout-opp nn / --self-rollout nn, via the ONNX student model)
  * shard format is identical, so exit_distill_soft.py works unchanged

Launch N workers with disjoint seeds, same as before:

    for w in $(seq 0 9); do
      nohup rl/venv/bin/python rl/exit_datagen2.py \
          --worker-id $w --n-workers 10 --n-games 20000 \
          --checkpoint <best.pt> --nn-model <student.onnx> \
          --save-dir rl/data/exit_sp1 > rl/data/exit_sp1/worker$w.log 2>&1 &
    done
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
    p.add_argument("--checkpoint",  required=True)
    p.add_argument("--nn-model",    default=None,
                   help="ONNX student for NN rollout policies")
    p.add_argument("--self-rollout", default="nn",
                   choices=["nn", "firstfelix", "chicken"],
                   help="our seat's policy inside rollout worlds")
    p.add_argument("--rollout-opp", default="nn",
                   choices=["nn", "firstfelix", "chicken"],
                   help="opponents' policy inside rollout worlds")
    p.add_argument("--rollout-threads", type=int, default=2)
    p.add_argument("--worker-id",   type=int, default=0)
    p.add_argument("--n-workers",   type=int, default=1)
    p.add_argument("--n-games",     type=int, default=20_000,
                   help="TOTAL games across all workers")
    p.add_argument("--seed-offset", type=int, default=12_000_000)
    p.add_argument("--n-worlds",    type=int,   default=64)
    p.add_argument("--top-k",       type=int,   default=6)
    p.add_argument("--z-threshold", type=float, default=1.5)
    p.add_argument("--min-gain",    type=float, default=0.25)
    p.add_argument("--games-per-shard", type=int, default=100)
    p.add_argument("--save-dir",    required=True)
    args = p.parse_args()

    import torch
    torch.set_num_threads(1)
    from env import encode_state, get_action_mask
    from model import MahjongAgent
    from mcts import MCTSRolloutClient, PairedMCTSPolicy
    from self_play import TrueSelfPlayEnv

    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    agent = MahjongAgent.load(args.checkpoint, device="cpu")
    assert agent.net.obs_dim == 759, "self-play datagen expects a v3 net"
    client = MCTSRolloutClient(
        jar_path=args.jar, rollout_opp=args.rollout_opp,
        nn_model=args.nn_model, rollout_threads=args.rollout_threads)
    policy = PairedMCTSPolicy(
        net=agent.net, rollout_client=client,
        n_worlds=args.n_worlds, top_k=args.top_k,
        z_threshold=args.z_threshold, min_gain=args.min_gain, device="cpu",
        self_policy=args.self_rollout)
    env = TrueSelfPlayEnv(args.jar)

    my_seeds = [
        args.seed_offset + g
        for g in range(args.n_games)
        if g % args.n_workers == args.worker_id
    ]
    print(f"[worker {args.worker_id}] {len(my_seeds)} self-play games, "
          f"n_worlds={args.n_worlds} rollouts self={args.self_rollout} "
          f"opp={args.rollout_opp}")
    sys.stdout.flush()

    buf = defaultdict(list)
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
                out["discard_cands"]  = np.array([s["cands"]  for s in samples], dtype=np.int64)
                out["discard_mean_d"] = np.array([s["mean_d"] for s in samples], dtype=np.float32)
                out["discard_se_d"]   = np.array([s["se_d"]   for s in samples], dtype=np.float32)
        np.savez_compressed(
            save_dir / f"shard_w{args.worker_id:02d}_{shard_idx:04d}.npz", **out)
        buf = defaultdict(list)
        shard_idx += 1

    def pad(lst, fill, k=K):
        lst = list(lst)[:k]
        return lst + [fill] * (k - len(lst))

    for gi, seed in enumerate(my_seeds):
        msg = env.reset(seed=seed)
        while True:
            if msg["type"] == "game_over":
                game_rewards.append(float(msg["rewards"]["0"]))
                break
            seat = int(msg["seat_id"])
            decision = msg["decision"]
            context = msg.get("context", {})
            obs = encode_state(msg["state"], version=3, context=context)
            mask = get_action_mask(decision, context)

            if seat == 0:
                action, ainfo = policy.get_action(
                    obs, msg["state"], decision, context, mask)
                sample = {
                    "obs": obs, "act": int(action),
                    "mask": np.asarray(mask, dtype=bool),
                    "switched": bool(ainfo.get("switched", False)),
                }
                if decision == "discard":
                    sample["cands"]  = pad(ainfo.get("candidates", [action]), -1)
                    sample["mean_d"] = pad(ainfo.get("mean_diffs", [0.0]), 0.0)
                    sample["se_d"]   = pad(ainfo.get("se_diffs", [0.0]), 1e9)
                buf[decision].append(sample)
            else:
                action = agent.select_action(obs, decision, mask,
                                              deterministic=True)
            msg = env.step(decision, int(action))

        if (gi + 1) % args.games_per_shard == 0:
            flush()
        if (gi + 1) % 25 == 0:
            elapsed = time.time() - t0
            rr = np.array(game_rewards)
            print(f"[worker {args.worker_id}] {gi+1}/{len(my_seeds)} games "
                  f"seat0_money={rr.mean():+.2f} win%={np.mean(rr > 0):.1%} "
                  f"switch={policy.n_switches}/{policy.n_discard_decisions} "
                  f"({policy.n_switches / max(policy.n_discard_decisions, 1):.1%}) "
                  f"| {elapsed / (gi + 1):.1f}s/game")
            sys.stdout.flush()

    flush()
    rr = np.array(game_rewards)
    print(f"[worker {args.worker_id}] DONE {len(my_seeds)} games "
          f"seat0_money={rr.mean():+.2f} win%={np.mean(rr > 0):.1%}")
    env.close()
    client.close()


if __name__ == "__main__":
    main()
