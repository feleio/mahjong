"""
exit_datagen3.py — Expert Iteration datagen with a POPULATION of opponents.

Round 2 (exit_datagen2) plateaued from self-play specialization: seats 1-3
were all copies of the champion, so the distilled net learned to beat *that
one partner* rather than getting stronger in general. This variant fixes the
exact failure: seat 0 stays champion + NN-guided search (the teacher), but the
opponents each game are drawn from a diverse pool so the improvement operator
stays honest against a population.

Per game, one of three table types is chosen (weighted, deterministic by seed):
  mixed   : self-play env, seats 1-3 each independently sampled from a pool of
            net checkpoints (champion, r3, r2, imitation base)
  ff      : seat 0 vs 3 FirstFelix
  chicken : seat 0 vs 3 Chicken

Only seat 0's (search-improved) decisions are recorded. Shard format is
identical to exit_datagen2, so exit_distill_soft.py consumes it unchanged.

Launch N workers with disjoint strided seeds:

    for w in $(seq 0 5); do
      nohup rl/venv/bin/python rl/exit_datagen3.py --worker-id $w --n-workers 6 \
        --n-games 12000 --champion rl/checkpoints/exit_sp1b_soft/exit_final.pt \
        --nn-model rl/checkpoints/student/student48_sp1b.onnx \
        --save-dir rl/data/exit_mix1 > rl/data/exit_mix1/worker$w.log 2>&1 &
    done
"""

import argparse
import sys
import time
from collections import defaultdict
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

DEFAULT_POOL = [
    "rl/checkpoints/exit_sp1b_soft/exit_final.pt",
    "rl/checkpoints/exit_v3_r3_soft/exit_final.pt",
    "rl/checkpoints/exit_v3_r2_soft/exit_final.pt",
    "rl/checkpoints/imitation_v3d/imitation_epoch10.pt",
]
# table-type mix (weights need not sum to 1; normalised below)
MIX = [("mixed", 0.55), ("ff", 0.30), ("chicken", 0.15)]


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--champion", required=True,
                   help="seat-0 net (also the distillation warm-start base)")
    p.add_argument("--pool", nargs="+", default=DEFAULT_POOL,
                   help="net checkpoints for opponent seats in 'mixed' tables")
    p.add_argument("--nn-model", required=True,
                   help="ONNX student for NN rollout policy")
    p.add_argument("--rollout-threads", type=int, default=3)
    p.add_argument("--worker-id", type=int, default=0)
    p.add_argument("--n-workers", type=int, default=1)
    p.add_argument("--n-games", type=int, default=12_000,
                   help="TOTAL games across all workers")
    p.add_argument("--seed-offset", type=int, default=30_000_000)
    p.add_argument("--n-worlds", type=int, default=64)
    p.add_argument("--top-k", type=int, default=6)
    p.add_argument("--z-threshold", type=float, default=1.5)
    p.add_argument("--min-gain", type=float, default=0.25)
    p.add_argument("--games-per-shard", type=int, default=100)
    p.add_argument("--save-dir", required=True)
    args = p.parse_args()

    import torch
    torch.set_num_threads(1)
    from env import MahjongEnv, encode_state, get_action_mask
    from model import MahjongAgent
    from mcts import MCTSRolloutClient, PairedMCTSPolicy
    from self_play import TrueSelfPlayEnv

    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    champ = MahjongAgent.load(args.champion, device="cpu")
    assert champ.net.obs_dim == 759, "expects a v3 net"
    pool = [MahjongAgent.load(c, device="cpu") for c in args.pool]

    client = MCTSRolloutClient(
        jar_path=args.jar, rollout_opp="firstfelix",
        nn_model=args.nn_model, rollout_threads=args.rollout_threads)
    policy = PairedMCTSPolicy(
        net=champ.net, rollout_client=client,
        n_worlds=args.n_worlds, top_k=args.top_k,
        z_threshold=args.z_threshold, min_gain=args.min_gain,
        device="cpu", self_policy="nn")

    sp_env = TrueSelfPlayEnv(args.jar)
    ff_env = MahjongEnv(args.jar, opponent="firstfelix", obs_version=3)
    ch_env = MahjongEnv(args.jar, opponent="chicken", obs_version=3)

    # deterministic table-type + opponent assignment by seed (stride-safe)
    types, weights = zip(*MIX)
    cum = np.cumsum(np.array(weights) / sum(weights))

    def table_for(seed):
        r = ((seed * 2654435761) % 10_000) / 10_000.0
        return types[int(np.searchsorted(cum, r))]

    def pool_seat(seed, seat):
        return pool[(seed * 7 + seat * 101) % len(pool)]

    my_seeds = [args.seed_offset + g for g in range(args.n_games)
                if g % args.n_workers == args.worker_id]
    print(f"[worker {args.worker_id}] {len(my_seeds)} games | pool={len(pool)} "
          f"nets | mix={dict(MIX)} | rollout self=nn opp=ff")
    sys.stdout.flush()

    buf = defaultdict(list)
    game_rewards = []
    type_counts = defaultdict(int)
    shard_idx = 0
    t0 = time.time()
    K = args.top_k

    def flush():
        nonlocal shard_idx, buf
        if not any(buf.values()):
            return
        out = {}
        for dec, samples in buf.items():
            out[f"{dec}_obs"] = np.array([s["obs"] for s in samples], dtype=np.float32)
            out[f"{dec}_act"] = np.array([s["act"] for s in samples], dtype=np.int64)
            out[f"{dec}_mask"] = np.array([s["mask"] for s in samples], dtype=bool)
            out[f"{dec}_switched"] = np.array([s["switched"] for s in samples], dtype=bool)
            if dec == "discard":
                out["discard_cands"] = np.array([s["cands"] for s in samples], dtype=np.int64)
                out["discard_mean_d"] = np.array([s["mean_d"] for s in samples], dtype=np.float32)
                out["discard_se_d"] = np.array([s["se_d"] for s in samples], dtype=np.float32)
                out["discard_balance"] = np.array([s["balance"] for s in samples], dtype=np.float32)
        np.savez_compressed(
            save_dir / f"shard_w{args.worker_id:02d}_{shard_idx:04d}.npz", **out)
        buf = defaultdict(list)
        shard_idx += 1

    def pad(lst, fill, k=K):
        lst = list(lst)[:k]
        return lst + [fill] * (k - len(lst))

    def record_seat0(decision, obs, mask, state, context):
        action, ainfo = policy.get_action(obs, state, decision, context, mask)
        sample = {"obs": obs, "act": int(action),
                  "mask": np.asarray(mask, dtype=bool),
                  "switched": bool(ainfo.get("switched", False))}
        if decision == "discard":
            sample["cands"] = pad(ainfo.get("candidates", [action]), -1)
            sample["mean_d"] = pad(ainfo.get("mean_diffs", [0.0]), 0.0)
            sample["se_d"] = pad(ainfo.get("se_diffs", [0.0]), 1e9)
            sample["balance"] = 0.0  # value target — filled with final game reward below
        buf[decision].append(sample)
        return int(action)

    def play_mixed(seed):
        msg = sp_env.reset(seed=seed)
        while True:
            if msg["type"] == "game_over":
                return float(msg["rewards"]["0"])
            seat = int(msg["seat_id"])
            decision, context = msg["decision"], msg.get("context", {})
            obs = encode_state(msg["state"], version=3, context=context)
            mask = get_action_mask(decision, context)
            if seat == 0:
                action = record_seat0(decision, obs, mask, msg["state"], context)
            else:
                action = pool_seat(seed, seat).select_action(
                    obs, decision, mask, deterministic=True)
            msg = sp_env.step(decision, int(action))

    def play_heuristic(env, seed):
        try:
            obs, info = env.reset(seed=seed)
        except AssertionError:
            return None
        while True:
            action = record_seat0(info["decision"], obs,
                                  info["action_mask"], info["state"],
                                  info["context"])
            obs, r, done, info = env.step(action)
            if done:
                return float(r)

    for gi, seed in enumerate(my_seeds):
        ttype = table_for(seed)
        type_counts[ttype] += 1
        d0 = len(buf["discard"])  # value-corpus: mark where this game's discard states start
        if ttype == "mixed":
            r = play_mixed(seed)
        elif ttype == "ff":
            r = play_heuristic(ff_env, seed)
        else:
            r = play_heuristic(ch_env, seed)
        if r is not None:
            game_rewards.append(r)
            for s in buf["discard"][d0:]:  # tag every discard state with the final game balance
                s["balance"] = float(r)
        else:
            del buf["discard"][d0:]  # drop a failed game's untagged discard states

        if (gi + 1) % args.games_per_shard == 0:
            flush()
        if (gi + 1) % 25 == 0:
            rr = np.array(game_rewards)
            print(f"[worker {args.worker_id}] {gi+1}/{len(my_seeds)} "
                  f"seat0_money={rr.mean():+.2f} win%={np.mean(rr > 0):.1%} "
                  f"switch={policy.n_switches}/{policy.n_discard_decisions} "
                  f"({policy.n_switches / max(policy.n_discard_decisions,1):.1%}) "
                  f"mix={dict(type_counts)} "
                  f"| {(time.time()-t0)/(gi+1):.1f}s/game")
            sys.stdout.flush()

    flush()
    rr = np.array(game_rewards)
    print(f"[worker {args.worker_id}] DONE {len(my_seeds)} games "
          f"seat0_money={rr.mean():+.2f} win%={np.mean(rr > 0):.1%} "
          f"mix={dict(type_counts)}")
    sp_env.close(); ff_env.close(); ch_env.close(); client.close()


if __name__ == "__main__":
    main()
