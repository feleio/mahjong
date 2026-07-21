"""
eval_teacher_mixed.py — paired teacher A/B on MIXED (NN-opponent) tables.

The FF-anchored gates (#24 depth, #25 rollout, #22 danger) all measured levers
against 3xFirstFelix, where FF-rollouts are oracle-correct and defense is
invisible. This harness runs the SAME paired-CRN teacher on the production
self-play mixed table (seat 0 = champion + search; seats 1-3 = NN pool nets),
so opponent-model realism can actually pay off.

Two arms, SAME net, differ only in (rollout_opp, nn_model). Paired by seed:
identical deal + identical pool opponents for both arms; trajectories diverge
only where seat 0's search picks differently. Reports paired seat-0 money delta.

Rollout-realism test (does modelling opponents as NN beat modelling them as FF
when they ARE NN?):
    rl/venv/bin/python rl/eval_teacher_mixed.py \
        --checkpoint rl/checkpoints/best_raw_net.pt \
        --a-rollout-opp firstfelix --b-rollout-opp nn \
        --a-nn-model rl/checkpoints/student/student48_sp1b.onnx \
        --b-nn-model rl/checkpoints/student/student48_sp1b.onnx \
        --pool rl/checkpoints/exit_sp1b_soft/exit_final.pt ... best_raw_net.pt \
        --n-games 400 --n-workers 4

Self-tail test (does a stronger self-continuation policy in rollouts help?):
    ... --a-rollout-opp firstfelix --b-rollout-opp firstfelix \
        --a-nn-model .../student48_sp1b.onnx --b-nn-model .../best_raw_net.onnx
"""

import argparse
import sys
import time
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

_W = {}


def _worker_init(jar, ckpt, a_ropp, b_ropp, a_nn, b_nn, a_belief, b_belief,
                 pool_paths, n_worlds, top_k, z, min_gain, threads):
    import torch
    torch.set_num_threads(1)
    from env import encode_state, get_action_mask  # noqa: F401
    from model import MahjongAgent
    from mcts import MCTSRolloutClient, PairedMCTSPolicy
    from self_play import TrueSelfPlayEnv

    agent = MahjongAgent.load(ckpt, device="cpu")
    assert agent.net.obs_dim == 759, "expects a v3 net"
    pool = [MahjongAgent.load(c, device="cpu") for c in pool_paths]

    def build(ropp, nn, belief):
        client = MCTSRolloutClient(jar_path=jar, rollout_opp=ropp, nn_model=nn,
                                   belief_model=belief, rollout_threads=threads)
        return PairedMCTSPolicy(
            net=agent.net, rollout_client=client, n_worlds=n_worlds,
            top_k=top_k, z_threshold=z, min_gain=min_gain, device="cpu",
            self_policy="nn")

    _W.update(A=build(a_ropp, a_nn, a_belief), B=build(b_ropp, b_nn, b_belief),
              pool=pool, sp=TrueSelfPlayEnv(jar))


def _play_arm(arm, seed):
    from env import encode_state, get_action_mask
    sp, policy, pool = _W["sp"], _W[arm], _W["pool"]
    msg = sp.reset(seed=seed)
    while True:
        if msg["type"] == "game_over":
            return float(msg["rewards"]["0"])
        seat = int(msg["seat_id"])
        decision, context = msg["decision"], msg.get("context", {})
        obs = encode_state(msg["state"], version=3, context=context)
        mask = get_action_mask(decision, context)
        if seat == 0:
            action, _ = policy.get_action(obs, msg["state"], decision, context, mask)
        else:
            net = pool[(seed * 7 + seat * 101) % len(pool)]
            action = net.select_action(obs, decision, mask, deterministic=True)
        msg = sp.step(decision, int(action))


def _play_seed(seed):
    # same seed => identical deal + identical pool opponents for both arms
    return {"A": _play_arm("A", seed), "B": _play_arm("B", seed)}


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--checkpoint", required=True, help="net used by BOTH arms")
    p.add_argument("--pool", nargs="+", required=True, help="NN opponent seats")
    p.add_argument("--a-rollout-opp", default="firstfelix",
                   choices=["chicken", "firstfelix", "nn"])
    p.add_argument("--b-rollout-opp", default="nn",
                   choices=["chicken", "firstfelix", "nn"])
    p.add_argument("--a-nn-model", required=True)
    p.add_argument("--b-nn-model", required=True)
    p.add_argument("--a-belief", default=None, help="belief ONNX for arm A (None=uniform)")
    p.add_argument("--b-belief", default=None, help="belief ONNX for arm B (None=uniform)")
    p.add_argument("--n-worlds", type=int, default=64)
    p.add_argument("--top-k", type=int, default=6)
    p.add_argument("--n-games", type=int, default=400)
    p.add_argument("--z-threshold", type=float, default=1.5)
    p.add_argument("--min-gain", type=float, default=0.25)
    p.add_argument("--n-workers", type=int, default=4)
    p.add_argument("--rollout-threads", type=int, default=3)
    p.add_argument("--seed-offset", type=int, default=50_000_000)
    args = p.parse_args()

    seeds = list(range(args.seed_offset, args.seed_offset + args.n_games))
    results = []
    t0 = time.time()
    def _bel(x):
        return Path(x).stem if x else "uniform"
    print(f"A=[ropp={args.a_rollout_opp}, nn={Path(args.a_nn_model).stem}, "
          f"belief={_bel(args.a_belief)}]  "
          f"B=[ropp={args.b_rollout_opp}, nn={Path(args.b_nn_model).stem}, "
          f"belief={_bel(args.b_belief)}]  "
          f"mixed tables, pool={len(args.pool)} nets, n={args.n_games}")
    sys.stdout.flush()
    with ProcessPoolExecutor(
        max_workers=args.n_workers, initializer=_worker_init,
        initargs=(args.jar, args.checkpoint, args.a_rollout_opp,
                  args.b_rollout_opp, args.a_nn_model, args.b_nn_model,
                  args.a_belief, args.b_belief,
                  tuple(args.pool), args.n_worlds, args.top_k,
                  args.z_threshold, args.min_gain, args.rollout_threads),
    ) as pool_exec:
        for res in pool_exec.map(_play_seed, seeds, chunksize=2):
            results.append(res)
            n = len(results)
            if n % 25 == 0:
                a = np.array([r["A"] for r in results])
                b = np.array([r["B"] for r in results])
                d = b - a
                print(f"[{n}/{args.n_games}] {time.time()-t0:.0f}s  "
                      f"A={a.mean():+.2f} B={b.mean():+.2f} "
                      f"delta={d.mean():+.2f}±{d.std()/np.sqrt(n):.2f}")
                sys.stdout.flush()

    a = np.array([r["A"] for r in results]); b = np.array([r["B"] for r in results])
    d = b - a
    print(f"\narm A: {a.mean():+.3f}/game  win {np.mean(a>0):.1%}")
    print(f"arm B: {b.mean():+.3f}/game  win {np.mean(b>0):.1%}")
    print(f"paired delta (B-A): {d.mean():+.3f} ± {d.std()/np.sqrt(len(d)):.3f}  "
          f"(n={len(d)}, {d.mean()/(d.std()/np.sqrt(len(d))):+.2f} sigma)")


if __name__ == "__main__":
    main()
