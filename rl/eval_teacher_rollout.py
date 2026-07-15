"""
eval_teacher_rollout.py — rollout-opponent realism gate (opponent model bias).

The #24 depth gate showed search VARIANCE is not the ceiling (4× worlds ≈
+1/game, 0.92σ) — so the Q-estimates are limited by something systematic. The
prime suspect: rollouts model all three opponents as FirstFelix, so every Q is
"value of this move against FF" and the search optimizes exploiting a fixed
weak opponent. This gate swaps the rollout opponents for the NN student
(server-side NNPlayer, same OnnxPolicyService that self_policy="nn" already
uses) and asks whether the teacher gets stronger.

Both arms are the SAME net inside the same flat CRN search (64w/top6); they
differ ONLY in rollout_opp. Paired by seed vs 3×FirstFelix. Caveat, stated up
front: vs an FF table, arm A's opponent model is exactly matched to the real
opponents — a home-field advantage — so this is a CONSERVATIVE gate for arm B.

    rl/venv/bin/python rl/eval_teacher_rollout.py \
        --checkpoint rl/checkpoints/exit_sp1b_soft/exit_final.pt \
        --nn-model rl/checkpoints/student/student48_sp1b.onnx \
        --a-rollout-opp firstfelix --b-rollout-opp nn \
        --n-games 1000 --n-workers 3
"""

import argparse
import sys
import time
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

_W = {}


def _worker_init(jar, ckpt, nn_model, a_rollout_opp, b_rollout_opp,
                 n_worlds, top_k, z, min_gain, self_policy, threads):
    import torch
    torch.set_num_threads(1)
    from env import MahjongEnv, OBS_DIM_V3, OBS_DIM_V4
    from model import MahjongAgent
    from mcts import MCTSRolloutClient, PairedMCTSPolicy

    agent = MahjongAgent.load(ckpt, device="cpu")
    d = agent.net.obs_dim
    version = 4 if d == OBS_DIM_V4 else 3 if d == OBS_DIM_V3 else 2

    def build(rollout_opp):
        client = MCTSRolloutClient(
            jar_path=jar, rollout_opp=rollout_opp,
            nn_model=nn_model, rollout_threads=threads)
        return PairedMCTSPolicy(
            net=agent.net, rollout_client=client, n_worlds=n_worlds,
            top_k=top_k, z_threshold=z, min_gain=min_gain, device="cpu",
            self_policy=self_policy)

    _W["A"] = build(a_rollout_opp)
    _W["B"] = build(b_rollout_opp)
    _W["ver"] = version
    _W["env"] = MahjongEnv(jar, opponent="firstfelix", obs_version=4)


def _play_arm(arm, seed):
    from env import encode_state
    env = _W["env"]
    policy, version = _W[arm], _W["ver"]
    n_dec = n_sw = 0
    try:
        _, info = env.reset(seed=seed)
    except AssertionError:
        return 0.0, 0, 0
    while True:
        obs = encode_state(info["state"], version=version, context=info["context"])
        action, ainfo = policy.get_action(obs, info["state"], info["decision"],
                                          info["context"], info["action_mask"])
        if "mean_gain" in ainfo:
            n_dec += 1
            n_sw += bool(ainfo["switched"])
        _, r, done, info = env.step(action)
        if done:
            return float(r), n_sw, n_dec


def _play_seed(seed):
    a, a_sw, a_dec = _play_arm("A", seed)
    b, b_sw, b_dec = _play_arm("B", seed)
    return {"A": a, "B": b, "A_sw": a_sw, "A_dec": a_dec, "B_sw": b_sw, "B_dec": b_dec}


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--checkpoint", required=True, help="net used by BOTH arms")
    p.add_argument("--nn-model", required=True, help="rollout student ONNX")
    p.add_argument("--self-policy", default="nn", choices=["nn", "firstfelix"])
    p.add_argument("--a-rollout-opp", default="firstfelix",
                   choices=["chicken", "firstfelix", "nn"])
    p.add_argument("--b-rollout-opp", default="nn",
                   choices=["chicken", "firstfelix", "nn"])
    p.add_argument("--n-worlds", type=int, default=64)
    p.add_argument("--top-k", type=int, default=6)
    p.add_argument("--n-games", type=int, default=1000)
    p.add_argument("--z-threshold", type=float, default=1.5)
    p.add_argument("--min-gain", type=float, default=0.25)
    p.add_argument("--n-workers", type=int, default=3)
    p.add_argument("--rollout-threads", type=int, default=4)
    p.add_argument("--seed-offset", type=int, default=40_000_000)
    args = p.parse_args()

    seeds = list(range(args.seed_offset, args.seed_offset + args.n_games))
    results = []
    t0 = time.time()
    with ProcessPoolExecutor(
        max_workers=args.n_workers, initializer=_worker_init,
        initargs=(args.jar, args.checkpoint, args.nn_model,
                  args.a_rollout_opp, args.b_rollout_opp,
                  args.n_worlds, args.top_k, args.z_threshold, args.min_gain,
                  args.self_policy, args.rollout_threads),
    ) as pool:
        for res in pool.map(_play_seed, seeds, chunksize=2):
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
    a_sw = sum(r["A_sw"] for r in results); a_dec = sum(r["A_dec"] for r in results)
    b_sw = sum(r["B_sw"] for r in results); b_dec = sum(r["B_dec"] for r in results)
    print(f"\narm A [rollout_opp={args.a_rollout_opp}]: {a.mean():+.3f}/game  "
          f"win {np.mean(a>0):.1%}  switches {a_sw}/{a_dec} ({a_sw/max(1,a_dec):.1%})")
    print(f"arm B [rollout_opp={args.b_rollout_opp}]: {b.mean():+.3f}/game  "
          f"win {np.mean(b>0):.1%}  switches {b_sw}/{b_dec} ({b_sw/max(1,b_dec):.1%})")
    print(f"paired rollout-opp delta (B−A): {d.mean():+.3f} ± "
          f"{d.std()/np.sqrt(len(d)):.3f}  (n={len(d)})")


if __name__ == "__main__":
    main()
