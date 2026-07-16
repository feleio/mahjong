"""
eval_mcts_ab.py — A/B gate between two SEARCH configurations.

For each seed, plays two games vs a 3×FirstFelix table with the SAME base
net + PairedMCTSPolicy, differing only in the rollout configuration:

  arm A: --a-self / --a-opp   (e.g. firstfelix / firstfelix — current champion)
  arm B: --b-self / --b-opp   (e.g. nn / firstfelix — student-guided rollouts)

Paired seeds give a low-variance delta. This is the gate for choosing the
rollout policy before an expensive ExIt datagen run.

Usage
─────
    rl/venv/bin/python rl/eval_mcts_ab.py \
        --checkpoint rl/checkpoints/exit_v3_r3_soft/exit_final.pt \
        --nn-model rl/checkpoints/student/student48.onnx \
        --a-self firstfelix --b-self nn \
        --n-games 400 --n-workers 5 --n-worlds 64 --rollout-threads 4
"""

import argparse
import sys
import time
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

_W = {}


def _worker_init(jar, checkpoint, nn_model, n_worlds, top_k, z, min_gain,
                 a_self, a_opp, b_self, b_opp, threads, a_belief, b_belief,
                 a_search, b_search, sims, c_puct, max_depth, search_tail, n_parallel,
                 value_model, leaf_plies, b_z, b_min_gain):
    import torch
    torch.set_num_threads(1)
    from env import MahjongEnv
    from model import MahjongAgent
    from mcts import MCTSRolloutClient, PairedMCTSPolicy, SearchPolicy

    agent = MahjongAgent.load(checkpoint, device="cpu")

    def make_policy(self_pol, opp_pol, belief, search, z_arm, gain_arm):
        # IS-MCTS and flatvalue always need the net loaded server-side; flat
        # only when a seat uses the "nn" rollout policy.
        needs_nn = search in ("ismcts", "flatvalue") or "nn" in (self_pol, opp_pol)
        client = MCTSRolloutClient(
            jar_path=jar, rollout_opp=opp_pol,
            nn_model=nn_model if needs_nn else None,
            belief_model=belief,
            value_model=value_model if search in ("ismcts", "flatvalue") else None,
            rollout_threads=threads)
        if search == "ismcts":
            return SearchPolicy(
                net=agent.net, rollout_client=client,
                sims=sims, top_k=top_k, c_puct=c_puct, max_depth=max_depth,
                rollout_tail=search_tail, n_parallel=n_parallel,
                rollout_opp=opp_pol, deterministic=True, device="cpu")
        return PairedMCTSPolicy(
            net=agent.net, rollout_client=client,
            n_worlds=n_worlds, top_k=top_k, z_threshold=z_arm,
            min_gain=gain_arm, device="cpu", self_policy=self_pol,
            value_leaf_plies=leaf_plies if search == "flatvalue" else 0)

    _W["policies"] = {"A": make_policy(a_self, a_opp, a_belief, a_search, z, min_gain),
                      "B": make_policy(b_self, b_opp, b_belief, b_search,
                                       b_z if b_z is not None else z,
                                       b_min_gain if b_min_gain is not None else min_gain)}
    _W["env"] = MahjongEnv(jar, opponent="firstfelix", obs_version=3)


def _play_arm(arm, seed):
    env, policy = _W["env"], _W["policies"][arm]
    n_dec = n_sw = 0
    try:
        obs, info = env.reset(seed=seed)
    except AssertionError:
        return 0.0, 0, 0  # game over before our first decision
    while True:
        action, ainfo = policy.get_action(obs, info["state"], info["decision"],
                                          info["context"], info["action_mask"])
        # Count searched discard decisions and overrides of the NN choice —
        # the bias-through-the-gate diagnosis (#20) compares switch rates.
        if "mean_gain" in ainfo:
            n_dec += 1
            n_sw += bool(ainfo["switched"])
        obs, r, done, info = env.step(action)
        if done:
            return float(r), n_sw, n_dec


def _play_seed(seed):
    a, a_sw, a_dec = _play_arm("A", seed)
    b, b_sw, b_dec = _play_arm("B", seed)
    return {"seed": seed, "A": a, "B": b,
            "A_sw": a_sw, "A_dec": a_dec, "B_sw": b_sw, "B_dec": b_dec}


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--checkpoint", required=True)
    p.add_argument("--nn-model", default=None)
    p.add_argument("--a-self", default="firstfelix")
    p.add_argument("--a-opp", default="firstfelix")
    p.add_argument("--b-self", default="nn")
    p.add_argument("--b-opp", default="firstfelix")
    p.add_argument("--a-belief", default=None,
                   help="belief ONNX for arm A's determinization (default: uniform)")
    p.add_argument("--b-belief", default=None,
                   help="belief ONNX for arm B's determinization (default: uniform)")
    p.add_argument("--a-search", default="flat", choices=["flat", "ismcts", "flatvalue"],
                   help="arm A search type (flat PIMC, IS-MCTS tree, or CRN flat+value-leaf)")
    p.add_argument("--b-search", default="flat", choices=["flat", "ismcts", "flatvalue"],
                   help="arm B search type; 'flatvalue' is the issue-#20 hybrid gate")
    p.add_argument("--sims", type=int, default=256, help="IS-MCTS simulations/decision")
    p.add_argument("--c-puct", type=float, default=1.5)
    p.add_argument("--max-depth", type=int, default=64)
    p.add_argument("--search-tail", default="inf", choices=["inf", "zero"])
    p.add_argument("--n-parallel", type=int, default=3, help="concurrent IS-MCTS sims")
    p.add_argument("--value-model", default=None,
                   help="outcome-trained value ONNX for IS-MCTS leaf bootstrap (search-tail=zero)"
                        " and the flatvalue hybrid")
    p.add_argument("--leaf-plies", type=int, default=1,
                   help="flatvalue: bootstrap the value net at our Nth discard decision"
                        " after the root discard (>=1; 0-ply would ignore the world)")
    p.add_argument("--n-games", type=int, default=400)
    p.add_argument("--n-workers", type=int, default=5)
    p.add_argument("--n-worlds", type=int, default=64)
    p.add_argument("--top-k", type=int, default=6)
    p.add_argument("--z-threshold", type=float, default=1.5)
    p.add_argument("--min-gain", type=float, default=0.25)
    p.add_argument("--b-z-threshold", type=float, default=None,
                   help="override z-threshold for arm B only (gate recalibration when"
                        " B's Q-estimator has a different noise/bias profile, see #20)")
    p.add_argument("--b-min-gain", type=float, default=None,
                   help="override min-gain for arm B only")
    p.add_argument("--rollout-threads", type=int, default=4)
    p.add_argument("--seed-offset", type=int, default=15_000_000)
    args = p.parse_args()

    seeds = list(range(args.seed_offset, args.seed_offset + args.n_games))
    results = []
    t0 = time.time()
    with ProcessPoolExecutor(
        max_workers=args.n_workers, initializer=_worker_init,
        initargs=(args.jar, args.checkpoint, args.nn_model, args.n_worlds,
                  args.top_k, args.z_threshold, args.min_gain,
                  args.a_self, args.a_opp, args.b_self, args.b_opp,
                  args.rollout_threads, args.a_belief, args.b_belief,
                  args.a_search, args.b_search, args.sims, args.c_puct,
                  args.max_depth, args.search_tail, args.n_parallel,
                  args.value_model, args.leaf_plies,
                  args.b_z_threshold, args.b_min_gain),
    ) as pool:
        for res in pool.map(_play_seed, seeds, chunksize=2):
            results.append(res)
            n = len(results)
            if n % 50 == 0:
                a = np.array([r["A"] for r in results])
                b = np.array([r["B"] for r in results])
                d = b - a
                print(f"[{n}/{args.n_games}] {time.time()-t0:.0f}s  "
                      f"A={a.mean():+.2f} B={b.mean():+.2f} "
                      f"delta={d.mean():+.2f}±{d.std()/np.sqrt(n):.2f}")
                sys.stdout.flush()

    a = np.array([r["A"] for r in results])
    b = np.array([r["B"] for r in results])
    d = b - a
    a_sw  = sum(r["A_sw"] for r in results);  b_sw  = sum(r["B_sw"] for r in results)
    a_dec = sum(r["A_dec"] for r in results); b_dec = sum(r["B_dec"] for r in results)
    print(f"\narm A [{args.a_search}] ({args.a_self}/{args.a_opp}): {a.mean():+.3f}/game  "
          f"win {np.mean(a > 0):.1%}  switches {a_sw}/{a_dec} "
          f"({a_sw / max(1, a_dec):.1%})")
    print(f"arm B [{args.b_search}] ({args.b_self}/{args.b_opp}): {b.mean():+.3f}/game  "
          f"win {np.mean(b > 0):.1%}  switches {b_sw}/{b_dec} "
          f"({b_sw / max(1, b_dec):.1%})")
    print(f"paired delta (B−A): {d.mean():+.3f} ± {d.std()/np.sqrt(len(d)):.3f}")


if __name__ == "__main__":
    main()
