"""
eval_paired_mcts.py — Gate evaluation: paired-worlds MCTS at inference vs baseline.

For each seed, plays TWO games vs a 3×FirstFelix table:
  arm A: plain imitation policy (deterministic argmax)         — baseline
  arm B: PairedMCTSPolicy (MCTS override with significance test)

Paired seeds + paired determinized worlds give a low-variance estimate of the
MCTS improvement delta. This is the gate for investing in ExIt data generation.

Usage
─────
    rl/venv/bin/python rl/eval_paired_mcts.py \
        --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \
        --checkpoint rl/checkpoints/imitation_v2/imitation_final.pt \
        --n-games 500 --n-workers 10 --n-worlds 32 --top-k 6
"""

import argparse
import sys
import time
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

# Worker globals (one set of JVMs per worker process)
_W = {}


def _worker_init(jar, checkpoint, n_worlds, top_k, z_threshold, min_gain):
    import torch
    torch.set_num_threads(1)
    from env import MahjongEnv
    from model import MahjongAgent
    from mcts import MCTSRolloutClient, PairedMCTSPolicy

    agent  = MahjongAgent.load(checkpoint, device="cpu")
    client = MCTSRolloutClient(jar_path=jar, rollout_opp="firstfelix")
    policy = PairedMCTSPolicy(
        net=agent.net, rollout_client=client,
        n_worlds=n_worlds, top_k=top_k,
        z_threshold=z_threshold, min_gain=min_gain, device="cpu",
    )
    _W["agent"]  = agent
    _W["policy"] = policy
    _W["env"]    = MahjongEnv(jar, opponent="firstfelix")


def _play_seed(seed):
    agent, policy, env = _W["agent"], _W["policy"], _W["env"]

    # ── Arm A: baseline (no MCTS) ─────────────────────────────────────────────
    obs, info = env.reset(seed=seed)
    while True:
        action = agent.select_action(obs, info["decision"], info["action_mask"],
                                     deterministic=True)
        obs, r, done, info = env.step(action)
        if done:
            r_base = r
            break

    # ── Arm B: paired MCTS ────────────────────────────────────────────────────
    sw0, nd0 = policy.n_switches, policy.n_discard_decisions
    obs, info = env.reset(seed=seed)
    while True:
        action, _ = policy.get_action(obs, info["state"], info["decision"],
                                      info["context"], info["action_mask"])
        obs, r, done, info = env.step(action)
        if done:
            r_mcts = r
            break

    return {
        "seed":       seed,
        "r_base":     float(r_base),
        "r_mcts":     float(r_mcts),
        "n_switches": policy.n_switches - sw0,
        "n_discards": policy.n_discard_decisions - nd0,
    }


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar",         default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--checkpoint",  default="rl/checkpoints/imitation_v2/imitation_final.pt")
    p.add_argument("--n-games",     type=int,   default=500)
    p.add_argument("--n-workers",   type=int,   default=10)
    p.add_argument("--n-worlds",    type=int,   default=32)
    p.add_argument("--top-k",       type=int,   default=6)
    p.add_argument("--z-threshold", type=float, default=1.5)
    p.add_argument("--min-gain",    type=float, default=0.25)
    p.add_argument("--seed-offset", type=int,   default=0)
    args = p.parse_args()

    print(f"Gate eval: {args.n_games} paired games, {args.n_workers} workers, "
          f"n_worlds={args.n_worlds}, top_k={args.top_k}, "
          f"z={args.z_threshold}, min_gain={args.min_gain}")
    print(f"checkpoint: {args.checkpoint}")
    sys.stdout.flush()

    seeds = list(range(args.seed_offset, args.seed_offset + args.n_games))
    results = []
    t0 = time.time()

    with ProcessPoolExecutor(
        max_workers=args.n_workers,
        initializer=_worker_init,
        initargs=(args.jar, args.checkpoint, args.n_worlds, args.top_k,
                  args.z_threshold, args.min_gain),
    ) as pool:
        for res in pool.map(_play_seed, seeds, chunksize=1):
            results.append(res)
            n = len(results)
            if n % 25 == 0:
                rb = np.array([x["r_base"] for x in results])
                rm = np.array([x["r_mcts"] for x in results])
                d  = rm - rb
                sw = sum(x["n_switches"] for x in results)
                nd = sum(x["n_discards"] for x in results)
                elapsed = time.time() - t0
                print(f"[{n}/{args.n_games}] base={rb.mean():+.2f} "
                      f"mcts={rm.mean():+.2f} delta={d.mean():+.2f}±{d.std()/np.sqrt(n):.2f} "
                      f"| win% base={np.mean(rb > 0):.1%} mcts={np.mean(rm > 0):.1%} "
                      f"| switch rate={sw / max(nd, 1):.1%} ({sw}/{nd}) "
                      f"| {elapsed:.0f}s ({elapsed / n:.1f}s/game)")
                sys.stdout.flush()

    rb = np.array([x["r_base"] for x in results])
    rm = np.array([x["r_mcts"] for x in results])
    d  = rm - rb
    sw = sum(x["n_switches"] for x in results)
    nd = sum(x["n_discards"] for x in results)

    print("\n" + "=" * 64)
    print(f"FINAL ({len(results)} paired games)")
    print(f"  baseline : {rb.mean():+.3f}/game  win {np.mean(rb > 0):.1%}  "
          f"draw {np.mean(rb == 0):.1%}")
    print(f"  MCTS     : {rm.mean():+.3f}/game  win {np.mean(rm > 0):.1%}  "
          f"draw {np.mean(rm == 0):.1%}")
    print(f"  delta    : {d.mean():+.3f} ± {d.std() / np.sqrt(len(d)):.3f} (paired SE)")
    print(f"  switches : {sw}/{nd} discard decisions ({sw / max(nd, 1):.1%})")
    print("=" * 64)


if __name__ == "__main__":
    main()
