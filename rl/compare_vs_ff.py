"""
compare_vs_ff.py — where exactly does the champion beat FirstFelix?

Plays the champion net (seat 0) vs 3×FirstFelix, and at EVERY discard
decision also asks what FirstFelix would play from the same seat
(rollout server `ff_decide`: a fresh FF re-decides its target from this
hand + discard history). Disagreements are priced with the paired CRN
evaluator: both tiles rolled out on the SAME determinized worlds, so the
$ difference between the champion's choice and FF's choice is low-variance.

This decomposes the champion's edge over the strongest heuristic into
per-decision dollars, bucketed by what the champion's choice did:

  tempo  : champion's discard leaves strictly BETTER shanten than FF's
  accept : same shanten, materially higher ukeire
  shape  : same shanten, similar ukeire — subtle re-waiting / safety
  slower : champion's discard leaves WORSE shanten (deliberate slowdown)

NOTE: unlike mine_overrides.py, gains here are NOT argmax-selected —
the two sides are fixed before pricing, so the paired estimates are
unbiased (no winner's curse). Means are trustworthy.

Usage:
    rl/venv/bin/python rl/compare_vs_ff.py --n-games 100 --n-worlds 32 \
        --out rl/analysis/champion_vs_ff_raw.json
"""

import argparse
import json
import sys
import time
from collections import defaultdict
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

# v3 obs offsets (see env.encode_state)
SH_AFTER, UK_AFTER, REMAINING = 587, 621, 578


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--checkpoint", default="rl/checkpoints/best_raw_net.pt")
    p.add_argument("--nn-model", default="rl/checkpoints/student/student48_sp1b.onnx")
    p.add_argument("--n-games", type=int, default=100)
    p.add_argument("--n-worlds", type=int, default=32)
    p.add_argument("--rollout-threads", type=int, default=2)
    p.add_argument("--seed-offset", type=int, default=90_000_000)
    p.add_argument("--out", default="rl/analysis/champion_vs_ff_raw.json")
    args = p.parse_args()

    import torch
    torch.set_num_threads(1)
    from env import MahjongEnv, encode_state
    from model import MahjongAgent
    from mcts import MCTSRolloutClient

    agent = MahjongAgent.load(args.checkpoint, device="cpu")
    agent.net.eval()
    client = MCTSRolloutClient(
        jar_path=args.jar, rollout_opp="firstfelix",
        nn_model=args.nn_model, rollout_threads=args.rollout_threads)
    env = MahjongEnv(args.jar, opponent="firstfelix", obs_version=3)

    rows = []                 # per-disagreement records
    n_dec = n_agree = 0
    rewards, lengths = [], []
    deal_ins = wins = 0
    t0 = time.time()

    for g in range(args.n_games):
        seed = args.seed_offset + g
        try:
            obs, info = env.reset(seed=seed)
        except AssertionError:
            continue
        game_edge, steps = 0.0, 0
        while True:
            steps += 1
            decision, state = info["decision"], info["state"]
            action = agent.select_action(obs, decision, info["action_mask"],
                                         deterministic=True)
            if decision == "discard":
                n_dec += 1
                ff_tile = client.ff_decide(state)
                if ff_tile == action:
                    n_agree += 1
                else:
                    r = client.evaluate_batch(state, [int(action), int(ff_tile)],
                                              args.n_worlds, self_policy="nn")
                    d = r[:, 0] - r[:, 1]          # champion − FF, paired
                    sh = 9.0 - obs[SH_AFTER:SH_AFTER + 34] * 10.0
                    uk = obs[UK_AFTER:UK_AFTER + 34] * 60.0
                    rows.append({
                        "seed": seed,
                        "gain": float(d.mean()),
                        "se": float(d.std(ddof=1) / np.sqrt(len(d))),
                        "champ": int(action), "ff": int(ff_tile),
                        "d_sh": float(sh[action] - sh[ff_tile]),
                        "d_uk": float(uk[action] - uk[ff_tile]),
                        "remaining": float(obs[REMAINING] * 136),
                        "hand": [int(x) for x in
                                 np.rint(np.asarray(state["hand"]))],
                    })
                    game_edge += float(d.mean())
            obs, r, done, info = env.step(action)
            if done:
                rewards.append(float(r))
                lengths.append(steps)
                if r > 0:
                    wins += 1
                if info.get("loser_id") is not None and int(info["loser_id"]) == 0:
                    deal_ins += 1
                break
        if (g + 1) % 10 == 0:
            gains = np.array([x["gain"] for x in rows]) if rows else np.zeros(1)
            print(f"[{g+1}/{args.n_games}] {time.time()-t0:.0f}s  "
                  f"agree {n_agree}/{n_dec} ({n_agree/max(1,n_dec):.1%})  "
                  f"priced {len(rows)}  mean edge ${gains.mean():+.2f}/decision  "
                  f"money {np.mean(rewards):+.2f}/game")
            sys.stdout.flush()

    env.close(); client.close()

    gains = np.array([x["gain"] for x in rows])
    d_sh = np.array([x["d_sh"] for x in rows])
    d_uk = np.array([x["d_uk"] for x in rows])

    def bucket(i):
        if d_sh[i] > 0.25:
            return "tempo"      # champion's tile leaves BETTER shanten (higher quality = lower shanten)
        if d_sh[i] < -0.25:
            return "slower"
        return "accept" if d_uk[i] > 2.0 else "shape"

    buckets = defaultdict(list)
    for i in range(len(rows)):
        buckets[bucket(i)].append(gains[i])

    print(f"\n=== champion vs FirstFelix, {args.n_games} games ===")
    print(f"money: {np.mean(rewards):+.2f}/game  win {wins/max(1,len(rewards)):.1%}  "
          f"deal-in {deal_ins/max(1,len(rewards)):.1%}")
    print(f"discard decisions: {n_dec}  agreement {n_agree/max(1,n_dec):.1%}  "
          f"priced disagreements {len(rows)}")
    print(f"edge when they disagree: ${gains.mean():+.3f} ± "
          f"{gains.std(ddof=1)/np.sqrt(len(gains)):.3f} per decision "
          f"(unbiased paired estimate)")
    print(f"summed over a game: ${gains.sum()/args.n_games:+.2f}/game of "
          f"one-step discard edge")
    print(f"\n{'bucket':8s} {'n':>6s} {'share':>7s} {'mean$':>8s}")
    for b in ("tempo", "accept", "shape", "slower"):
        v = np.array(buckets[b]) if buckets[b] else np.zeros(1)
        print(f"{b:8s} {len(buckets[b]):>6,} {len(buckets[b])/max(1,len(rows)):>7.1%} "
              f"{v.mean():>8.2f}")

    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    with open(args.out, "w") as f:
        json.dump({"games": args.n_games, "rewards": rewards,
                   "n_dec": n_dec, "n_agree": n_agree,
                   "wins": wins, "deal_ins": deal_ins, "lengths": lengths,
                   "rows": rows}, f)
    print(f"\nraw rows → {args.out}")


if __name__ == "__main__":
    main()
