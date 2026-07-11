"""
test_danger_labels.py — smoke test for the #21 danger-label instrumentation.

Plays random-valid-action games against FirstFelix with -Drl.dangerlabels=true
and checks, on every observation:

  1. shape/typing: opp_tenpai is [3] of {0,1}, opp_waits is [3][34] of {0,1}
  2. consistency:  opp_tenpai[j] == (sum(opp_waits[j]) > 0)

plus the strong end-to-end invariant:

  3. deal-in consistency: whenever WE deal in (loser_id == 0, not a self win),
     every winner must have been labelled waiting on the exact tile we just
     discarded, at the observation where we chose that discard. A win on our
     discard is an immediate reaction, so the winner's hand cannot have
     changed between the label and the win.

  4. flag off ⇒ labels absent (no accidental hidden-state leak into default
     datagen/eval paths).

Usage:
    rl/venv/bin/python rl/test_danger_labels.py --n-games 200
"""

import argparse
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

from env import MahjongEnv  # noqa: E402


def check_labels(state, where):
    tenpai = state.get("opp_tenpai")
    waits = state.get("opp_waits")
    assert tenpai is not None and waits is not None, f"labels missing at {where}"
    assert len(tenpai) == 3 and all(t in (0, 1) for t in tenpai), \
        f"bad opp_tenpai {tenpai} at {where}"
    assert len(waits) == 3 and all(len(w) == 34 for w in waits), \
        f"bad opp_waits shape at {where}"
    assert all(v in (0, 1) for w in waits for v in w), \
        f"non-binary opp_waits at {where}"
    for j in range(3):
        assert tenpai[j] == (1 if sum(waits[j]) > 0 else 0), \
            f"tenpai[{j}]={tenpai[j]} but sum(waits)={sum(waits[j])} at {where}"
    return tenpai, waits


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--n-games", type=int, default=200)
    p.add_argument("--seed-offset", type=int, default=77_000_000)
    args = p.parse_args()

    rng = np.random.default_rng(0)
    env = MahjongEnv(args.jar, opponent="firstfelix", obs_version=3,
                     danger_labels=True)

    n_obs = 0
    tenpai_hits = 0          # opponent-decision pairs where that opponent is tenpai
    deal_ins = 0             # games where we dealt in (invariant-3 checks)
    wait_pop = []            # wait-set sizes of tenpai opponents

    for g in range(args.n_games):
        seed = args.seed_offset + g
        try:
            obs, info = env.reset(seed=seed)
        except AssertionError:
            continue  # game over before our first decision
        last_discard = None  # (tile, waits) at our most recent discard choice
        was_discard = False  # previous step's decision was a discard
        while True:
            where = f"game {g} seed {seed} obs {n_obs} ({info['decision']})"
            tenpai, waits = check_labels(info["state"], where)
            n_obs += 1
            tenpai_hits += sum(tenpai)
            wait_pop += [sum(w) for j, w in enumerate(waits) if tenpai[j]]

            action = int(rng.choice(np.where(info["action_mask"])[0]))
            if info["decision"] == "discard":
                last_discard = (action, waits)
            was_discard = info["decision"] == "discard"
            obs, r, done, info = env.step(action)
            if done:
                loser = info.get("loser_id")
                if loser is not None and int(loser) == 0 and not info["is_self_win"]:
                    # We dealt in: the win is an immediate reaction to our
                    # discard, so the game must have ended on that very step
                    # and every winner must have been labelled waiting on it.
                    assert was_discard, \
                        f"deal-in not immediately after our discard at {where}"
                    tile, w = last_discard
                    deal_ins += 1
                    for wid in info["winner_ids"]:
                        rel = (int(wid) - 1) % 4  # seat-relative index of winner
                        assert w[rel][tile] == 1, (
                            f"deal-in tile {tile} to seat {wid} not in labelled "
                            f"waits {np.where(np.array(w[rel]))[0].tolist()} "
                            f"(game {g} seed {seed})")
                break

    env.close()
    assert deal_ins > 0, "no deal-ins seen — invariant 3 never exercised"
    print(f"OK: {args.n_games} games, {n_obs} observations")
    print(f"  opponent tenpai rate : {tenpai_hits / (3 * n_obs):.1%} "
          f"(per opponent-observation)")
    print(f"  mean wait-set size   : {np.mean(wait_pop):.2f} tiles when tenpai")
    print(f"  deal-ins verified    : {deal_ins} "
          f"(winner's labelled waits contained the discarded tile)")

    # 4. flag off ⇒ labels absent
    env = MahjongEnv(args.jar, opponent="firstfelix", obs_version=3)
    obs, info = env.reset(seed=args.seed_offset)
    assert "opp_tenpai" not in info["state"] and "opp_waits" not in info["state"], \
        "danger labels leaked with danger_labels=False"
    env.close()
    print("  flag off             : labels absent, as required")


if __name__ == "__main__":
    main()
