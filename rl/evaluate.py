"""
evaluate.py — Evaluate a trained Mahjong RL agent.

Runs N games against Chicken bots and reports win-rate, average reward,
and per-score-tier breakdown.

Usage
─────
    python rl/evaluate.py \\
        --checkpoint rl/checkpoints/final.pt \\
        --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \\
        --n-games 1000
"""

import argparse
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

from env import MahjongEnv
from model import MahjongAgent


def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Evaluate a trained Mahjong RL agent")
    p.add_argument("--checkpoint",   type=str, required=True)
    p.add_argument("--jar",          type=str, required=True)
    p.add_argument("--java",         type=str, default="java")
    p.add_argument("--n-games",      type=int, default=1000)
    p.add_argument("--deterministic",action="store_true",
                   help="Greedy policy (argmax) instead of sampling")
    p.add_argument("--device",       type=str, default="cpu")
    return p.parse_args()


def main() -> None:
    args  = parse_args()
    agent = MahjongAgent.load(args.checkpoint, device=args.device)
    agent.net.eval()

    rewards    = []
    win_ids    = []
    loser_ids  = []
    self_wins  = []

    with MahjongEnv(args.jar, java_bin=args.java) as env:
        for game_i in range(args.n_games):
            if game_i % 100 == 0:
                print(f"  game {game_i}/{args.n_games} …")

            obs, info = env.reset(seed=game_i)
            while True:
                decision = info["decision"]
                mask     = info["action_mask"]
                action   = agent.select_action(obs, decision, mask,
                                               deterministic=args.deterministic)
                obs, reward, done, info = env.step(action)
                if done:
                    rewards.append(reward)
                    win_ids.extend(info.get("winner_ids", []))
                    lid = info.get("loser_id")
                    if lid is not None:
                        loser_ids.append(int(lid) if isinstance(lid, str) else lid)
                    self_wins.append(info.get("is_self_win", False))
                    break

    rewards   = np.array(rewards)
    agent_wins = sum(1 for w in win_ids if w == 0)
    agent_loss = sum(1 for l in loser_ids if l == 0)

    print("\n" + "=" * 50)
    print("  EVALUATION RESULTS")
    print("=" * 50)
    print(f"  Games           : {args.n_games}")
    print(f"  Agent wins      : {agent_wins}  ({agent_wins/args.n_games*100:.1f}%)")
    print(f"  Agent losses    : {agent_loss}  ({agent_loss/args.n_games*100:.1f}%)")
    print(f"  Self-win games  : {sum(self_wins)}")
    print(f"  Mean reward     : {rewards.mean():+.2f}")
    print(f"  Reward std      : {rewards.std():.2f}")
    print(f"  Max reward      : {rewards.max():+.0f}")
    print(f"  Min reward      : {rewards.min():+.0f}")
    print("=" * 50)


if __name__ == "__main__":
    main()
