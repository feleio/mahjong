"""
evaluate.py — Evaluate a trained Mahjong RL agent.

Runs N games against the chosen opponent type and reports win-rate, average
reward, and per-score-tier breakdown.

Usage
─────
    # vs Chicken bots (default)
    python rl/evaluate.py \\
        --checkpoint rl/checkpoints/final.pt \\
        --jar target/scala-2.12/mahjong-assembly-0.1.0.jar

    # vs FirstFelix
    python rl/evaluate.py \\
        --checkpoint rl/checkpoints/final.pt \\
        --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \\
        --opponent firstfelix
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
    p.add_argument("--checkpoint",    type=str, required=True)
    p.add_argument("--jar",           type=str, required=True)
    p.add_argument("--java",          type=str, default="java")
    p.add_argument("--n-games",       type=int, default=1000)
    p.add_argument("--opponent",      type=str, default="chicken",
                   choices=["chicken", "firstfelix", "mixed"],
                   help="Opponent type: chicken, firstfelix, or mixed (50/50 each game)")
    p.add_argument("--deterministic", action="store_true",
                   help="Greedy policy (argmax) instead of sampling")
    p.add_argument("--device",        type=str, default="cpu")
    return p.parse_args()


def main() -> None:
    args  = parse_args()
    agent = MahjongAgent.load(args.checkpoint, device=args.device)
    agent.net.eval()

    money        = []   # per-game balance: +ve = won $, -ve = paid $
    win_ids      = []
    loser_ids    = []
    self_wins    = []
    agent_scores = []   # raw score (3-10) when agent wins

    print(f"Evaluating vs {args.opponent.upper()} over {args.n_games} games …")
    print("-" * 50)

    with MahjongEnv(args.jar, java_bin=args.java, opponent=args.opponent) as env:
        for game_i in range(args.n_games):
            if game_i % 200 == 0 and game_i > 0:
                wins_so_far = sum(1 for w in win_ids if w == 0)
                avg_m = np.mean(money) if money else 0.0
                print(f"  game {game_i}/{args.n_games} … "
                      f"win rate: {wins_so_far/game_i*100:.1f}%  "
                      f"avg money/game: {avg_m:+.2f}")

            obs, info = env.reset(seed=game_i)
            while True:
                decision = info["decision"]
                mask     = info["action_mask"]
                action   = agent.select_action(obs, decision, mask,
                                               deterministic=args.deterministic)
                obs, reward, done, info = env.step(action)
                if done:
                    money.append(reward)
                    win_ids.extend(info.get("winner_ids", []))
                    lid = info.get("loser_id")
                    if lid is not None:
                        loser_ids.append(int(lid) if isinstance(lid, str) else lid)
                    self_wins.append(info.get("is_self_win", False))
                    sc = info.get("agent_score")
                    if sc is not None:
                        agent_scores.append(int(sc))
                    break

    money      = np.array(money)
    n          = args.n_games
    agent_wins = sum(1 for w in win_ids if w == 0)
    agent_loss = sum(1 for l in loser_ids if l == 0)
    draw_games = sum(1 for m in money if m == 0)
    won        = money[money > 0]   # games where agent received money
    paid       = money[money < 0]   # games where agent paid money

    print("\n" + "=" * 55)
    print(f"  EVALUATION vs {args.opponent.upper()}")
    print("=" * 55)
    print(f"  Games              : {n}")
    print(f"  Agent wins         : {agent_wins:4d}  ({agent_wins/n*100:.1f}%)  [baseline ~25%]")
    print(f"    of which self-win: {sum(self_wins):4d}")
    print(f"  Agent resp. losses : {agent_loss:4d}  ({agent_loss/n*100:.1f}%)")
    print(f"  Draw games         : {draw_games:4d}  ({draw_games/n*100:.1f}%)")
    print("-" * 55)
    print(f"  MONEY (scoreMap $)")
    print(f"  Total money        : {money.sum():+.0f}  over {n} games")
    print(f"  Avg money/game     : {money.mean():+.2f}  (target: > 0)")
    print(f"  Money std          : {money.std():.2f}")
    if len(won):
        print(f"  Avg money on win   : {won.mean():+.2f}  (over {len(won)} games)")
    if len(paid):
        print(f"  Avg money on loss  : {paid.mean():+.2f}  (over {len(paid)} games)")
    print("-" * 55)
    # Score tier breakdown (3–10 pts)
    if agent_scores:
        from collections import Counter
        print(f"  Win score distribution:")
        for pts, cnt in sorted(Counter(agent_scores).items()):
            bar = "█" * cnt
            print(f"    {pts:2d} pts : {cnt:3d} wins  {bar}")
        print(f"  Avg win score      : {np.mean(agent_scores):.2f} pts")
        print(f"  Max win score      : {max(agent_scores)} pts")
    else:
        print("  No wins recorded — score distribution unavailable.")
    print("=" * 55)


if __name__ == "__main__":
    main()
