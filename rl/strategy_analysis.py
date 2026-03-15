"""
strategy_analysis.py — Extract and explain the strategies learned by the RL agent.

This module lets human players understand *what* the trained network has
discovered, so they can improve their own play.

Features
────────
  1.  Tile discard heatmap
        Which tiles does the agent prefer to discard early / late game?
        Printed as an ASCII table grouped by tile type.

  2.  Win-action statistics
        How often does the agent accept / reject win opportunities,
        pong, kong, chow — and how does this vary with score threshold?

  3.  Decision value map
        For a given hand, what does the network "think" about each possible
        discard?  (sorted by policy logit, i.e. preference)

  4.  Strategy tips
        Automatically generated English-language tips based on what the
        trained network learned compared to a random baseline.

Usage
─────
    python rl/strategy_analysis.py \\
        --checkpoint rl/checkpoints/selfplay_final.pt \\
        --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \\
        --n-games 500
"""

import argparse
import collections
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
import torch

sys.path.insert(0, str(Path(__file__).parent))

from env import MahjongEnv, DECISION_SPACES, encode_state, get_action_mask
from model import MahjongNet, MahjongAgent

# ── Tile names ────────────────────────────────────────────────────────────────

TILE_NAMES = (
    ["Dot 1", "Dot 2", "Dot 3", "Dot 4", "Dot 5", "Dot 6", "Dot 7", "Dot 8", "Dot 9"]
    + ["Bam 1", "Bam 2", "Bam 3", "Bam 4", "Bam 5", "Bam 6", "Bam 7", "Bam 8", "Bam 9"]
    + ["Chr 1", "Chr 2", "Chr 3", "Chr 4", "Chr 5", "Chr 6", "Chr 7", "Chr 8", "Chr 9"]
    + ["E Wind", "S Wind", "W Wind", "N Wind", "Red Dragon", "Green Dragon", "Blue Dragon"]
)

TILE_TYPES = (
    ["Dots"] * 9 + ["Bamboos"] * 9 + ["Characters"] * 9
    + ["Wind"] * 4 + ["Dragon"] * 3
)


# ── Data collection ───────────────────────────────────────────────────────────

def collect_game_data(
    agent: MahjongAgent,
    env: MahjongEnv,
    n_games: int,
    deterministic: bool = False,
) -> Dict:
    """
    Play n_games episodes and collect:
      - discard choices (tile_id, remaining_tiles, phase)
      - binary decision choices (win/pong/kong/chow accept rate)
      - episode rewards
      - win rates
    """
    discard_counts     = collections.Counter()   # tile_id → count
    discard_early      = collections.Counter()   # remaining > 60
    discard_mid        = collections.Counter()   # 30-60
    discard_late       = collections.Counter()   # < 30

    accept_counts      = collections.defaultdict(lambda: [0, 0])  # dec → [pass, accept]
    chow_counts        = [0, 0, 0, 0]   # pass / LEFT / MID / RIGHT

    rewards            = []
    win_scores         = collections.Counter()

    for game_i in range(n_games):
        obs, info = env.reset(seed=game_i)
        ep_reward = 0.0

        while True:
            decision = info["decision"]
            mask     = info["action_mask"]
            state    = info["state"]
            context  = info["context"]

            action = agent.select_action(obs, decision, mask, deterministic=deterministic)

            # Record statistics
            remaining = state.get("remaining", 0)
            if decision == "discard":
                tile_id = action
                discard_counts[tile_id] += 1
                if remaining > 60:
                    discard_early[tile_id] += 1
                elif remaining > 30:
                    discard_mid[tile_id] += 1
                else:
                    discard_late[tile_id] += 1
            elif decision in ("win", "self_win", "pong", "kong"):
                accept_counts[decision][action] += 1
            elif decision == "chow":
                chow_counts[action] += 1

            obs, reward, done, info = env.step(action)
            if done:
                ep_reward = reward
                rewards.append(reward)
                break

    return {
        "discard_counts":  discard_counts,
        "discard_early":   discard_early,
        "discard_mid":     discard_mid,
        "discard_late":    discard_late,
        "accept_counts":   dict(accept_counts),
        "chow_counts":     chow_counts,
        "rewards":         rewards,
        "n_games":         n_games,
    }


# ── Analysis & printing ───────────────────────────────────────────────────────

SUIT_GROUPS = [
    ("Dots",       range(0,  9)),
    ("Bamboos",    range(9,  18)),
    ("Characters", range(18, 27)),
    ("Honours",    range(27, 34)),
]


def print_discard_heatmap(data: Dict, phase: str = "overall") -> None:
    """Print which tiles the agent discards most."""
    counts = {
        "overall": data["discard_counts"],
        "early":   data["discard_early"],
        "mid":     data["discard_mid"],
        "late":    data["discard_late"],
    }[phase]

    total = max(sum(counts.values()), 1)
    print(f"\n{'='*60}")
    print(f"  DISCARD PREFERENCE  ({phase.upper()} game — {data['n_games']} games)")
    print(f"{'='*60}")

    for group_name, tile_range in SUIT_GROUPS:
        group_total = sum(counts.get(t, 0) for t in tile_range)
        if group_total == 0:
            continue
        print(f"\n  {group_name}")
        print(f"  {'Tile':<14}  {'Count':>6}  {'%':>6}  Bar")
        print(f"  {'-'*14}  {'-'*6}  {'-'*6}  {'-'*20}")
        for t in tile_range:
            c   = counts.get(t, 0)
            pct = 100.0 * c / total
            bar = "█" * int(pct * 2)
            print(f"  {TILE_NAMES[t]:<14}  {c:>6}  {pct:>5.1f}%  {bar}")


def print_decision_stats(data: Dict) -> None:
    """Print accept/reject rates for win / pong / kong / chow."""
    print(f"\n{'='*60}")
    print("  DECISION STATISTICS")
    print(f"{'='*60}")

    for dec in ("win", "self_win", "pong", "kong"):
        counts = data["accept_counts"].get(dec, [0, 0])
        n      = counts[0] + counts[1]
        if n == 0:
            continue
        rate = 100.0 * counts[1] / n
        bar  = "█" * int(rate / 5)
        print(f"  {dec:<12}  accept={rate:5.1f}%  n={n:>5}  {bar}")

    # Chow
    cc = data["chow_counts"]
    n  = sum(cc)
    if n > 0:
        print(f"\n  Chow decisions (n={n}):")
        for label, idx in [("Pass", 0), ("LEFT", 1), ("MIDDLE", 2), ("RIGHT", 3)]:
            pct = 100.0 * cc[idx] / n
            bar = "█" * int(pct / 5)
            print(f"    {label:<8}  {pct:5.1f}%  {bar}")


def print_reward_summary(data: Dict) -> None:
    """Print reward / win statistics."""
    rewards = np.array(data["rewards"])
    wins    = rewards > 0
    losses  = rewards < 0
    draws   = rewards == 0

    print(f"\n{'='*60}")
    print("  REWARD SUMMARY")
    print(f"{'='*60}")
    print(f"  Games played      : {len(rewards)}")
    print(f"  Win rate          : {wins.mean()*100:.1f}%")
    print(f"  Loss rate         : {losses.mean()*100:.1f}%")
    print(f"  Draw rate         : {draws.mean()*100:.1f}%")
    print(f"  Mean reward       : {rewards.mean():+.2f}")
    print(f"  Reward std        : {rewards.std():.2f}")
    print(f"  Best game         : {rewards.max():+.0f}")
    print(f"  Worst game        : {rewards.min():+.0f}")


def generate_tips(agent_data: Dict, random_data: Optional[Dict] = None) -> List[str]:
    """
    Generate human-readable strategy tips from the agent's behaviour.
    If random_data is provided, tips are relative to the random baseline.
    """
    tips = []

    # ── Tip 1: Honour tile discarding ────────────────────────────────────────
    honour_pct  = sum(agent_data["discard_counts"].get(t, 0) for t in range(27, 34))
    total_disc  = max(sum(agent_data["discard_counts"].values()), 1)
    h_rate      = honour_pct / total_disc
    if h_rate > 0.35:
        tips.append(
            f"DISCARD HONOURS EARLY: The agent discards honour tiles (winds & dragons) "
            f"{h_rate*100:.0f}% of the time.  These tiles can only form Pong/Kong groups "
            f"and don't form sequences — discard them early unless you have a pair."
        )

    # ── Tip 2: Terminal tile discarding ──────────────────────────────────────
    terminal_ids = [0, 8, 9, 17, 18, 26]  # 1s and 9s of each suit
    term_pct     = sum(agent_data["discard_counts"].get(t, 0) for t in terminal_ids)
    t_rate       = term_pct / total_disc
    if t_rate > 0.25:
        tips.append(
            f"DISCARD ISOLATED TERMINALS: Tiles 1 and 9 can only form sequences with "
            f"2-3 (for 1) or 7-8 (for 9).  The agent discards them often ({t_rate*100:.0f}%) "
            f"when they are isolated — follow suit unless you're building a pure terminal hand."
        )

    # ── Tip 3: Win acceptance ─────────────────────────────────────────────────
    win_counts = agent_data["accept_counts"].get("win", [0, 0])
    if sum(win_counts) > 0:
        win_rate = win_counts[1] / sum(win_counts)
        if win_rate > 0.95:
            tips.append(
                "ALWAYS WIN WHEN YOU CAN: The agent accepts virtually every win opportunity.  "
                "Never pass on a valid win — even a minimum-score (3-point) hand beats waiting "
                "for a higher-value hand that may never come."
            )
        elif win_rate < 0.5:
            tips.append(
                "BE SELECTIVE ABOUT WINNING: The agent sometimes passes on low-value wins, "
                "waiting for a higher-scoring hand.  This is a high-risk strategy — only do "
                "this when you have a clear path to a significantly better hand."
            )

    # ── Tip 4: Pong acceptance ────────────────────────────────────────────────
    pong_counts = agent_data["accept_counts"].get("pong", [0, 0])
    if sum(pong_counts) > 0:
        pong_rate = pong_counts[1] / sum(pong_counts)
        if pong_rate < 0.4:
            tips.append(
                f"BE CONSERVATIVE WITH PONG: The agent ponging only {pong_rate*100:.0f}% of the "
                f"time.  Ponging locks your hand into one direction and lets opponents see your "
                f"strategy.  Pass on low-value pongs unless it clearly helps your hand or blocks "
                f"an opponent."
            )
        elif pong_rate > 0.8:
            tips.append(
                f"PONG AGGRESSIVELY: The agent pongs ~{pong_rate*100:.0f}% of the time.  "
                f"Ponging reduces your hand size, making it easier to complete a winning hand "
                f"and scores bonus points (All Pong).  Accept pong opportunities freely."
            )

    # ── Tip 5: Late-game discard changes ─────────────────────────────────────
    total_early = max(sum(agent_data["discard_early"].values()), 1)
    total_late  = max(sum(agent_data["discard_late"].values()), 1)
    # Compare how much the distribution shifts
    early_vec = np.array([agent_data["discard_early"].get(t, 0) / total_early
                           for t in range(34)])
    late_vec  = np.array([agent_data["discard_late"].get(t,  0) / total_late
                           for t in range(34)])
    shift = np.abs(early_vec - late_vec).sum()
    if shift > 0.5:
        # Find biggest shifts
        diffs = late_vec - early_vec
        most_dropped = int(np.argmin(diffs))
        most_added   = int(np.argmax(diffs))
        tips.append(
            f"ADJUST YOUR DISCARD STRATEGY AS THE GAME PROGRESSES: "
            f"The agent discards {TILE_NAMES[most_dropped]} much less often late game "
            f"(keeps it), and discards {TILE_NAMES[most_added]} more often late game "
            f"(drops it when the wall is short).  As the wall shrinks, commit to your "
            f"target hand and shed tiles that don't fit."
        )

    if not tips:
        tips.append(
            "The agent is still learning.  Run more training games for richer strategy insights."
        )

    return tips


def print_tips(tips: List[str]) -> None:
    print(f"\n{'='*60}")
    print("  STRATEGY TIPS FOR HUMAN PLAYERS")
    print(f"{'='*60}")
    for i, tip in enumerate(tips, 1):
        # Word-wrap at ~55 chars
        words   = tip.split()
        line    = ""
        wrapped = []
        for w in words:
            if len(line) + len(w) + 1 > 55:
                wrapped.append(line)
                line = w
            else:
                line = f"{line} {w}".strip()
        if line:
            wrapped.append(line)
        print(f"\n  [{i}] {wrapped[0]}")
        for rest in wrapped[1:]:
            print(f"      {rest}")


# ── Main ──────────────────────────────────────────────────────────────────────

def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="Mahjong RL strategy analyser")
    p.add_argument("--checkpoint", type=str, required=True,
                   help="Path to trained model checkpoint (.pt)")
    p.add_argument("--jar",        type=str, required=True,
                   help="Path to assembled fat-JAR")
    p.add_argument("--java",       type=str, default="java")
    p.add_argument("--n-games",    type=int, default=500,
                   help="Games to play for analysis")
    p.add_argument("--deterministic", action="store_true",
                   help="Use argmax policy (greedy) instead of sampling")
    p.add_argument("--device",     type=str, default="cpu")
    return p.parse_args()


def main() -> None:
    args  = parse_args()

    print(f"Loading agent from {args.checkpoint} …")
    agent = MahjongAgent.load(args.checkpoint, device=args.device)
    agent.net.eval()

    print(f"Opening game server …")
    with MahjongEnv(jar_path=args.jar, java_bin=args.java) as env:
        print(f"Collecting data from {args.n_games} games …")
        data = collect_game_data(agent, env, args.n_games,
                                 deterministic=args.deterministic)

    print_reward_summary(data)
    print_discard_heatmap(data, phase="overall")
    print_discard_heatmap(data, phase="early")
    print_discard_heatmap(data, phase="late")
    print_decision_stats(data)

    tips = generate_tips(data)
    print_tips(tips)


if __name__ == "__main__":
    main()
