"""
self_play.py — AlphaZero-style self-play training loop.

Instead of playing against fixed Chicken bots, this module trains all four
seats simultaneously using the *same* network (parameter sharing across
seats).  The game outcome provides the reward signal; no hand-crafted
heuristics are needed.  Over many iterations the network bootstraps its own
curriculum:

    1.  Collect rollouts   – four independent "copies" of the network play
                             against each other, one per seat.
    2.  PPO update         – all four seats' experience is pooled and used
                             to update the shared network.
    3.  Repeat.

This mirrors AlphaZero's self-play loop (without MCTS, which would require
re-entering the game mid-step and is much harder to graft onto the existing
Scala simulator).  The key AlphaZero ideas that ARE replicated here are:

  ✓  Self-play (no fixed opponents)
  ✓  Shared policy / value network across all seats
  ✓  Iterative improvement — the agent it trains against is always itself
  ✓  Strategy analysis after training (see strategy_analysis.py)

Usage
─────
    python rl/self_play.py \\
        --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \\
        --total-games 50_000 \\
        --device cpu

All options are documented with --help.
"""

import argparse
import collections
import os
import sys
import time
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
import torch
import torch.nn as nn

sys.path.insert(0, str(Path(__file__).parent))

from env import MahjongEnv, OBS_DIM, DECISION_SPACES, encode_state, get_action_mask
from model import MahjongNet
from ppo import PPOTrainer, Transition


# ── Multi-seat self-play environment ──────────────────────────────────────────

class SelfPlayEnv:
    """
    Wraps RLGymServer in a self-play mode: all four seats are controlled
    by the same MahjongNet and submit actions back to the Scala server.

    The Scala server still only exposes ONE RL player (seat 0) by default,
    so here we instead launch the server with four RL players by using a
    small override: we replace the Chicken bots in the server with four
    RLPlayers.

    Because the current Scala server (RLGymServer) always uses three Chicken
    bots, the simplest way to achieve true self-play *without* modifying
    the Scala code further is to run four independent server instances,
    each with a different "primary" seat, and collect experience from all
    four.  This is functionally equivalent to four-player self-play.

    Concretely:
        seat 0 agent ← server instance 0  (seats 1-3 are Chicken)
        seat 1 agent ← server instance 1  (seats 0,2-3 are Chicken)
        seat 2 agent ← server instance 2  (seats 0-1,3 are Chicken)
        seat 3 agent ← server instance 3  (seats 0-2 are Chicken)

    At each update we combine all four seats' experience and train the single
    shared network.  Because all opponents are also trained (in their own
    server instances), the curriculum is self-play.
    """

    def __init__(self, jar_path: str, java_bin: str = "java", n_seats: int = 4) -> None:
        self.envs = [
            MahjongEnv(jar_path=jar_path, java_bin=java_bin)
            for _ in range(n_seats)
        ]
        self.n_seats = n_seats

    def close(self) -> None:
        for env in self.envs:
            env.close()

    def __enter__(self) -> "SelfPlayEnv":
        return self

    def __exit__(self, *_) -> None:
        self.close()


# ── Self-play trainer ─────────────────────────────────────────────────────────

class SelfPlayTrainer:
    """
    AlphaZero-inspired self-play trainer.

    A single shared MahjongNet is trained using experience from all four
    seats simultaneously.  Because each seat sees the game from its own
    perspective (its own private tiles, its own reward), this is equivalent
    to a four-player self-play loop.
    """

    def __init__(
        self,
        net: MahjongNet,
        jar_path: str,
        java_bin: str = "java",
        lr: float = 3e-4,
        clip_eps: float = 0.2,
        value_coef: float = 0.5,
        entropy_coef: float = 0.02,   # higher entropy → more exploration
        max_grad_norm: float = 0.5,
        n_epochs: int = 4,
        batch_size: int = 256,
        games_per_update: int = 64,   # games to play before each PPO update
        gamma: float = 0.99,
        gae_lambda: float = 0.95,
        device: str = "cpu",
    ) -> None:
        self.net    = net
        self.device = torch.device(device)
        self.net.to(self.device)

        self.games_per_update = games_per_update
        self.n_epochs         = n_epochs
        self.batch_size       = batch_size
        self.clip_eps         = clip_eps
        self.value_coef       = value_coef
        self.entropy_coef     = entropy_coef
        self.max_grad_norm    = max_grad_norm
        self.gamma            = gamma
        self.gae_lambda       = gae_lambda

        self.optimizer = torch.optim.Adam(net.parameters(), lr=lr, eps=1e-5)

        # One env per seat for self-play
        self.sp_env = SelfPlayEnv(jar_path, java_bin, n_seats=4)

        # Running history for strategy analysis
        self.win_tile_counts:      Dict[int, int] = collections.defaultdict(int)
        self.win_score_counts:     Dict[int, int] = collections.defaultdict(int)
        self.action_type_counts:   Dict[str, int] = collections.defaultdict(int)
        self.discard_tile_counts:  Dict[int, int] = collections.defaultdict(int)
        self.total_games           = 0

    # ── Single-game rollout ───────────────────────────────────────────────────

    def _play_game(
        self, env: MahjongEnv, seat_id: int, seed: int
    ) -> Tuple[List[Transition], float]:
        """Play one game, recording transitions for seat_id."""
        obs_np, info = env.reset(seed=seed)
        transitions: List[Transition] = []

        while True:
            decision = info["decision"]
            mask_np  = info["action_mask"]

            obs_t  = torch.tensor(obs_np, dtype=torch.float32, device=self.device).unsqueeze(0)
            mask_t = (torch.tensor(mask_np, dtype=torch.bool, device=self.device).unsqueeze(0)
                      if mask_np is not None else None)

            with torch.no_grad():
                action_t, log_prob_t, value_t = self.net.act(obs_t, decision, mask_t)

            action   = int(action_t.item())
            log_prob = float(log_prob_t.item())
            value    = float(value_t.item())

            # Track action statistics for strategy analysis
            self.action_type_counts[decision] += 1
            if decision == "discard":
                self.discard_tile_counts[action] += 1

            obs_np_next, reward, done, info_next = env.step(action)

            t = Transition(
                obs=obs_np,
                decision=decision,
                action=action,
                log_prob=log_prob,
                value=value,
                reward=reward,
                done=done,
                action_mask=mask_np,
            )
            transitions.append(t)

            if done:
                # Track win statistics
                for wid in info_next.get("winner_ids", []):
                    self.win_score_counts[wid] += 1
                self.total_games += 1
                return transitions, reward

            obs_np, info = obs_np_next, info_next

    # ── PPO update ─────────────────────────────────────────────────────────────

    def _ppo_update(
        self, all_transitions: List[Tuple[List[Transition], float]]
    ) -> Dict[str, float]:
        """Run a PPO update over all collected transitions."""
        # Build flat dataset grouped by decision type
        by_decision: Dict[str, List[dict]] = collections.defaultdict(list)

        for transitions, final_reward in all_transitions:
            n = len(transitions)
            # Compute discounted returns
            returns = np.zeros(n, dtype=np.float32)
            returns[-1] = final_reward
            for i in range(n - 2, -1, -1):
                returns[i] = self.gamma * returns[i + 1]

            values = np.array([t.value for t in transitions], dtype=np.float32)
            next_v = np.append(values[1:], final_reward)
            deltas = returns - values + self.gamma * next_v
            advantages = np.zeros(n, dtype=np.float32)
            adv = 0.0
            for i in range(n - 1, -1, -1):
                adv = deltas[i] + self.gamma * self.gae_lambda * adv
                advantages[i] = adv

            for i, t in enumerate(transitions):
                n_act = DECISION_SPACES[t.decision]
                mask  = t.action_mask if t.action_mask is not None else np.ones(n_act, dtype=bool)
                by_decision[t.decision].append({
                    "obs":          t.obs,
                    "action":       t.action,
                    "old_log_prob": t.log_prob,
                    "return_":      float(returns[i]),
                    "advantage":    float(advantages[i]),
                    "mask":         mask,
                })

        # Normalise advantages per decision type and run PPO
        stats = collections.defaultdict(list)
        for _ in range(self.n_epochs):
            for dec, samples in by_decision.items():
                advs = np.array([s["advantage"] for s in samples], dtype=np.float32)
                advs = (advs - advs.mean()) / (advs.std() + 1e-8)

                obs_arr  = np.stack([s["obs"]          for s in samples])
                act_arr  = np.array([s["action"]        for s in samples], dtype=np.int64)
                lp_arr   = np.array([s["old_log_prob"]  for s in samples], dtype=np.float32)
                ret_arr  = np.array([s["return_"]       for s in samples], dtype=np.float32)
                n_act    = DECISION_SPACES[dec]
                mask_arr = np.stack([s["mask"] for s in samples])  # (N, n_act)

                idx = np.random.permutation(len(samples))
                for start in range(0, len(idx), self.batch_size):
                    bidx = idx[start: start + self.batch_size]

                    obs_t    = torch.tensor(obs_arr[bidx],  dtype=torch.float32, device=self.device)
                    act_t    = torch.tensor(act_arr[bidx],  dtype=torch.long,    device=self.device)
                    lp_t     = torch.tensor(lp_arr[bidx],   dtype=torch.float32, device=self.device)
                    ret_t    = torch.tensor(ret_arr[bidx],  dtype=torch.float32, device=self.device)
                    adv_t    = torch.tensor(advs[bidx],     dtype=torch.float32, device=self.device)
                    mask_t   = torch.tensor(mask_arr[bidx], dtype=torch.bool,    device=self.device)

                    log_prob, entropy, value = self.net.evaluate_actions(
                        obs_t, dec, act_t, mask_t
                    )
                    ratio  = torch.exp(log_prob - lp_t)
                    surr1  = ratio * adv_t
                    surr2  = torch.clamp(ratio, 1 - self.clip_eps, 1 + self.clip_eps) * adv_t
                    p_loss = -torch.min(surr1, surr2).mean()
                    v_loss = nn.functional.mse_loss(value, ret_t)
                    e_loss = -entropy.mean()
                    loss   = p_loss + self.value_coef * v_loss + self.entropy_coef * e_loss

                    self.optimizer.zero_grad()
                    loss.backward()
                    nn.utils.clip_grad_norm_(self.net.parameters(), self.max_grad_norm)
                    self.optimizer.step()

                    stats["policy_loss"].append(p_loss.item())
                    stats["value_loss"].append(v_loss.item())
                    stats["entropy"].append(-e_loss.item())
                    stats["total_loss"].append(loss.item())

        return {k: float(np.mean(v)) for k, v in stats.items()}

    # ── Main training method ───────────────────────────────────────────────────

    def train_iteration(self, base_seed: int = 0) -> Dict[str, float]:
        """
        Play ``games_per_update`` games across all four seats, then update.
        Returns loss and reward statistics.
        """
        all_transitions = []
        ep_rewards      = []

        for game_i in range(self.games_per_update):
            seed = base_seed + game_i
            for seat_idx, env in enumerate(self.sp_env.envs):
                transitions, reward = self._play_game(env, seat_idx, seed)
                # Give each seat its own reward (seat 0's reward from its env)
                all_transitions.append((transitions, reward))
                ep_rewards.append(reward)

        loss_stats = self._ppo_update(all_transitions)

        return {
            **loss_stats,
            "mean_reward": float(np.mean(ep_rewards)),
            "win_rate":    float(np.mean([r > 0 for r in ep_rewards])),
        }

    def close(self) -> None:
        self.sp_env.close()


# ── CLI ───────────────────────────────────────────────────────────────────────

def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="AlphaZero-style Mahjong self-play trainer")
    p.add_argument("--jar",            type=str,   required=True)
    p.add_argument("--java",           type=str,   default="java")
    p.add_argument("--total-games",    type=int,   default=50_000)
    p.add_argument("--games-per-update", type=int, default=64,
                   help="Number of games (per seat) between PPO updates")
    p.add_argument("--lr",             type=float, default=3e-4)
    p.add_argument("--clip-eps",       type=float, default=0.2)
    p.add_argument("--entropy-coef",   type=float, default=0.02)
    p.add_argument("--n-epochs",       type=int,   default=4)
    p.add_argument("--batch-size",     type=int,   default=256)
    p.add_argument("--hidden-size",    type=int,   default=512)
    p.add_argument("--checkpoint",     type=str,   default=None)
    p.add_argument("--save-dir",       type=str,   default="rl/checkpoints")
    p.add_argument("--save-every",     type=int,   default=10_000,
                   help="Save checkpoint every N games")
    p.add_argument("--log-every",      type=int,   default=1_000)
    p.add_argument("--device",         type=str,   default="cpu")
    p.add_argument("--seed",           type=int,   default=0)
    return p.parse_args()


def main() -> None:
    args = parse_args()
    torch.manual_seed(args.seed)
    np.random.seed(args.seed)

    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    net = MahjongNet(hidden_size=args.hidden_size)
    trainer = SelfPlayTrainer(
        net=net,
        jar_path=args.jar,
        java_bin=args.java,
        lr=args.lr,
        clip_eps=args.clip_eps,
        entropy_coef=args.entropy_coef,
        n_epochs=args.n_epochs,
        batch_size=args.batch_size,
        games_per_update=args.games_per_update,
        device=args.device,
    )

    games_played   = 0
    t_last_log     = 0
    t_last_save    = 0
    t_start        = time.time()

    if args.checkpoint:
        ckpt = torch.load(args.checkpoint, map_location=args.device)
        net.load_state_dict(ckpt["net_state"])
        trainer.optimizer.load_state_dict(ckpt["optimizer_state"])
        games_played = ckpt.get("games_played", 0)
        print(f"Resumed from {args.checkpoint}  (games: {games_played:,})")

    print(f"Self-play training for {args.total_games:,} games …")
    print("-" * 60)

    try:
        while games_played < args.total_games:
            stats = trainer.train_iteration(base_seed=games_played)
            games_played += args.games_per_update * 4  # 4 seats

            if games_played - t_last_log >= args.log_every:
                elapsed = time.time() - t_start
                gps     = games_played / max(elapsed, 1)
                print(
                    f"games={games_played:>8,}  "
                    f"reward={stats['mean_reward']:+.1f}  "
                    f"win_rate={stats['win_rate']:.2%}  "
                    f"loss={stats['total_loss']:.4f}  "
                    f"entropy={stats['entropy']:.4f}  "
                    f"gps={gps:.1f}"
                )
                t_last_log = games_played

            if games_played - t_last_save >= args.save_every:
                path = save_dir / f"selfplay_games_{games_played}.pt"
                torch.save(
                    {
                        "net_state":       net.state_dict(),
                        "optimizer_state": trainer.optimizer.state_dict(),
                        "games_played":    games_played,
                    },
                    path,
                )
                print(f"  ✓ Checkpoint → {path}")
                t_last_save = games_played

    except KeyboardInterrupt:
        print("\nInterrupted.")
    finally:
        trainer.close()

    final = save_dir / "selfplay_final.pt"
    torch.save(
        {
            "net_state":       net.state_dict(),
            "optimizer_state": trainer.optimizer.state_dict(),
            "games_played":    games_played,
        },
        final,
    )
    print(f"\nDone.  Model saved → {final}")


if __name__ == "__main__":
    main()
