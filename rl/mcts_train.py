"""
mcts_train.py — AlphaZero-style training with imperfect-information MCTS.

Training loop
─────────────
1. Data collection (MCTS-augmented games):
   - RL player (seat 0) uses MCTS for discard decisions, NN for all others.
   - 3 opponents are FirstFelix (strong) or Chicken (fast).
   - At each discard decision, MCTS evaluates every valid tile via MC rollouts
     and produces a policy distribution π_mcts.
   - Records (obs, decision, action, π_mcts, nn_value) for every step.

2. Training (AlphaZero objective):
   - Discard steps:
       policy_loss = -sum(π_mcts * log π_net)   [cross-entropy, MCTS → NN]
       value_loss  = (v_net - R_final)^2
   - Non-discard steps:
       value_loss  = (v_net - R_final)^2         [value only; NN policy stays]
   - entropy_loss = -H(π_net)                   [small weight, prevents collapse]

Usage
─────
    python rl/mcts_train.py \\
        --jar   target/scala-2.12/mahjong-assembly-0.1.0.jar \\
        --checkpoint rl/checkpoints/mixed_5e6/step_2731542.pt \\
        --device cuda \\
        --n-rollouts 8 \\
        --total-games 200_000

Key flags
─────────
    --n-rollouts     MC rollouts per candidate tile (default 8)
    --rollout-opp    "chicken" (fast) or "firstfelix" (stronger signal)
    --temperature    τ for Boltzmann action selection from MCTS Q (default 1.0)
    --opponent       "firstfelix" | "chicken" | "mixed" for actual training games
    --games-per-update  games collected before each NN update (default 64)
"""

import argparse
import collections
import json
import subprocess
import sys
import time
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F

sys.path.insert(0, str(Path(__file__).parent))

from env import MahjongEnv, OBS_DIM, DECISION_SPACES, encode_state, get_action_mask
from model import MahjongNet
from mcts import MCTSRolloutClient, ImperfectInfoMCTS


# ── Data record ───────────────────────────────────────────────────────────────

MCTSStep = collections.namedtuple(
    "MCTSStep",
    ["obs", "decision", "action", "mcts_policy", "nn_value", "action_mask"],
)


# ── Game collector ────────────────────────────────────────────────────────────

class MCTSGameCollector:
    """
    Plays one game of RL-agent-with-MCTS vs. heuristic opponents.

    All decisions go through `ImperfectInfoMCTS.get_action_and_policy`.
    For discard decisions this triggers MC rollouts; for binary decisions
    it falls through to the neural network.
    """

    def __init__(
        self,
        jar_path: str,
        mcts: ImperfectInfoMCTS,
        opponent: str = "firstfelix",
        java_bin: str = "java",
    ) -> None:
        self.mcts = mcts
        self.env  = MahjongEnv(jar_path, java_bin, opponent=opponent)

    def collect_game(self, seed: Optional[int] = None) -> Tuple[List[MCTSStep], float]:
        """
        Play one complete game.

        Returns
        -------
        steps       : list of MCTSStep named-tuples (one per decision)
        final_reward: final game balance for the RL agent
        """
        obs, info = self.env.reset(seed=seed)
        steps: List[MCTSStep] = []

        while True:
            decision    = info["decision"]
            context     = info["context"]
            action_mask = info["action_mask"]
            state       = info["state"]

            action, mcts_policy, nn_value = self.mcts.get_action_and_policy(
                obs, state, decision, context, action_mask
            )

            steps.append(MCTSStep(
                obs         = obs.copy(),
                decision    = decision,
                action      = action,
                mcts_policy = mcts_policy,
                nn_value    = nn_value,
                action_mask = action_mask,
            ))

            obs, step_reward, done, info = self.env.step(action)

            if done:
                return steps, float(step_reward)

    def close(self) -> None:
        self.env.close()


# ── AlphaZero-style NN updater ────────────────────────────────────────────────

class MCTSTrainer:
    """
    Updates MahjongNet using AlphaZero objectives:

    For discard decisions (where we have MCTS policy targets):
        L_policy = -Σ π_mcts(a) · log π_net(a)   (cross-entropy)
        L_value  = (v_net - R)²                   (MSE vs game outcome)

    For non-discard decisions:
        L_value  = (v_net - R)²                   (value only)

    Entropy regularisation (small weight) prevents policy collapse.
    """

    def __init__(
        self,
        net: MahjongNet,
        lr: float = 1e-4,
        policy_coef: float = 1.0,
        value_coef: float = 0.5,
        entropy_coef: float = 0.005,
        max_grad_norm: float = 0.5,
        batch_size: int = 256,
        n_epochs: int = 3,
        device: str = "cpu",
    ) -> None:
        self.net          = net
        self.device       = torch.device(device)
        self.policy_coef  = policy_coef
        self.value_coef   = value_coef
        self.entropy_coef = entropy_coef
        self.max_grad_norm = max_grad_norm
        self.batch_size   = batch_size
        self.n_epochs     = n_epochs
        self.optimizer    = torch.optim.Adam(net.parameters(), lr=lr, eps=1e-5)

    def update(
        self,
        all_steps: List[MCTSStep],
        all_rewards: List[float],
    ) -> Dict[str, float]:
        """
        Run one round of gradient updates over collected experience.

        Parameters
        ----------
        all_steps   : flat list of MCTSStep from all games in the batch
        all_rewards : final reward for each step (same ordering as all_steps;
                      each step's reward = the final reward of its game)

        Returns
        -------
        dict of mean losses for logging
        """
        # Organise by decision type
        by_dec: Dict[str, list] = collections.defaultdict(list)
        for step, reward in zip(all_steps, all_rewards):
            by_dec[step.decision].append({
                "obs":         step.obs,
                "action":      step.action,
                "mcts_policy": step.mcts_policy,
                "nn_value":    step.nn_value,
                "reward":      reward,
                "mask":        step.action_mask,
            })

        stats: Dict[str, list] = collections.defaultdict(list)

        for epoch in range(self.n_epochs):
            for dec, samples in by_dec.items():
                obs_arr  = np.stack([s["obs"]     for s in samples])
                act_arr  = np.array([s["action"]  for s in samples], dtype=np.int64)
                ret_arr  = np.array([s["reward"]  for s in samples], dtype=np.float32)
                mask_arr = np.stack([s["mask"]    for s in samples]) if samples[0]["mask"] is not None else None
                is_discard = (dec == "discard")
                if is_discard:
                    pol_arr = np.stack([s["mcts_policy"] for s in samples])

                idx = np.random.permutation(len(samples))
                for start in range(0, len(idx), self.batch_size):
                    bidx = idx[start: start + self.batch_size]

                    obs_t  = torch.tensor(obs_arr[bidx],  dtype=torch.float32, device=self.device)
                    ret_t  = torch.tensor(ret_arr[bidx],  dtype=torch.float32, device=self.device)
                    mask_t = (
                        torch.tensor(mask_arr[bidx], dtype=torch.bool, device=self.device)
                        if mask_arr is not None else None
                    )

                    logits, value = self.net.forward(obs_t, dec, mask_t)
                    dist          = torch.distributions.Categorical(logits=logits)
                    entropy       = dist.entropy().mean()

                    # Value loss (all decisions)
                    v_loss = F.mse_loss(value, ret_t)

                    if is_discard:
                        # Cross-entropy against MCTS policy target
                        pi_mcts = torch.tensor(pol_arr[bidx], dtype=torch.float32, device=self.device)
                        log_pi  = F.log_softmax(logits, dim=-1)
                        # Avoid 0 * -inf = nan: clamp log to large finite negative
                        log_pi_safe = log_pi.clamp(min=-20.0)
                        p_loss  = -(pi_mcts * log_pi_safe).sum(dim=-1).mean()
                    else:
                        # Behavioural cloning on NN's own argmax (no gradient wasted)
                        act_t  = torch.tensor(act_arr[bidx], dtype=torch.long, device=self.device)
                        p_loss = F.cross_entropy(logits, act_t)

                    loss = (self.policy_coef  * p_loss
                            + self.value_coef  * v_loss
                            - self.entropy_coef * entropy)

                    self.optimizer.zero_grad()
                    loss.backward()
                    nn.utils.clip_grad_norm_(self.net.parameters(), self.max_grad_norm)
                    self.optimizer.step()

                    stats["policy_loss"].append(p_loss.item())
                    stats["value_loss"].append(v_loss.item())
                    stats["entropy"].append(entropy.item())
                    stats["total_loss"].append(loss.item())

        return {k: float(np.mean(v)) for k, v in stats.items()}


# ── Main training loop ────────────────────────────────────────────────────────

def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="AlphaZero-style MCTS training for Mahjong")
    p.add_argument("--jar",              required=True,  type=str)
    p.add_argument("--java",             default="java", type=str)
    p.add_argument("--checkpoint",       default=None,   type=str, help="Resume from checkpoint")
    p.add_argument("--save-dir",         default="rl/checkpoints/mcts", type=str)
    p.add_argument("--total-games",      default=200_000, type=int)
    p.add_argument("--games-per-update", default=64,    type=int)
    p.add_argument("--n-rollouts",       default=8,     type=int,  help="MC rollouts per tile")
    p.add_argument("--rollout-opp",      default="chicken", type=str,
                   choices=["chicken", "firstfelix"],
                   help="Opponent policy used in MCTS rollouts")
    p.add_argument("--opponent",         default="firstfelix", type=str,
                   choices=["chicken", "firstfelix", "mixed"],
                   help="Opponent in actual training games")
    p.add_argument("--temperature",      default=1.0,   type=float, help="MCTS Boltzmann τ")
    p.add_argument("--value-weight",     default=0.5,   type=float,
                   help="Blend factor for MCTS Q vs NN value (0=pure NN, 1=pure MCTS)")
    p.add_argument("--lr",               default=1e-4,  type=float)
    p.add_argument("--entropy-coef",     default=0.005, type=float)
    p.add_argument("--value-coef",       default=0.5,   type=float)
    p.add_argument("--n-epochs",         default=3,     type=int)
    p.add_argument("--batch-size",       default=256,   type=int)
    p.add_argument("--hidden-size",      default=512,   type=int)
    p.add_argument("--save-every",       default=5_000, type=int)
    p.add_argument("--log-every",        default=500,   type=int)
    p.add_argument("--device",           default="cpu", type=str)
    p.add_argument("--seed",             default=0,     type=int)
    return p.parse_args()


def main() -> None:
    args = parse_args()
    torch.manual_seed(args.seed)
    np.random.seed(args.seed)

    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    # ── Model ──────────────────────────────────────────────────────────────────
    net = MahjongNet(hidden_size=args.hidden_size)
    games_played = 0

    if args.checkpoint:
        ckpt = torch.load(args.checkpoint, map_location=args.device, weights_only=False)
        net.load_state_dict(ckpt["net_state"])
        games_played = ckpt.get("games_played", ckpt.get("steps", 0))
        print(f"Resumed from {args.checkpoint}  (games: {games_played:,})")

    net.to(torch.device(args.device))

    # ── MCTS + rollout client ──────────────────────────────────────────────────
    rollout_client = MCTSRolloutClient(
        jar_path    = args.jar,
        n_rollouts  = args.n_rollouts,
        rollout_opp = args.rollout_opp,
        java_bin    = args.java,
    )
    mcts = ImperfectInfoMCTS(
        net           = net,
        rollout_client = rollout_client,
        temperature   = args.temperature,
        value_weight  = args.value_weight,
        device        = args.device,
    )

    # ── Game collector + trainer ───────────────────────────────────────────────
    collector = MCTSGameCollector(
        jar_path  = args.jar,
        mcts      = mcts,
        opponent  = args.opponent,
        java_bin  = args.java,
    )
    trainer = MCTSTrainer(
        net          = net,
        lr           = args.lr,
        value_coef   = args.value_coef,
        entropy_coef = args.entropy_coef,
        n_epochs     = args.n_epochs,
        batch_size   = args.batch_size,
        device       = args.device,
    )

    # EMA tracking
    ema_reward  = None
    ema_alpha   = 0.05
    t_last_log  = 0
    t_last_save = 0
    t_start     = time.time()
    win_count   = 0
    game_count  = 0

    print(f"MCTS training | total_games={args.total_games:,} | "
          f"n_rollouts={args.n_rollouts} | rollout_opp={args.rollout_opp} | "
          f"opponent={args.opponent} | τ={args.temperature}")
    print("-" * 70)

    try:
        while games_played < args.total_games:
            # Collect a batch of games
            all_steps:   List[MCTSStep] = []
            all_rewards: List[float]    = []
            batch_rewards: List[float]  = []

            for _ in range(args.games_per_update):
                steps, reward = collector.collect_game(seed=games_played)
                # Assign each step the game's final reward
                all_steps.extend(steps)
                all_rewards.extend([reward] * len(steps))
                batch_rewards.append(reward)
                games_played += 1
                game_count   += 1
                if reward > 0:
                    win_count += 1

            # Update EMA
            batch_mean = float(np.mean(batch_rewards))
            ema_reward = batch_mean if ema_reward is None else (
                ema_alpha * batch_mean + (1 - ema_alpha) * ema_reward
            )

            # Train
            loss_stats = trainer.update(all_steps, all_rewards)

            # Log
            if games_played - t_last_log >= args.log_every:
                elapsed  = time.time() - t_start
                gps      = games_played / max(elapsed, 1)
                win_rate = win_count / max(game_count, 1)
                n_disc   = sum(1 for s in all_steps if s.decision == "discard")
                print(
                    f"games={games_played:>8,}  "
                    f"ema_reward={ema_reward:+.2f}  "
                    f"win%={win_rate:.1%}  "
                    f"p_loss={loss_stats.get('policy_loss', 0):.4f}  "
                    f"v_loss={loss_stats.get('value_loss', 0):.4f}  "
                    f"entropy={loss_stats.get('entropy', 0):.4f}  "
                    f"discard_steps={n_disc}  "
                    f"gps={gps:.1f}"
                )
                t_last_log = games_played

            # Save checkpoint
            if games_played - t_last_save >= args.save_every:
                path = save_dir / f"mcts_games_{games_played}.pt"
                torch.save({
                    "net_state":    net.state_dict(),
                    "optimizer":    trainer.optimizer.state_dict(),
                    "games_played": games_played,
                    "ema_reward":   ema_reward,
                }, path)
                print(f"  ✓ Checkpoint → {path}")
                t_last_save = games_played

    except KeyboardInterrupt:
        print("\nInterrupted.")
    finally:
        collector.close()
        rollout_client.close()

    final = save_dir / "mcts_final.pt"
    torch.save({
        "net_state":    net.state_dict(),
        "optimizer":    trainer.optimizer.state_dict(),
        "games_played": games_played,
        "ema_reward":   ema_reward,
    }, final)
    print(f"\nDone.  Model saved → {final}")


if __name__ == "__main__":
    main()
