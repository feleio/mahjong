"""
train.py — Main training script for the Mahjong RL agent.

Usage
─────
    # From the project root, after ``sbt assembly``:
    python rl/train.py \\
        --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \\
        --total-steps 2_000_000 \\
        --rollout-steps 2048 \\
        --device cpu

    # Resume from checkpoint:
    python rl/train.py \\
        --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \\
        --checkpoint rl/checkpoints/step_1000000.pt

All hyper-parameters are exposed as CLI flags (see ``--help``).
"""

import argparse
import os
import sys
import time
from pathlib import Path
from typing import List

import numpy as np
import torch

# Make sure the rl/ directory is importable
sys.path.insert(0, str(Path(__file__).parent))

from env import MahjongEnv
from model import MahjongNet
from ppo import PPOTrainer
from vec_env import VecMahjongEnv


# ── CLI ───────────────────────────────────────────────────────────────────────

def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(
        description="Train a PPO agent to play Hong Kong Mahjong"
    )

    # Environment
    p.add_argument("--jar", type=str, required=True,
                   help="Path to the assembled Scala fat-JAR")
    p.add_argument("--java", type=str, default="java",
                   help="java executable (default: java)")
    p.add_argument("--n-envs", type=int, default=1,
                   help="Number of parallel environments (>1 uses VecMahjongEnv)")
    p.add_argument("--opponent", type=str, default="chicken",
                   choices=["chicken", "firstfelix", "mixed"],
                   help="Opponent type: chicken, firstfelix, or mixed (50/50 each game)")

    # Training budget
    p.add_argument("--total-steps", type=int, default=2_000_000,
                   help="Total environment steps to train for")
    p.add_argument("--rollout-steps", type=int, default=2048,
                   help="Transitions to collect per PPO update")

    # PPO hyper-parameters
    p.add_argument("--lr",            type=float, default=3e-4)
    p.add_argument("--clip-eps",      type=float, default=0.2)
    p.add_argument("--value-coef",    type=float, default=0.5)
    p.add_argument("--entropy-coef",  type=float, default=0.01)
    p.add_argument("--max-grad-norm", type=float, default=0.5)
    p.add_argument("--n-epochs",      type=int,   default=4)
    p.add_argument("--batch-size",    type=int,   default=256)
    p.add_argument("--gamma",         type=float, default=0.99)
    p.add_argument("--gae-lambda",    type=float, default=0.95)
    p.add_argument("--hidden-size",   type=int,   default=512)

    # I/O
    p.add_argument("--checkpoint",  type=str, default=None,
                   help="Path to a .pt checkpoint to resume from")
    p.add_argument("--save-dir",    type=str, default="rl/checkpoints",
                   help="Directory to write checkpoints to")
    p.add_argument("--save-every",  type=int, default=100_000,
                   help="Save a checkpoint every N environment steps")
    p.add_argument("--log-every",   type=int, default=10_000,
                   help="Print stats every N environment steps")

    # Misc
    p.add_argument("--device", type=str, default="cpu",
                   help="torch device (cpu / cuda / mps)")
    p.add_argument("--seed", type=int, default=42)

    return p.parse_args()


# ── Training loop ─────────────────────────────────────────────────────────────

class RunningStats:
    """Lightweight exponential-moving-average tracker."""
    def __init__(self, alpha: float = 0.05) -> None:
        self.alpha = alpha
        self._vals: dict = {}

    def update(self, **kwargs: float) -> None:
        for k, v in kwargs.items():
            if k not in self._vals:
                self._vals[k] = v
            else:
                self._vals[k] = (1 - self.alpha) * self._vals[k] + self.alpha * v

    def __getitem__(self, k: str) -> float:
        return self._vals.get(k, float("nan"))

    def __repr__(self) -> str:
        return "  ".join(f"{k}={v:.4f}" for k, v in self._vals.items())


def train(args: argparse.Namespace) -> None:
    torch.manual_seed(args.seed)
    np.random.seed(args.seed)

    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    # ── Build network and trainer ─────────────────────────────────────────────
    # Peek at checkpoint to get architecture if provided
    if args.checkpoint:
        _ckpt_peek = torch.load(args.checkpoint, map_location="cpu", weights_only=False)
        _hidden = _ckpt_peek.get("hidden_size", args.hidden_size)
        _layers = _ckpt_peek.get("n_layers", 2)
        del _ckpt_peek
    else:
        _hidden, _layers = args.hidden_size, 2
    net = MahjongNet(hidden_size=_hidden, n_layers=_layers)
    trainer = PPOTrainer(
        net=net,
        lr=args.lr,
        clip_eps=args.clip_eps,
        value_coef=args.value_coef,
        entropy_coef=args.entropy_coef,
        max_grad_norm=args.max_grad_norm,
        n_epochs=args.n_epochs,
        batch_size=args.batch_size,
        rollout_steps=args.rollout_steps,
        gamma=args.gamma,
        gae_lambda=args.gae_lambda,
        device=args.device,
    )

    # ── Resume from checkpoint ────────────────────────────────────────────────
    total_steps   = 0
    total_episodes = 0

    if args.checkpoint:
        print(f"Loading checkpoint: {args.checkpoint}")
        ckpt = torch.load(args.checkpoint, map_location=args.device)
        net.load_state_dict(ckpt["net_state"])
        if "optimizer_state" in ckpt:
            trainer.optimizer.load_state_dict(ckpt["optimizer_state"])
        total_steps    = ckpt.get("total_steps", 0)
        total_episodes = ckpt.get("total_episodes", 0)
        print(f"Resumed at step {total_steps:,}")

    # ── Open environment ──────────────────────────────────────────────────────
    use_vec = args.n_envs > 1
    if use_vec:
        print(f"Starting {args.n_envs} parallel Scala game servers: {args.jar}")
        env = VecMahjongEnv(
            jar_path=args.jar, n_envs=args.n_envs,
            java_bin=args.java, opponent=args.opponent,
        )
    else:
        print(f"Starting Scala game server: {args.jar}")
        env = MahjongEnv(jar_path=args.jar, java_bin=args.java, opponent=args.opponent)

    stats  = RunningStats()
    t_last_log  = total_steps
    t_last_save = total_steps
    t_start     = time.time()

    print(f"Training for {args.total_steps:,} total steps …")
    print(f"Rollout steps: {args.rollout_steps}  |  PPO epochs: {args.n_epochs}"
          + (f"  |  n_envs: {args.n_envs}" if use_vec else ""))
    print("-" * 60)

    try:
        while total_steps < args.total_steps:
            # ── Collect rollout ───────────────────────────────────────────────
            if use_vec:
                ep_rewards, n_eps = trainer.collect_rollout_vec(env, args.rollout_steps)
            else:
                ep_rewards, n_eps = trainer.collect_rollout(env, args.rollout_steps)
            rollout_steps      = len(trainer.buffer._buf)  # actual collected
            total_steps       += rollout_steps
            total_episodes    += n_eps

            # ── PPO update ────────────────────────────────────────────────────
            loss_stats = trainer.update()

            # ── Logging ───────────────────────────────────────────────────────
            # money = per-game balance: +ve = won $, -ve = paid $
            mean_money  = float(np.mean(ep_rewards)) if ep_rewards else float("nan")
            win_money   = [r for r in ep_rewards if r > 0]
            avg_win_pay = float(np.mean(win_money)) if win_money else 0.0
            stats.update(
                money=mean_money,
                avg_win_pay=avg_win_pay,
                **loss_stats,
            )

            if total_steps - t_last_log >= args.log_every:
                elapsed = time.time() - t_start
                sps     = total_steps / max(elapsed, 1)
                print(
                    f"step={total_steps:>8,}  eps={total_episodes:>7,}  "
                    f"money={stats['money']:+.1f}  "
                    f"avg_win_pay={stats['avg_win_pay']:+.1f}  "
                    f"loss={stats['loss']:.4f}  "
                    f"entropy={stats['entropy']:.4f}  "
                    f"sps={sps:.0f}"
                )
                t_last_log = total_steps

            # ── Checkpoint ────────────────────────────────────────────────────
            if total_steps - t_last_save >= args.save_every:
                ckpt_path = save_dir / f"step_{total_steps}.pt"
                torch.save(
                    {
                        "net_state":       net.state_dict(),
                        "optimizer_state": trainer.optimizer.state_dict(),
                        "total_steps":     total_steps,
                        "total_episodes":  total_episodes,
                        "hidden_size":     net.obs_dim and _hidden,
                        "n_layers":        _layers,
                    },
                    ckpt_path,
                )
                print(f"  ✓ Saved checkpoint → {ckpt_path}")
                t_last_save = total_steps

    except KeyboardInterrupt:
        print("\nTraining interrupted by user.")

    finally:
        env.close()

    # ── Final checkpoint ──────────────────────────────────────────────────────
    final_path = save_dir / "final.pt"
    torch.save(
        {
            "net_state":       net.state_dict(),
            "optimizer_state": trainer.optimizer.state_dict(),
            "total_steps":     total_steps,
            "total_episodes":  total_episodes,
            "hidden_size":     _hidden,
            "n_layers":        _layers,
        },
        final_path,
    )
    print(f"\nTraining complete.  Final model → {final_path}")
    print(f"Total steps: {total_steps:,}  |  Total episodes: {total_episodes:,}")


# ── Entry point ───────────────────────────────────────────────────────────────

if __name__ == "__main__":
    train(parse_args())
