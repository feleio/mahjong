"""
self_play.py — True AlphaZero-style self-play training loop.

All four seats in every game are controlled by the SAME shared MahjongNet.
A single Scala JVM process runs the game with four RLPlayer instances (one per
seat), sending observations tagged with ``seat_id``.  Python reads each
observation, runs the shared network, and sends back the action.  At game end
the Scala server sends per-seat rewards for all four players.

AlphaZero ideas replicated here:
  ✓  True self-play — all 4 seats are the same evolving network
  ✓  Shared policy / value network across all seats
  ✓  Iterative improvement — the network always plays against itself
  ✓  Strategy analysis after training (see strategy_analysis.py)

Usage
─────
    python rl/self_play.py \\
        --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \\
        --total-games 500_000 \\
        --device cuda
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

sys.path.insert(0, str(Path(__file__).parent))

from env import MahjongEnv, OBS_DIM, DECISION_SPACES, encode_state, get_action_mask
from model import MahjongNet
from ppo import Transition


# ── Action encoding (mirrors MahjongEnv._action_to_json) ──────────────────────

def _action_to_json(decision: str, action: int) -> dict:
    if decision in ("win", "self_win", "pong", "kong"):
        return {"action": bool(action)}
    elif decision == "discard":
        return {"action": action}
    elif decision == "chow":
        pos_id = (action - 1) if action > 0 else None
        return {"action": pos_id}
    elif decision == "self_kong":
        tile_id = (action - 1) if action > 0 else None
        return {"action": tile_id}
    else:
        raise ValueError(f"Unknown decision type: {decision!r}")


# ── True self-play environment (single JVM, 4 RL seats) ───────────────────────

class TrueSelfPlayEnv:
    """
    Wraps a single Scala JVM process launched with ``-Drl.selfplay=true``.

    All four seats are RLPlayer instances.  The Scala server sends observations
    tagged with ``seat_id`` (0-3) sequentially as the game progresses.  Python
    reads each observation, infers an action for the correct seat using the
    shared network, and sends the action back.  At game end the server emits a
    ``game_over`` message with a ``rewards`` dict keyed by seat id string.
    """

    def __init__(self, jar_path: str, java_bin: str = "java") -> None:
        self._proc = subprocess.Popen(
            [
                java_bin,
                "-Dlogback.statusListenerClass=ch.qos.logback.core.status.NopStatusListener",
                "-Drl.selfplay=true",
                "-cp", jar_path,
                "io.fele.app.mahjong.rl.RLGymServer",
            ],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            bufsize=1,
            text=True,
        )

    def _send(self, msg: dict) -> None:
        self._proc.stdin.write(json.dumps(msg) + "\n")
        self._proc.stdin.flush()

    def _recv(self) -> dict:
        line = self._proc.stdout.readline()
        if not line:
            stderr = self._proc.stderr.read()
            raise RuntimeError(f"Scala server closed unexpectedly.\nstderr:\n{stderr}")
        return json.loads(line.strip())

    def reset(self, seed: Optional[int] = None) -> dict:
        cmd: dict = {"cmd": "reset"}
        if seed is not None:
            cmd["seed"] = seed
        self._send(cmd)
        return self._recv()

    def step(self, decision: str, action: int) -> dict:
        self._send(_action_to_json(decision, action))
        return self._recv()

    def close(self) -> None:
        try:
            self._proc.stdin.close()
            self._proc.wait(timeout=5)
        except Exception:
            self._proc.kill()

    def __enter__(self) -> "TrueSelfPlayEnv":
        return self

    def __exit__(self, *_) -> None:
        self.close()


# ── Self-play trainer ──────────────────────────────────────────────────────────

class SelfPlayTrainer:
    """
    True AlphaZero-inspired self-play trainer.

    A single shared MahjongNet plays all four seats in every game.  Experience
    from all seats is pooled and used to update the network after every
    ``games_per_update`` games.
    """

    def __init__(
        self,
        net: MahjongNet,
        jar_path: str,
        java_bin: str = "java",
        lr: float = 1e-4,
        clip_eps: float = 0.2,
        value_coef: float = 0.5,
        entropy_coef: float = 0.01,
        max_grad_norm: float = 0.5,
        n_epochs: int = 4,
        batch_size: int = 512,
        games_per_update: int = 128,
        hybrid_ratio: float = 0.3,
        gamma: float = 0.99,
        gae_lambda: float = 0.95,
        device: str = "cpu",
    ) -> None:
        self.net    = net
        self.device = torch.device(device)
        self.net.to(self.device)

        self.games_per_update = games_per_update
        self.hybrid_ratio     = max(0.0, min(1.0, hybrid_ratio))
        self.n_epochs         = n_epochs
        self.batch_size       = batch_size
        self.clip_eps         = clip_eps
        self.value_coef       = value_coef
        self.entropy_coef     = entropy_coef
        self.max_grad_norm    = max_grad_norm
        self.gamma            = gamma
        self.gae_lambda       = gae_lambda

        self.optimizer   = torch.optim.Adam(net.parameters(), lr=lr, eps=1e-5)
        self.sp_env      = TrueSelfPlayEnv(jar_path, java_bin)
        self.chicken_env = MahjongEnv(jar_path, java_bin, opponent="chicken") if hybrid_ratio > 0 else None

        # Running statistics for strategy analysis
        self.action_type_counts:  Dict[str, int] = collections.defaultdict(int)
        self.discard_tile_counts: Dict[int, int]  = collections.defaultdict(int)
        self.win_score_counts:    Dict[int, int]  = collections.defaultdict(int)
        self.total_games = 0

    # ── Single-game rollout ────────────────────────────────────────────────────

    def _play_game(
        self, seed: int
    ) -> Tuple[Dict[int, List[Transition]], Dict[int, float]]:
        """
        Play one complete game with all 4 seats as RL players.

        Returns:
            seat_transitions  – per-seat list of Transition objects
            rewards           – per-seat final reward  {seat_id: float}
        """
        msg = self.sp_env.reset(seed=seed)
        seat_transitions: Dict[int, List[Transition]] = {i: [] for i in range(4)}

        while True:
            if msg["type"] == "game_over":
                rewards_raw = msg["rewards"]          # {"0": 5.0, "1": -2.5, …}
                rewards = {int(k): float(v) for k, v in rewards_raw.items()}
                # Track win statistics
                for wid in msg.get("winner_ids", []):
                    self.win_score_counts[wid] += 1
                self.total_games += 1
                return seat_transitions, rewards

            assert msg["type"] == "observation", f"Unexpected message: {msg}"

            seat_id  = int(msg["seat_id"])
            decision = msg["decision"]
            context  = msg.get("context", {})
            state    = msg["state"]

            obs  = encode_state(state)
            mask = get_action_mask(decision, context)

            obs_t  = torch.tensor(obs,  dtype=torch.float32, device=self.device).unsqueeze(0)
            mask_t = (
                torch.tensor(mask, dtype=torch.bool, device=self.device).unsqueeze(0)
                if mask is not None else None
            )

            with torch.no_grad():
                action_t, log_prob_t, value_t = self.net.act(obs_t, decision, mask_t)

            action = int(action_t.item())

            self.action_type_counts[decision] += 1
            if decision == "discard":
                self.discard_tile_counts[action] += 1

            seat_transitions[seat_id].append(Transition(
                obs=obs,
                decision=decision,
                action=action,
                log_prob=float(log_prob_t.item()),
                value=float(value_t.item()),
                reward=0.0,   # intermediate reward; final reward set at game end
                done=False,
                action_mask=mask,
            ))

            msg = self.sp_env.step(decision, action)

    # ── Chicken game (hybrid signal) ───────────────────────────────────────────

    def _play_chicken_game(self, seed: int) -> Tuple[Dict[int, List[Transition]], Dict[int, float]]:
        """
        Play one game as seat 0 vs 3 Chicken bots.
        Returns data in the same format as _play_game so _ppo_update can
        consume both interchangeably.
        """
        assert self.chicken_env is not None
        obs_np, info = self.chicken_env.reset(seed=seed)
        transitions: List[Transition] = []

        while True:
            decision = info["decision"]
            mask     = info["action_mask"]

            obs_t  = torch.tensor(obs_np, dtype=torch.float32, device=self.device).unsqueeze(0)
            mask_t = (torch.tensor(mask, dtype=torch.bool, device=self.device).unsqueeze(0)
                      if mask is not None else None)

            with torch.no_grad():
                action_t, log_prob_t, value_t = self.net.act(obs_t, decision, mask_t)

            action = int(action_t.item())
            self.action_type_counts[decision] += 1
            if decision == "discard":
                self.discard_tile_counts[action] += 1

            obs_np_next, _, done, info_next = self.chicken_env.step(action)

            transitions.append(Transition(
                obs=obs_np, decision=decision, action=action,
                log_prob=float(log_prob_t.item()), value=float(value_t.item()),
                reward=0.0, done=done, action_mask=mask,
            ))

            if done:
                final_reward = float(info_next.get("reward", 0.0))
                if final_reward > 0:
                    self.total_games += 1
                # Wrap as seat 0 only; seats 1-3 are Chicken (not trained)
                seat_transitions = {0: transitions, 1: [], 2: [], 3: []}
                rewards          = {0: final_reward, 1: 0.0, 2: 0.0, 3: 0.0}
                return seat_transitions, rewards

            obs_np, info = obs_np_next, info_next

    # ── PPO update ─────────────────────────────────────────────────────────────

    def _ppo_update(
        self,
        all_game_data: List[Tuple[Dict[int, List[Transition]], Dict[int, float]]],
    ) -> Dict[str, float]:
        """Run a PPO update over all collected transitions from all seats."""
        by_decision: Dict[str, List[dict]] = collections.defaultdict(list)

        for seat_transitions, rewards in all_game_data:
            for seat_id, transitions in seat_transitions.items():
                if not transitions:
                    continue
                final_reward = rewards.get(seat_id, 0.0)
                n = len(transitions)

                # Monte-Carlo returns discounted from game end
                returns = np.zeros(n, dtype=np.float32)
                returns[-1] = final_reward
                for i in range(n - 2, -1, -1):
                    returns[i] = self.gamma * returns[i + 1]

                # GAE advantages
                values  = np.array([t.value for t in transitions], dtype=np.float32)
                next_v  = np.append(values[1:], final_reward)
                deltas  = returns - values + self.gamma * next_v
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

        stats: Dict[str, list] = collections.defaultdict(list)
        for _ in range(self.n_epochs):
            for dec, samples in by_decision.items():
                advs = np.array([s["advantage"] for s in samples], dtype=np.float32)
                advs = (advs - advs.mean()) / (advs.std() + 1e-8)

                obs_arr  = np.stack([s["obs"]         for s in samples])
                act_arr  = np.array([s["action"]       for s in samples], dtype=np.int64)
                lp_arr   = np.array([s["old_log_prob"] for s in samples], dtype=np.float32)
                ret_arr  = np.array([s["return_"]      for s in samples], dtype=np.float32)
                mask_arr = np.stack([s["mask"]         for s in samples])

                idx = np.random.permutation(len(samples))
                for start in range(0, len(idx), self.batch_size):
                    bidx = idx[start: start + self.batch_size]

                    obs_t  = torch.tensor(obs_arr[bidx],  dtype=torch.float32, device=self.device)
                    act_t  = torch.tensor(act_arr[bidx],  dtype=torch.long,    device=self.device)
                    lp_t   = torch.tensor(lp_arr[bidx],   dtype=torch.float32, device=self.device)
                    ret_t  = torch.tensor(ret_arr[bidx],  dtype=torch.float32, device=self.device)
                    adv_t  = torch.tensor(advs[bidx],     dtype=torch.float32, device=self.device)
                    mask_t = torch.tensor(mask_arr[bidx], dtype=torch.bool,    device=self.device)

                    log_prob, entropy, value = self.net.evaluate_actions(obs_t, dec, act_t, mask_t)
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
        Play ``games_per_update`` games then update.
        ``hybrid_ratio`` fraction are vs Chicken (keeps win signal alive);
        the rest are true self-play (refines strategy against itself).
        """
        n_chicken   = round(self.games_per_update * self.hybrid_ratio)
        n_self_play = self.games_per_update - n_chicken

        all_game_data: List[Tuple[Dict[int, List[Transition]], Dict[int, float]]] = []
        ep_rewards: List[float] = []

        for game_i in range(n_self_play):
            seat_transitions, rewards = self._play_game(base_seed + game_i)
            all_game_data.append((seat_transitions, rewards))
            ep_rewards.extend(rewards.values())   # 4 rewards per self-play game

        for game_i in range(n_chicken):
            seat_transitions, rewards = self._play_chicken_game(base_seed + n_self_play + game_i)
            all_game_data.append((seat_transitions, rewards))
            ep_rewards.append(rewards[0])         # only seat 0 played

        loss_stats = self._ppo_update(all_game_data)

        return {
            **loss_stats,
            "mean_reward": float(np.mean(ep_rewards)),
            "win_rate":    float(np.mean([r > 0 for r in ep_rewards])),
        }

    def close(self) -> None:
        self.sp_env.close()
        if self.chicken_env is not None:
            self.chicken_env.close()


# ── CLI ────────────────────────────────────────────────────────────────────────

def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="True AlphaZero-style Mahjong self-play trainer")
    p.add_argument("--jar",              type=str,   required=True)
    p.add_argument("--java",             type=str,   default="java")
    p.add_argument("--total-games",      type=int,   default=500_000)
    p.add_argument("--games-per-update", type=int,   default=128,
                   help="Actual games to play before each PPO update")
    p.add_argument("--hybrid-ratio",     type=float, default=0.3,
                   help="Fraction of games played vs Chicken (keeps win signal alive)")
    p.add_argument("--lr",               type=float, default=1e-4)
    p.add_argument("--clip-eps",         type=float, default=0.2)
    p.add_argument("--entropy-coef",     type=float, default=0.01)
    p.add_argument("--n-epochs",         type=int,   default=4)
    p.add_argument("--batch-size",       type=int,   default=512)
    p.add_argument("--hidden-size",      type=int,   default=512)
    p.add_argument("--checkpoint",       type=str,   default=None)
    p.add_argument("--save-dir",         type=str,   default="rl/checkpoints/selfplay")
    p.add_argument("--save-every",       type=int,   default=10_000,
                   help="Save checkpoint every N games")
    p.add_argument("--log-every",        type=int,   default=1_000,
                   help="Print stats every N games")
    p.add_argument("--device",           type=str,   default="cpu")
    p.add_argument("--seed",             type=int,   default=0)
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
        hybrid_ratio=args.hybrid_ratio,
        device=args.device,
    )

    games_played = 0
    t_last_log   = 0
    t_last_save  = 0
    t_start      = time.time()

    if args.checkpoint:
        ckpt = torch.load(args.checkpoint, map_location=args.device)
        net.load_state_dict(ckpt["net_state"])
        trainer.optimizer.load_state_dict(ckpt["optimizer_state"])
        games_played = ckpt.get("games_played", 0)
        print(f"Resumed from {args.checkpoint}  (games: {games_played:,})")

    n_sp  = round(args.games_per_update * (1 - args.hybrid_ratio))
    n_ch  = args.games_per_update - n_sp
    print(f"Hybrid self-play training for {args.total_games:,} games …")
    print(f"Per update: {n_sp} self-play games + {n_ch} vs Chicken  |  lr={args.lr}  |  entropy={args.entropy_coef}")
    print(f"Score bonus: 5pt +20%, 7pt +50%, 8pt +100%")
    print("-" * 60)

    try:
        while games_played < args.total_games:
            stats = trainer.train_iteration(base_seed=games_played)
            games_played += args.games_per_update   # actual games played

            if games_played - t_last_log >= args.log_every:
                elapsed = time.time() - t_start
                gps     = games_played / max(elapsed, 1)
                print(
                    f"games={games_played:>8,}  "
                    f"reward={stats['mean_reward']:+.2f}  "
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
