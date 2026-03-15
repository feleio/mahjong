"""
PPO (Proximal Policy Optimisation) trainer for the Mahjong RL agent.

Because the game produces multiple decision points per episode (discard,
win, pong, …) and each has a *different* action head, this implementation
tracks each transition with its associated decision type and action mask.
The update step groups transitions by decision type and updates the
corresponding head independently, but shares the same encoder and value head.

References
──────────
  Schulman et al., 2017 — "Proximal Policy Optimization Algorithms"
  https://arxiv.org/abs/1707.06347
"""

import collections
import time
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim

from env import MahjongEnv, OBS_DIM
from model import MahjongNet

# ── Rollout buffer ─────────────────────────────────────────────────────────────

@dataclass
class Transition:
    obs:         np.ndarray       # (obs_dim,)
    decision:    str
    action:      int
    log_prob:    float
    value:       float
    reward:      float            # intermediate = 0.0; terminal = game reward
    done:        bool
    action_mask: Optional[np.ndarray]  # (n_actions,)


class RolloutBuffer:
    """
    Stores a fixed number of transitions (across multiple episodes).
    Call ``finish_episode()`` to fill in the final reward for all pending
    transitions of the completed game, then call ``get_batches()`` to
    retrieve mini-batches grouped by decision type.
    """

    def __init__(self, capacity: int, gamma: float = 0.99, gae_lambda: float = 0.95) -> None:
        self.capacity   = capacity
        self.gamma      = gamma
        self.gae_lambda = gae_lambda
        self._buf: List[Transition] = []
        self._ep_start: int = 0   # index where the current episode started

    def add(self, t: Transition) -> None:
        self._buf.append(t)

    def finish_episode(self, final_reward: float) -> None:
        """
        Assign the game-level reward to all transitions in the current episode
        and compute GAE advantages using a simple Monte-Carlo return
        (since every intermediate reward is 0, the return equals the terminal
        reward discounted by how many steps remain in the episode).
        """
        ep = self._buf[self._ep_start:]
        n  = len(ep)
        if n == 0:
            return

        # Compute discounted returns (terminal reward only model)
        returns = np.zeros(n, dtype=np.float32)
        returns[-1] = final_reward
        for i in range(n - 2, -1, -1):
            returns[i] = self.gamma * returns[i + 1]

        # GAE advantages
        values = np.array([t.value for t in ep], dtype=np.float32)
        next_vals = np.append(values[1:], final_reward)
        deltas = (returns - values +
                  self.gamma * next_vals * (1.0 - np.array([t.done for t in ep], dtype=np.float32)))
        advantages = np.zeros(n, dtype=np.float32)
        adv = 0.0
        for i in range(n - 1, -1, -1):
            adv = deltas[i] + self.gamma * self.gae_lambda * adv * (1.0 - ep[i].done)
            advantages[i] = adv

        # Patch transitions in-place with return and advantage
        for i, t in enumerate(ep):
            self._buf[self._ep_start + i] = Transition(
                obs=t.obs,
                decision=t.decision,
                action=t.action,
                log_prob=t.log_prob,
                value=t.value,
                reward=float(returns[i]),
                done=t.done,
                action_mask=t.action_mask,
            )
            # Store advantage as extra attribute (monkey-patch)
            self._buf[self._ep_start + i].advantage = float(advantages[i])  # type: ignore[attr-defined]

        self._ep_start = len(self._buf)

    def full(self) -> bool:
        return len(self._buf) >= self.capacity

    def clear(self) -> None:
        self._buf.clear()
        self._ep_start = 0

    def get_batches(
        self, batch_size: int, device: torch.device
    ) -> Dict[str, List[Dict[str, torch.Tensor]]]:
        """
        Returns a dict mapping decision type → list of mini-batch dicts.
        Each mini-batch dict contains tensors: obs, actions, old_log_probs,
        returns, advantages, action_masks.
        """
        # Group by decision
        by_decision: Dict[str, List[Transition]] = collections.defaultdict(list)
        for t in self._buf:
            if hasattr(t, "advantage"):
                by_decision[t.decision].append(t)

        result: Dict[str, List[Dict[str, torch.Tensor]]] = {}
        for dec, transitions in by_decision.items():
            # Normalise advantages within each decision type
            advs = np.array([t.advantage for t in transitions], dtype=np.float32)  # type: ignore[attr-defined]
            advs = (advs - advs.mean()) / (advs.std() + 1e-8)

            obs_arr       = np.stack([t.obs         for t in transitions])
            act_arr       = np.array([t.action      for t in transitions], dtype=np.int64)
            lp_arr        = np.array([t.log_prob    for t in transitions], dtype=np.float32)
            ret_arr       = np.array([t.reward      for t in transitions], dtype=np.float32)

            # Pad / stack action masks (some may be None)
            n_actions     = MahjongEnv.decision_spaces[dec]
            mask_arr      = np.ones((len(transitions), n_actions), dtype=bool)
            for i, t in enumerate(transitions):
                if t.action_mask is not None:
                    mask_arr[i] = t.action_mask

            # Shuffle
            idx = np.random.permutation(len(transitions))
            batches = []
            for start in range(0, len(idx), batch_size):
                end  = start + batch_size
                bidx = idx[start:end]
                batches.append({
                    "obs":          torch.tensor(obs_arr[bidx],  dtype=torch.float32, device=device),
                    "actions":      torch.tensor(act_arr[bidx],  dtype=torch.long,    device=device),
                    "old_log_probs":torch.tensor(lp_arr[bidx],   dtype=torch.float32, device=device),
                    "returns":      torch.tensor(ret_arr[bidx],  dtype=torch.float32, device=device),
                    "advantages":   torch.tensor(advs[bidx],     dtype=torch.float32, device=device),
                    "action_masks": torch.tensor(mask_arr[bidx], dtype=torch.bool,    device=device),
                })
            result[dec] = batches
        return result


# ── PPO trainer ────────────────────────────────────────────────────────────────

class PPOTrainer:
    """
    Parameters
    ----------
    net             : MahjongNet
    lr              : learning rate
    clip_eps        : PPO clip epsilon (ε)
    value_coef      : coefficient for value loss
    entropy_coef    : coefficient for entropy bonus
    max_grad_norm   : gradient clipping threshold
    n_epochs        : PPO update epochs per rollout
    batch_size      : mini-batch size per decision type
    rollout_steps   : transitions to collect before each update
    gamma           : discount factor
    gae_lambda      : GAE λ
    device          : torch device string
    """

    def __init__(
        self,
        net: MahjongNet,
        lr: float = 3e-4,
        clip_eps: float = 0.2,
        value_coef: float = 0.5,
        entropy_coef: float = 0.01,
        max_grad_norm: float = 0.5,
        n_epochs: int = 4,
        batch_size: int = 256,
        rollout_steps: int = 2048,
        gamma: float = 0.99,
        gae_lambda: float = 0.95,
        device: str = "cpu",
    ) -> None:
        self.net           = net
        self.clip_eps      = clip_eps
        self.value_coef    = value_coef
        self.entropy_coef  = entropy_coef
        self.max_grad_norm = max_grad_norm
        self.n_epochs      = n_epochs
        self.batch_size    = batch_size
        self.device        = torch.device(device)
        self.net.to(self.device)

        self.optimizer = optim.Adam(net.parameters(), lr=lr, eps=1e-5)
        self.buffer    = RolloutBuffer(rollout_steps, gamma, gae_lambda)

    # ── PPO update ─────────────────────────────────────────────────────────────

    def update(self) -> Dict[str, float]:
        """Run PPO update on the collected rollout. Returns loss statistics."""
        batches_by_dec = self.buffer.get_batches(self.batch_size, self.device)

        stats = collections.defaultdict(list)

        for _ in range(self.n_epochs):
            for dec, batches in batches_by_dec.items():
                for b in batches:
                    obs          = b["obs"]
                    actions      = b["actions"]
                    old_log_prob = b["old_log_probs"]
                    returns      = b["returns"]
                    advantages   = b["advantages"]
                    masks        = b["action_masks"]

                    # Forward pass
                    log_prob, entropy, value = self.net.evaluate_actions(
                        obs, dec, actions, masks
                    )

                    # Policy loss (clipped surrogate)
                    ratio       = torch.exp(log_prob - old_log_prob)
                    surr1       = ratio * advantages
                    surr2       = torch.clamp(ratio, 1.0 - self.clip_eps,
                                                     1.0 + self.clip_eps) * advantages
                    policy_loss = -torch.min(surr1, surr2).mean()

                    # Value loss (clipped)
                    value_loss  = F.mse_loss(value, returns)

                    # Entropy bonus (encourages exploration)
                    entropy_loss = -entropy.mean()

                    loss = (policy_loss
                            + self.value_coef  * value_loss
                            + self.entropy_coef * entropy_loss)

                    self.optimizer.zero_grad()
                    loss.backward()
                    nn.utils.clip_grad_norm_(self.net.parameters(), self.max_grad_norm)
                    self.optimizer.step()

                    stats["policy_loss"].append(policy_loss.item())
                    stats["value_loss"].append(value_loss.item())
                    stats["entropy"].append(-entropy_loss.item())
                    stats["loss"].append(loss.item())

        self.buffer.clear()
        return {k: float(np.mean(v)) for k, v in stats.items()}

    # ── Rollout collection ────────────────────────────────────────────────────

    def collect_episode(self, env: MahjongEnv, seed: Optional[int] = None) -> float:
        """
        Play one full game, recording transitions into the buffer.
        Returns the final game reward.
        """
        obs_np, info = env.reset(seed=seed)
        episode_reward = 0.0
        ep_transitions: List[Transition] = []

        while True:
            decision = info["decision"]
            mask_np  = info["action_mask"]

            obs_t = torch.tensor(obs_np,  dtype=torch.float32, device=self.device).unsqueeze(0)
            mask_t = (torch.tensor(mask_np, dtype=torch.bool, device=self.device).unsqueeze(0)
                      if mask_np is not None else None)

            with torch.no_grad():
                action_t, log_prob_t, value_t = self.net.act(obs_t, decision, mask_t)

            action   = int(action_t.item())
            log_prob = float(log_prob_t.item())
            value    = float(value_t.item())

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
            self.buffer.add(t)
            ep_transitions.append(t)

            if done:
                episode_reward = reward
                self.buffer.finish_episode(episode_reward)
                break

            obs_np, info = obs_np_next, info_next

        return episode_reward

    def collect_rollout(
        self, env: MahjongEnv, target_steps: int
    ) -> Tuple[List[float], int]:
        """
        Collect transitions until the buffer reaches ``target_steps``.
        Returns (list of episode rewards, number of episodes played).
        """
        rewards    = []
        n_episodes = 0
        seed       = 0
        while not self.buffer.full():
            r = self.collect_episode(env, seed=seed)
            rewards.append(r)
            n_episodes += 1
            seed += 1
        return rewards, n_episodes
