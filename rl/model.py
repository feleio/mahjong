"""
MahjongNet — Multi-head actor-critic network for Hong Kong Mahjong.

Architecture
────────────
  Shared encoder  : Linear(OBS_DIM→512) → LayerNorm → ReLU
                    Linear(512→256)      → LayerNorm → ReLU

  Value head      : Linear(256→1)                          (critic)

  Policy heads (actor) — one per decision type:
    discard_head   : Linear(256→34)    tile ID 0-33
    binary_head    : Linear(256→2)     pass / accept (win/pong/kong/self_win)
    chow_head      : Linear(256→4)     pass / LEFT / MIDDLE / RIGHT
    self_kong_head : Linear(256→35)    pass + 34 tile IDs

During forward() the appropriate head is selected by the ``decision`` argument
and an action-mask is applied so that invalid actions have -inf log-prob.
"""

from typing import Dict, Optional, Tuple

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.distributions import Categorical

from env import OBS_DIM, DECISION_SPACES

# ── Network ───────────────────────────────────────────────────────────────────

class MahjongNet(nn.Module):
    """
    Parameters
    ----------
    obs_dim      : int   – observation vector size (default 485)
    hidden_size  : int   – width of shared hidden layers (default 512)
    """

    def __init__(self, obs_dim: int = OBS_DIM, hidden_size: int = 512) -> None:
        super().__init__()
        self.obs_dim = obs_dim

        # ── Shared encoder ────────────────────────────────────────────────────
        self.encoder = nn.Sequential(
            nn.Linear(obs_dim, hidden_size),
            nn.LayerNorm(hidden_size),
            nn.ReLU(),
            nn.Linear(hidden_size, hidden_size // 2),
            nn.LayerNorm(hidden_size // 2),
            nn.ReLU(),
        )
        enc_out = hidden_size // 2  # 256

        # ── Critic ────────────────────────────────────────────────────────────
        self.value_head = nn.Linear(enc_out, 1)

        # ── Policy heads ──────────────────────────────────────────────────────
        self.discard_head    = nn.Linear(enc_out, DECISION_SPACES["discard"])    # 34
        self.binary_head     = nn.Linear(enc_out, DECISION_SPACES["win"])        # 2
        self.chow_head       = nn.Linear(enc_out, DECISION_SPACES["chow"])       # 4
        self.self_kong_head  = nn.Linear(enc_out, DECISION_SPACES["self_kong"])  # 35

        self._init_weights()

    def _init_weights(self) -> None:
        for m in self.modules():
            if isinstance(m, nn.Linear):
                nn.init.orthogonal_(m.weight, gain=np.sqrt(2))
                nn.init.zeros_(m.bias)
        # smaller init for output layers
        for head in (self.value_head, self.discard_head, self.binary_head,
                     self.chow_head, self.self_kong_head):
            nn.init.orthogonal_(head.weight, gain=0.01)

    # ── Forward ───────────────────────────────────────────────────────────────

    def _head_logits(self, enc: torch.Tensor, decision: str) -> torch.Tensor:
        if decision in ("win", "self_win", "pong", "kong"):
            return self.binary_head(enc)
        elif decision == "discard":
            return self.discard_head(enc)
        elif decision == "chow":
            return self.chow_head(enc)
        elif decision == "self_kong":
            return self.self_kong_head(enc)
        else:
            raise ValueError(f"Unknown decision: {decision!r}")

    def forward(
        self,
        obs: torch.Tensor,
        decision: str,
        action_mask: Optional[torch.Tensor] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        Parameters
        ----------
        obs         : (B, obs_dim)
        decision    : which decision head to use
        action_mask : (B, n_actions) bool tensor; True = valid action.
                      If None, all actions are assumed valid.

        Returns
        -------
        logits : (B, n_actions)   raw (masked) logits
        value  : (B,)             state-value estimate
        """
        enc    = self.encoder(obs)
        value  = self.value_head(enc).squeeze(-1)
        logits = self._head_logits(enc, decision)

        if action_mask is not None:
            # set logit of invalid actions to -inf
            logits = logits.masked_fill(~action_mask, float("-inf"))

        return logits, value

    def act(
        self,
        obs: torch.Tensor,
        decision: str,
        action_mask: Optional[torch.Tensor] = None,
        deterministic: bool = False,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Sample (or argmax) an action.

        Returns
        -------
        action   : (B,)  sampled action index
        log_prob : (B,)  log probability of the sampled action
        value    : (B,)  state-value estimate
        """
        logits, value = self.forward(obs, decision, action_mask)
        dist = Categorical(logits=logits)
        if deterministic:
            action = logits.argmax(dim=-1)
        else:
            action = dist.sample()
        log_prob = dist.log_prob(action)
        return action, log_prob, value

    def evaluate_actions(
        self,
        obs: torch.Tensor,
        decision: str,
        actions: torch.Tensor,
        action_mask: Optional[torch.Tensor] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor]:
        """
        Compute log-probs, entropy, and values for a batch of (obs, action) pairs.
        Used during PPO update.

        Returns
        -------
        log_prob : (B,)
        entropy  : (B,)
        value    : (B,)
        """
        logits, value = self.forward(obs, decision, action_mask)
        dist     = Categorical(logits=logits)
        log_prob = dist.log_prob(actions)
        entropy  = dist.entropy()
        return log_prob, entropy, value


# ── Convenience wrapper ───────────────────────────────────────────────────────

class MahjongAgent:
    """
    Thin wrapper around MahjongNet for inference and checkpoint I/O.

    Example
    -------
    >>> agent = MahjongAgent()
    >>> action = agent.select_action(obs_array, "discard", mask_array)
    """

    def __init__(
        self,
        obs_dim: int = OBS_DIM,
        hidden_size: int = 512,
        device: str = "cpu",
    ) -> None:
        self.device = torch.device(device)
        self.net    = MahjongNet(obs_dim, hidden_size).to(self.device)

    def select_action(
        self,
        obs: np.ndarray,
        decision: str,
        action_mask: Optional[np.ndarray] = None,
        deterministic: bool = False,
    ) -> int:
        """Single-step action selection (no gradient)."""
        obs_t = torch.tensor(obs, dtype=torch.float32, device=self.device).unsqueeze(0)
        mask_t = None
        if action_mask is not None:
            mask_t = torch.tensor(action_mask, dtype=torch.bool, device=self.device).unsqueeze(0)
        with torch.no_grad():
            action, _, _ = self.net.act(obs_t, decision, mask_t, deterministic)
        return int(action.item())

    def save(self, path: str) -> None:
        torch.save(
            {
                "net_state": self.net.state_dict(),
                "obs_dim":   self.net.obs_dim,
            },
            path,
        )

    @classmethod
    def load(cls, path: str, device: str = "cpu") -> "MahjongAgent":
        ckpt  = torch.load(path, map_location=device)
        agent = cls(obs_dim=ckpt["obs_dim"], device=device)
        agent.net.load_state_dict(ckpt["net_state"])
        return agent
