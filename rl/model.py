"""
MahjongNet — Multi-head actor-critic network for Hong Kong Mahjong.

Architecture
────────────
  Shared encoder  : Linear(OBS_DIM→512) → LayerNorm → ReLU  (OBS_DIM=587)
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

from env import OBS_DIM, OBS_DIM_V3, OBS_DIM_V4, DECISION_SPACES

# ── Network ───────────────────────────────────────────────────────────────────

class MahjongNet(nn.Module):
    """
    Parameters
    ----------
    obs_dim      : int   – observation vector size (default 485)
    hidden_size  : int   – width of shared hidden layers (default 512)
    """

    def __init__(self, obs_dim: int = OBS_DIM, hidden_size: int = 512,
                 n_layers: int = 2) -> None:
        super().__init__()
        self.obs_dim = obs_dim
        self.n_layers = n_layers

        # ── Shared encoder ────────────────────────────────────────────────────
        layers = []
        in_dim = obs_dim
        for i in range(n_layers):
            out_dim = hidden_size if i == 0 else hidden_size // (2 ** i)
            out_dim = max(out_dim, 128)  # floor at 128
            layers.extend([
                nn.Linear(in_dim, out_dim),
                nn.LayerNorm(out_dim),
                nn.ReLU(),
            ])
            in_dim = out_dim
        self.encoder = nn.Sequential(*layers)
        enc_out = in_dim

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


# ── Convolutional network (obs v3) ────────────────────────────────────────────

class _ResBlock1d(nn.Module):
    def __init__(self, ch: int):
        super().__init__()
        self.conv1 = nn.Conv1d(ch, ch, kernel_size=3, padding=1)
        self.norm1 = nn.GroupNorm(8, ch)
        self.conv2 = nn.Conv1d(ch, ch, kernel_size=3, padding=1)
        self.norm2 = nn.GroupNorm(8, ch)

    def forward(self, x):
        h = F.relu(self.norm1(self.conv1(x)))
        h = self.norm2(self.conv2(h))
        return F.relu(x + h)


class MahjongConvNet(nn.Module):
    """
    Residual 1D CNN over the 34-tile axis for obs v3 (759 dims) or v4 (1443).

    The flat observation is reorganised into tile-planes plus scalars
    broadcast as constant planes: v3 → 22 planes + 11 scalars = 33 input
    channels; v4 adds 20 discard-order planes (4 recency + 16 last-K
    one-hots) + 4 discard-count scalars = 57 channels. Convolution over the
    tile axis gives the suit-run/adjacency inductive bias an MLP lacks.

    Heads:
      discard   : 1×1 conv → per-tile logit (spatially aligned with the input)
      self_kong : per-tile logits (1×1 conv) + pooled pass-logit → 35
      binary / chow / value : mean+max pooled features → MLP
      danger (v4 only, issue #21) : per-opponent tenpai logits (pooled) and
        per-opponent wait logits (3-channel 1×1 conv, spatially aligned) —
        trained supervised from simulator ground truth via forward_danger();
        never part of forward(), so play-time APIs and ONNX export of the
        decision heads are unchanged.
    """

    N_PLANES_V2 = 17   # hand, my 3 groups, 3×3 opp groups, 4 discard planes
    N_SCAL_V2   = 9    # remaining + my seat one-hot + cur player one-hot
    N_PLANES_V3 = 5    # shanten_after, ukeire_after, improve_tiles, unseen, context_tile
    N_SCAL_V3   = 2    # cur_shanten, is_discard_state
    N_PLANES_V4 = 20   # 4 recency + 4×K(=4) last-K discard one-hots
    N_SCAL_V4   = 4    # per-player discard counts

    def __init__(self, obs_dim: int = OBS_DIM_V3, channels: int = 128,
                 n_blocks: int = 6) -> None:
        super().__init__()
        assert obs_dim in (OBS_DIM_V3, OBS_DIM_V4), \
            "MahjongConvNet requires obs v3 or v4"
        self.obs_dim  = obs_dim
        self.channels = channels
        self.n_blocks = n_blocks
        self.is_v4    = obs_dim == OBS_DIM_V4

        n_planes  = self.N_PLANES_V2 + self.N_PLANES_V3          # 22
        n_scalars = self.N_SCAL_V2 + self.N_SCAL_V3              # 11
        if self.is_v4:
            n_planes  += self.N_PLANES_V4                         # 42
            n_scalars += self.N_SCAL_V4                           # 15
        in_ch = n_planes + n_scalars                              # 33 / 57

        self.stem   = nn.Sequential(
            nn.Conv1d(in_ch, channels, kernel_size=3, padding=1),
            nn.GroupNorm(8, channels),
            nn.ReLU(),
        )
        self.blocks = nn.Sequential(*[_ResBlock1d(channels) for _ in range(n_blocks)])

        # Per-tile heads
        self.discard_conv   = nn.Conv1d(channels, 1, kernel_size=1)
        self.self_kong_conv = nn.Conv1d(channels, 1, kernel_size=1)

        # Pooled heads. Binary decisions get SEPARATE heads: the observation
        # does not identify which question is being asked, and kong labels
        # (which depend on FF's hidden target state) would otherwise corrupt
        # the always-accept win/self_win behaviour through a shared head.
        self.pool_fc     = nn.Sequential(nn.Linear(2 * channels, 256), nn.ReLU())
        self.value_head  = nn.Linear(256, 1)
        self.win_head      = nn.Linear(256, 2)
        self.self_win_head = nn.Linear(256, 2)
        self.pong_head     = nn.Linear(256, 2)
        self.kong_head     = nn.Linear(256, 2)
        self.chow_head   = nn.Linear(256, DECISION_SPACES["chow"])  # 4
        self.self_kong_pass = nn.Linear(256, 1)

        # Danger auxiliary heads (issue #21) — only exist on v4 nets so v3
        # checkpoints keep their exact module set (strict state_dict loads).
        if self.is_v4:
            self.danger_wait_conv = nn.Conv1d(channels, 3, kernel_size=1)
            self.danger_tenpai_head = nn.Linear(256, 3)

    # ── Input reorganisation ──────────────────────────────────────────────────

    def _to_planes(self, obs: torch.Tensor) -> torch.Tensor:
        B = obs.shape[0]
        p2 = self.N_PLANES_V2 * 34                       # 578
        s2 = p2 + self.N_SCAL_V2                          # 587
        p3 = s2 + self.N_PLANES_V3 * 34                   # 757
        s3 = p3 + self.N_SCAL_V3                          # 759 = OBS_DIM_V3
        plane_parts = [
            obs[:, :p2].view(B, self.N_PLANES_V2, 34),
            obs[:, s2:p3].view(B, self.N_PLANES_V3, 34),
        ]
        scalar_parts = [obs[:, p2:s2], obs[:, p3:s3]]
        if self.is_v4:
            p4 = s3 + self.N_PLANES_V4 * 34               # 1439
            plane_parts.append(obs[:, s3:p4].view(B, self.N_PLANES_V4, 34))
            scalar_parts.append(obs[:, p4:])              # (B, 4) counts
        planes = torch.cat(plane_parts, dim=1)            # (B, 22|42, 34)
        scalars = torch.cat(scalar_parts, dim=1)          # (B, 11|15)
        scal_planes = scalars.unsqueeze(-1).expand(-1, -1, 34)
        return torch.cat([planes, scal_planes], dim=1)    # (B, 33|57, 34)

    # ── Forward ───────────────────────────────────────────────────────────────

    def _trunk(self, obs: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        x = self.stem(self._to_planes(obs))
        x = self.blocks(x)                                          # (B, C, 34)
        pooled = torch.cat([x.mean(dim=2), x.amax(dim=2)], dim=1)   # (B, 2C)
        feat   = self.pool_fc(pooled)                                # (B, 256)
        return x, feat

    def forward(
        self,
        obs: torch.Tensor,
        decision: str,
        action_mask: Optional[torch.Tensor] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor]:
        x, feat = self._trunk(obs)
        value = self.value_head(feat).squeeze(-1)

        if decision == "discard":
            logits = self.discard_conv(x).squeeze(1)                 # (B, 34)
        elif decision in ("win", "self_win", "pong", "kong"):
            head = {"win": self.win_head, "self_win": self.self_win_head,
                    "pong": self.pong_head, "kong": self.kong_head}[decision]
            logits = head(feat)                                      # (B, 2)
        elif decision == "chow":
            logits = self.chow_head(feat)                            # (B, 4)
        elif decision == "self_kong":
            per_tile = self.self_kong_conv(x).squeeze(1)             # (B, 34)
            logits = torch.cat([self.self_kong_pass(feat), per_tile], dim=1)  # (B, 35)
        else:
            raise ValueError(f"Unknown decision: {decision!r}")

        if action_mask is not None:
            logits = logits.masked_fill(~action_mask, float("-inf"))
        return logits, value

    # ── Danger auxiliary forward (v4 only, issue #21) ─────────────────────────

    def forward_danger(self, obs: torch.Tensor) -> Tuple[torch.Tensor, torch.Tensor]:
        """
        Ground-truth-supervised opponent danger predictions.

        Returns (tenpai_logits (B, 3), wait_logits (B, 3, 34)), opponents in
        seat-relative order (my_id+1 .. my_id+3) matching the opp_groups
        planes and the opp_tenpai/opp_waits labels.
        """
        assert self.is_v4, "danger heads require a v4 net"
        x, feat = self._trunk(obs)
        return self.danger_tenpai_head(feat), self.danger_wait_conv(x)

    def forward_with_danger(
        self,
        obs: torch.Tensor,
        decision: str,
        action_mask: Optional[torch.Tensor] = None,
    ) -> Tuple[torch.Tensor, torch.Tensor, torch.Tensor, torch.Tensor]:
        """forward() + danger heads off a SINGLE trunk pass (training only).

        Returns (logits, value, tenpai_logits (B,3), wait_logits (B,3,34)).
        """
        assert self.is_v4, "danger heads require a v4 net"
        x, feat = self._trunk(obs)
        value = self.value_head(feat).squeeze(-1)

        if decision == "discard":
            logits = self.discard_conv(x).squeeze(1)
        elif decision in ("win", "self_win", "pong", "kong"):
            head = {"win": self.win_head, "self_win": self.self_win_head,
                    "pong": self.pong_head, "kong": self.kong_head}[decision]
            logits = head(feat)
        elif decision == "chow":
            logits = self.chow_head(feat)
        elif decision == "self_kong":
            per_tile = self.self_kong_conv(x).squeeze(1)
            logits = torch.cat([self.self_kong_pass(feat), per_tile], dim=1)
        else:
            raise ValueError(f"Unknown decision: {decision!r}")

        if action_mask is not None:
            logits = logits.masked_fill(~action_mask, float("-inf"))
        return logits, value, self.danger_tenpai_head(feat), self.danger_wait_conv(x)

    # Same sampling/evaluation API as MahjongNet
    act              = MahjongNet.act
    evaluate_actions = MahjongNet.evaluate_actions


def warm_start_v4_from_v3(v4_net: MahjongConvNet, v3_ckpt_path: str) -> None:
    """
    Initialise a v4 MahjongConvNet from a trained v3 conv checkpoint so the
    retrain starts from the champion's function instead of scratch.

    Every layer except the stem's first conv has identical shapes and is
    copied verbatim. The stem conv's input channels are remapped to the v4
    channel order (planes first, then broadcast scalars):

        v3 channel   0..21  (22 planes)   → v4 channel  0..21
        v3 channel  22..32  (11 scalars)  → v4 channel 42..52
        v4 channels 22..41 (order planes) and 53..56 (count scalars) → zero

    Zero weights on the new channels make the v4 net EXACTLY reproduce the
    v3 net's outputs at init (new inputs are ignored until training moves
    them). Danger heads keep their fresh init — they have no v3 ancestor.
    """
    assert v4_net.is_v4
    ckpt = torch.load(v3_ckpt_path, map_location="cpu", weights_only=False)
    assert ckpt.get("arch") == "conv" and ckpt.get("obs_dim") == OBS_DIM_V3, \
        "warm start requires a v3 conv checkpoint"
    v3_state = ckpt["net_state"]

    n_p3 = MahjongConvNet.N_PLANES_V2 + MahjongConvNet.N_PLANES_V3   # 22
    n_s3 = MahjongConvNet.N_SCAL_V2 + MahjongConvNet.N_SCAL_V3       # 11
    n_p4 = n_p3 + MahjongConvNet.N_PLANES_V4                          # 42

    own = v4_net.state_dict()
    stem_key = "stem.0.weight"
    for k, v in v3_state.items():
        if k == stem_key:
            assert v.shape[1] == n_p3 + n_s3, f"unexpected v3 stem in_ch {v.shape}"
            w = torch.zeros_like(own[k])                       # (C_out, 57, 3)
            w[:, :n_p3, :] = v[:, :n_p3, :]                    # planes
            w[:, n_p4:n_p4 + n_s3, :] = v[:, n_p3:, :]         # broadcast scalars
            own[k] = w
        else:
            assert own[k].shape == v.shape, f"shape mismatch at {k}"
            own[k] = v
    v4_net.load_state_dict(own)


def load_net(path: str, device: str = "cpu"):
    """Load a checkpoint into the right architecture (MLP or conv).

    Returns (net, meta) where meta holds the architecture fields needed to
    re-save a compatible checkpoint.
    """
    ckpt = torch.load(path, map_location="cpu", weights_only=False)
    arch = ckpt.get("arch", "mlp")
    if arch == "conv":
        net = MahjongConvNet(obs_dim=ckpt.get("obs_dim", OBS_DIM_V3),
                             channels=ckpt.get("channels", 128),
                             n_blocks=ckpt.get("n_blocks", 6))
        meta = {"obs_dim": net.obs_dim, "arch": "conv",
                "channels": net.channels, "n_blocks": net.n_blocks}
    else:
        net = MahjongNet(obs_dim=ckpt.get("obs_dim", OBS_DIM),
                         hidden_size=ckpt.get("hidden_size", 512),
                         n_layers=ckpt.get("n_layers", 2))
        meta = {"obs_dim": net.obs_dim, "arch": "mlp",
                "hidden_size": ckpt.get("hidden_size", 512),
                "n_layers": net.n_layers}
    net.load_state_dict(ckpt["net_state"])
    return net.to(device), meta


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
        n_layers: int = 2,
        device: str = "cpu",
        arch: str = "mlp",
        channels: int = 128,
        n_blocks: int = 6,
    ) -> None:
        self.device = torch.device(device)
        self.arch   = arch
        if arch == "conv":
            self.net = MahjongConvNet(obs_dim, channels=channels,
                                      n_blocks=n_blocks).to(self.device)
        else:
            self.net = MahjongNet(obs_dim, hidden_size, n_layers=n_layers).to(self.device)

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
        meta = {
            "net_state": self.net.state_dict(),
            "obs_dim":   self.net.obs_dim,
            "arch":      self.arch,
        }
        if self.arch == "conv":
            meta["channels"] = self.net.channels
            meta["n_blocks"] = self.net.n_blocks
        else:
            meta["n_layers"] = self.net.n_layers
        torch.save(meta, path)

    @classmethod
    def load(cls, path: str, device: str = "cpu") -> "MahjongAgent":
        ckpt  = torch.load(path, map_location=device, weights_only=False)
        agent = cls(obs_dim=ckpt.get("obs_dim", OBS_DIM),
                    hidden_size=ckpt.get("hidden_size", 512),
                    n_layers=ckpt.get("n_layers", 2),
                    arch=ckpt.get("arch", "mlp"),
                    channels=ckpt.get("channels", 128),
                    n_blocks=ckpt.get("n_blocks", 6),
                    device=device)
        agent.net.load_state_dict(ckpt["net_state"])
        return agent
