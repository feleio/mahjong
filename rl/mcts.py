"""
mcts.py — Imperfect-information MCTS for Hong Kong Mahjong.

Implements determinization-based Monte Carlo Tree Search (also called
Perfect Information Monte Carlo, or IS-MCTS) for the discard decision.

For binary decisions (win/pong/kong/chow/self_win/self_kong) the neural
network is used directly since these decisions are high-confidence and
MCTS would not add much.

For the discard decision, each valid tile is evaluated by running N
Monte Carlo rollouts through the MCTSRolloutServer (a Scala subprocess
that handles state reconstruction, determinization, and game completion
with heuristic players).

The resulting action-value estimates are converted to a policy
distribution (Boltzmann/softmax with temperature τ):

    π(a) ∝ exp(Q(a) / τ)

This policy is used both for:
  - Selecting the action during data collection
  - As the training target for AlphaZero-style supervised updates

Usage
─────
    from mcts import MCTSRolloutClient, ImperfectInfoMCTS

    client = MCTSRolloutClient(jar_path="target/scala-2.12/mahjong-assembly-0.1.0.jar",
                               n_rollouts=10)
    mcts = ImperfectInfoMCTS(net=net, rollout_client=client, temperature=1.0, device="cuda")

    # At a decision point:
    action, mcts_policy, nn_value = mcts.get_action_and_policy(
        obs, state, decision, context, action_mask
    )
"""

import json
import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
import torch
import torch.nn.functional as F
from torch.distributions import Categorical

sys.path.insert(0, str(Path(__file__).parent))
from env import DECISION_SPACES, OBS_DIM


# ── Rollout client ─────────────────────────────────────────────────────────────

class MCTSRolloutClient:
    """
    Subprocess wrapper for MCTSRolloutServer.

    One instance manages one Scala JVM process.  Multiple Python workers
    should each create their own client (one JVM per worker).

    Parameters
    ----------
    jar_path      : path to the assembled fat-JAR
    n_rollouts    : Monte Carlo rollouts per candidate tile
    rollout_opp   : "chicken" (fast, default) or "firstfelix" (stronger estimate)
    java_bin      : path to java executable
    """

    def __init__(
        self,
        jar_path: str,
        n_rollouts: int = 10,
        rollout_opp: str = "chicken",
        java_bin: str = "java",
    ) -> None:
        self.n_rollouts  = n_rollouts
        self.rollout_opp = rollout_opp
        self._proc = subprocess.Popen(
            [
                java_bin,
                "-Dlogback.statusListenerClass=ch.qos.logback.core.status.NopStatusListener",
                "-cp", jar_path,
                "io.fele.app.mahjong.rl.MCTSRolloutServer",
            ],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            bufsize=1,
            text=True,
        )

    def evaluate_batch(
        self,
        state: dict,
        candidate_tiles: List[int],
        n_worlds: int,
        self_policy: str = "firstfelix",
    ) -> np.ndarray:
        """
        Evaluate all candidate discard tiles on the SAME set of determinized
        worlds (common random numbers). Paired evaluation makes the variance
        of Q-value *differences* between tiles far smaller than independent
        rollouts, so far fewer worlds are needed for reliable ranking.

        Returns
        -------
        rewards : ndarray of shape (n_worlds, n_candidates)
        """
        cmd = {
            "cmd":             "evaluate_batch",
            "hand":            state["hand"],
            "my_groups":       state.get("my_groups", {"pongs": [], "kongs": [], "chows": []}),
            "opp_groups":      state.get("opp_groups", [{}, {}, {}]),
            "discards":        state["discarded"],
            "remaining":       state["remaining"],
            "candidate_tiles": [int(t) for t in candidate_tiles],
            "n_worlds":        int(n_worlds),
            "rollout_opp":     self.rollout_opp,
            "self_policy":     self_policy,
        }
        self._proc.stdin.write(json.dumps(cmd) + "\n")
        self._proc.stdin.flush()
        line = self._proc.stdout.readline()
        if not line:
            stderr = self._proc.stderr.read()
            raise RuntimeError(f"MCTSRolloutServer closed unexpectedly.\nstderr:\n{stderr}")
        resp = json.loads(line.strip())
        return np.asarray(resp["rewards"], dtype=np.float64)

    def evaluate_discard(self, state: dict, discard_tile: int) -> Tuple[float, float]:
        """
        Estimate the value of discarding `discard_tile` from `state`.

        Parameters
        ----------
        state        : raw state dict from the Scala observation (as-is)
        discard_tile : tile ID (0-33) to evaluate

        Returns
        -------
        (mean_reward, std_reward) across all rollouts
        """
        cmd = {
            "cmd":          "evaluate",
            "hand":         state["hand"],
            "my_groups":    state.get("my_groups", {"pongs": [], "kongs": [], "chows": []}),
            "opp_groups":   state.get("opp_groups", [{}, {}, {}]),
            "discards":     state["discarded"],
            "remaining":    state["remaining"],
            "discard_tile": discard_tile,
            "n_rollouts":   self.n_rollouts,
            "rollout_opp":  self.rollout_opp,
        }
        self._proc.stdin.write(json.dumps(cmd) + "\n")
        self._proc.stdin.flush()
        line = self._proc.stdout.readline()
        if not line:
            stderr = self._proc.stderr.read()
            raise RuntimeError(f"MCTSRolloutServer closed unexpectedly.\nstderr:\n{stderr}")
        resp = json.loads(line.strip())
        return float(resp["mean_reward"]), float(resp["std_reward"])

    def close(self) -> None:
        try:
            self._proc.stdin.close()
            self._proc.wait(timeout=5)
        except Exception:
            self._proc.kill()

    def __enter__(self) -> "MCTSRolloutClient":
        return self

    def __exit__(self, *_) -> None:
        self.close()


# ── MCTS policy wrapper ────────────────────────────────────────────────────────

class ImperfectInfoMCTS:
    """
    Imperfect-information MCTS via determinization (IS-MCTS / PIMC).

    For discard decisions: evaluate each valid tile with Monte Carlo rollouts
    and convert action values to a Boltzmann policy.

    For binary decisions (win/pong/kong/self_win/chow/self_kong): use the
    neural network policy directly (these decisions are straightforward and
    MCTS overhead is not justified).

    Parameters
    ----------
    net            : MahjongNet (actor-critic)
    rollout_client : MCTSRolloutClient instance
    temperature    : τ for Boltzmann action selection from MCTS values.
                     τ=1.0 → proportional to exp(Q/σ) where σ scales Q.
                     τ→0  → greedy; τ→∞ → uniform.
    value_weight   : blend NN value with MCTS mean Q for the stored value target.
                     0.0 = pure NN value; 1.0 = pure MCTS mean Q.
    device         : torch device string
    """

    def __init__(
        self,
        net,
        rollout_client: MCTSRolloutClient,
        temperature: float = 1.0,
        value_weight: float = 0.5,
        device: str = "cpu",
    ) -> None:
        self.net           = net
        self.rollout_client = rollout_client
        self.temperature   = temperature
        self.value_weight  = value_weight
        self.device        = torch.device(device)

    def get_action_and_policy(
        self,
        obs: np.ndarray,
        state: dict,
        decision: str,
        context: dict,
        action_mask: Optional[np.ndarray],
    ) -> Tuple[int, np.ndarray, float]:
        """
        Select an action and return the MCTS policy vector and value estimate.

        Parameters
        ----------
        obs         : (OBS_DIM,) float32 observation
        state       : raw state dict from the Scala observation
        decision    : decision type string
        context     : context dict from the observation
        action_mask : boolean mask (True = valid)

        Returns
        -------
        action      : int, selected action index
        policy      : ndarray of shape (n_actions,), probability distribution
        value       : float, value estimate (blended NN + MCTS)
        """
        obs_t  = torch.tensor(obs, dtype=torch.float32, device=self.device).unsqueeze(0)
        mask_t = (
            torch.tensor(action_mask, dtype=torch.bool, device=self.device).unsqueeze(0)
            if action_mask is not None else None
        )

        with torch.no_grad():
            logits, value_t = self.net.forward(obs_t, decision, mask_t)
            nn_value = value_t.item()
            nn_probs = torch.softmax(logits, dim=-1).squeeze(0).cpu().numpy()

        # ── Non-discard decisions: use neural network directly ─────────────────
        if decision != "discard":
            dist   = Categorical(logits=logits)
            action = dist.sample().item()
            return action, nn_probs, nn_value

        # ── Discard: MCTS rollout evaluation ──────────────────────────────────
        n_actions   = DECISION_SPACES["discard"]  # 34
        valid_tiles = [
            int(t)
            for t in context.get("valid_tiles", [])
            if action_mask is None or action_mask[int(t)]
        ]

        if len(valid_tiles) == 0:
            # Fallback: no valid tiles (shouldn't happen)
            return 0, np.ones(n_actions, dtype=np.float32) / n_actions, nn_value

        if len(valid_tiles) == 1:
            policy = np.zeros(n_actions, dtype=np.float32)
            policy[valid_tiles[0]] = 1.0
            return valid_tiles[0], policy, nn_value

        # Evaluate each candidate tile via MC rollouts
        tile_q: Dict[int, float] = {}
        for tile in valid_tiles:
            mean_r, _ = self.rollout_client.evaluate_discard(state, tile)
            tile_q[tile] = mean_r

        # Boltzmann policy from MCTS Q-values
        q_vals = np.array([tile_q[t] for t in valid_tiles], dtype=np.float64)

        if self.temperature > 0:
            scaled     = q_vals / (self.temperature * (q_vals.std() + 1e-6))
            scaled    -= scaled.max()             # numerical stability
            exp_q      = np.exp(scaled)
            mcts_probs = exp_q / (exp_q.sum() + 1e-12)
        else:
            mcts_probs = np.zeros(len(valid_tiles))
            mcts_probs[np.argmax(q_vals)] = 1.0

        # Full policy vector (34 actions)
        mcts_policy = np.zeros(n_actions, dtype=np.float32)
        for t, p in zip(valid_tiles, mcts_probs):
            mcts_policy[t] = float(p)

        # Sample action from MCTS policy
        action_idx = int(np.random.choice(len(valid_tiles), p=mcts_probs))
        action     = valid_tiles[action_idx]

        # Blended value
        mcts_q_mean = float(np.sum(mcts_probs * q_vals))
        value = (1.0 - self.value_weight) * nn_value + self.value_weight * mcts_q_mean

        return action, mcts_policy, value

    def greedy_discard(
        self,
        state: dict,
        context: dict,
        action_mask: Optional[np.ndarray],
    ) -> int:
        """Greedy discard selection (argmax Q, no sampling). For evaluation only."""
        valid_tiles = [
            int(t)
            for t in context.get("valid_tiles", [])
            if action_mask is None or action_mask[int(t)]
        ]
        if len(valid_tiles) <= 1:
            return valid_tiles[0] if valid_tiles else 0
        tile_q = {t: self.rollout_client.evaluate_discard(state, t)[0] for t in valid_tiles}
        return max(tile_q, key=tile_q.get)


# ── Paired-worlds MCTS policy improver ─────────────────────────────────────────

class PairedMCTSPolicy:
    """
    Policy improvement operator on top of a strong imitation policy.

    Discard decisions:
      1. Prune candidates to the top-k tiles by the NN policy (the NN argmax —
         the imitation/FF choice — is always candidate 0).
      2. Evaluate ALL candidates on the same K determinized worlds
         (common random numbers) via MCTSRolloutClient.evaluate_batch.
      3. For each alternative, compute the PAIRED difference vs the NN choice:
         d_c[k] = reward(c, world k) − reward(nn_choice, world k).
         Switch to the best alternative only when
         mean(d) > z · SE(d)  and  mean(d) > min_gain.
         Otherwise keep the NN choice.

    This makes MCTS a conservative strict-improvement operator: it can only
    deviate from the 99.5%-accurate imitation policy when the evidence that
    the deviation is better survives a paired significance test.

    Non-discard decisions: NN argmax (deterministic).
    """

    def __init__(
        self,
        net,
        rollout_client: MCTSRolloutClient,
        n_worlds: int = 32,
        top_k: int = 6,
        z_threshold: float = 1.5,
        min_gain: float = 0.25,
        device: str = "cpu",
    ) -> None:
        self.net            = net
        self.rollout_client = rollout_client
        self.n_worlds       = n_worlds
        self.top_k          = top_k
        self.z_threshold    = z_threshold
        self.min_gain       = min_gain
        self.device         = torch.device(device)
        # Stats
        self.n_discard_decisions = 0
        self.n_switches          = 0

    def _nn_logits(self, obs, decision, action_mask):
        obs_t  = torch.tensor(obs, dtype=torch.float32, device=self.device).unsqueeze(0)
        mask_t = (
            torch.tensor(action_mask, dtype=torch.bool, device=self.device).unsqueeze(0)
            if action_mask is not None else None
        )
        with torch.no_grad():
            logits, value_t = self.net.forward(obs_t, decision, mask_t)
        return logits.squeeze(0).cpu().numpy(), value_t.item()

    def get_action(
        self,
        obs: np.ndarray,
        state: dict,
        decision: str,
        context: dict,
        action_mask: Optional[np.ndarray],
    ) -> Tuple[int, dict]:
        """
        Returns (action, info). info contains:
          nn_action  : what the imitation policy would have played
          switched   : whether MCTS overrode the NN choice
          candidates : candidate tiles evaluated (discard only)
          mean_gain  : paired mean gain of chosen vs NN choice (discard only)
        """
        logits, nn_value = self._nn_logits(obs, decision, action_mask)
        nn_action = int(np.argmax(logits))

        if decision != "discard":
            return nn_action, {"nn_action": nn_action, "switched": False}

        valid_tiles = [
            int(t)
            for t in context.get("valid_tiles", [])
            if action_mask is None or action_mask[int(t)]
        ]
        if len(valid_tiles) <= 1:
            action = valid_tiles[0] if valid_tiles else nn_action
            return action, {"nn_action": action, "switched": False}

        self.n_discard_decisions += 1

        # Candidate set: top-k valid tiles by NN logit, NN choice first
        order = sorted(valid_tiles, key=lambda t: -logits[t])
        candidates = order[: self.top_k]
        if nn_action not in candidates:
            # nn_action should be order[0] when it is valid; be safe anyway
            candidates = [nn_action] + candidates[:-1] if nn_action in valid_tiles \
                         else candidates
        base_idx = candidates.index(nn_action) if nn_action in candidates else 0

        rewards = self.rollout_client.evaluate_batch(
            state, candidates, self.n_worlds
        )  # (K, C)

        diffs = rewards - rewards[:, base_idx:base_idx + 1]     # paired diffs vs NN choice
        mean_d = diffs.mean(axis=0)                             # (C,)
        se_d   = diffs.std(axis=0, ddof=1) / np.sqrt(rewards.shape[0]) + 1e-9

        best = int(np.argmax(mean_d))
        q_info = {
            "candidates": candidates,
            "mean_diffs": mean_d.tolist(),   # paired ΔQ vs NN choice, per candidate
            "se_diffs":   se_d.tolist(),
        }
        if (
            best != base_idx
            and mean_d[best] > self.z_threshold * se_d[best]
            and mean_d[best] > self.min_gain
        ):
            self.n_switches += 1
            return candidates[best], {
                "nn_action": nn_action,
                "switched": True,
                "mean_gain": float(mean_d[best]),
                **q_info,
            }

        return nn_action, {
            "nn_action": nn_action,
            "switched": False,
            "mean_gain": 0.0,
            **q_info,
        }

