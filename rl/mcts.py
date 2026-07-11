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
        nn_model: Optional[str] = None,
        rollout_threads: int = 1,
        belief_model: Optional[str] = None,
        value_model: Optional[str] = None,
    ) -> None:
        self.n_rollouts  = n_rollouts
        self.rollout_opp = rollout_opp
        extra = []
        if nn_model is not None:
            extra.append(f"-Drl.nnmodel={nn_model}")
        if belief_model is not None:
            # Opt in to belief-weighted opponent-hand determinization; without
            # this flag the server falls back to the uniform shuffle-and-deal.
            extra.append(f"-Drl.belief={belief_model}")
        if value_model is not None:
            # Dedicated outcome-trained value net for IS-MCTS leaf bootstrap
            # (rollout_tail="zero"); without it leaves use the policy net's value.
            extra.append(f"-Drl.valuemodel={value_model}")
        if rollout_threads > 1:
            extra.append(f"-Drl.rolloutThreads={rollout_threads}")
        self._proc = subprocess.Popen(
            [
                java_bin,
                "-Dlogback.statusListenerClass=ch.qos.logback.core.status.NopStatusListener",
                *extra,
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
        value_leaf_plies: int = 0,
    ) -> np.ndarray:
        """
        Evaluate all candidate discard tiles on the SAME set of determinized
        worlds (common random numbers). Paired evaluation makes the variance
        of Q-value *differences* between tiles far smaller than independent
        rollouts, so far fewer worlds are needed for reliable ranking.

        value_leaf_plies > 0 selects the CRN-preserving flat + value-leaf
        hybrid (issue #20): each rollout stops at our Nth discard decision
        after the root discard and returns the value net's estimate there
        (real outcome if the game ends earlier). Needs the client constructed
        with value_model= (and nn_model= for the pre-leaf policy); our seat's
        self_policy is effectively "nn" in this mode.

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
        if value_leaf_plies > 0:
            cmd["value_leaf_plies"] = int(value_leaf_plies)
        # Real discard history in rollout seat order (0 = us, then opponents
        # in opp_groups order). NN rollout policies read it; heuristics don't.
        dbp = state.get("discarded_by_player")
        if dbp is not None:
            me = int(state["my_id"])
            cmd["discards_by_player"] = [dbp[(me + i) % 4] for i in range(4)]
        self._proc.stdin.write(json.dumps(cmd) + "\n")
        self._proc.stdin.flush()
        line = self._proc.stdout.readline()
        if not line:
            stderr = self._proc.stderr.read()
            raise RuntimeError(f"MCTSRolloutServer closed unexpectedly.\nstderr:\n{stderr}")
        resp = json.loads(line.strip())
        return np.asarray(resp["rewards"], dtype=np.float64)

    def ff_decide(self, state: dict) -> int:
        """
        What FirstFelix would discard from this exact seat: a fresh FF
        re-decides its target from this hand + discard history, then picks
        its discard (deterministic). For champion-vs-FF analysis
        (rl/compare_vs_ff.py).
        """
        cmd = {
            "cmd":        "ff_decide",
            "hand":       state["hand"],
            "my_groups":  state.get("my_groups", {"pongs": [], "kongs": [], "chows": []}),
            "opp_groups": state.get("opp_groups", [{}, {}, {}]),
            "discards":   state["discarded"],
            "remaining":  state["remaining"],
        }
        dbp = state.get("discarded_by_player")
        if dbp is not None:
            me = int(state["my_id"])
            cmd["discards_by_player"] = [dbp[(me + i) % 4] for i in range(4)]
        self._proc.stdin.write(json.dumps(cmd) + "\n")
        self._proc.stdin.flush()
        line = self._proc.stdout.readline()
        if not line:
            stderr = self._proc.stderr.read()
            raise RuntimeError(f"MCTSRolloutServer closed unexpectedly.\nstderr:\n{stderr}")
        return int(json.loads(line.strip())["tile"])

    def search(
        self,
        state: dict,
        candidate_tiles: List[int],
        sims: int,
        c_puct: float = 1.5,
        temperature: float = 1.0,
        rollout_tail: str = "inf",
        n_parallel: int = 1,
        max_depth: int = 64,
        rollout_opp: str = "firstfelix",
        root_dirichlet_frac: float = 0.0,
        root_dirichlet_alpha: float = 0.3,
        candidate_priors: Optional[List[float]] = None,
        seed: Optional[int] = None,
    ) -> dict:
        """
        Run a determinized information-set MCTS (PIMC-UCT) rooted at `state`,
        returning the root statistics. The tree branches on our discard
        sequence; leaves are evaluated by playing to game end (rollout_tail
        "inf") or by the net value head (rollout_tail "zero").

        Returns
        -------
        dict with keys: tiles, visits, q, prior, value, policy — each list
        aligned to `candidate_tiles` (policy = visits ** (1/temperature)).
        """
        cmd = {
            "cmd":             "search",
            "hand":            state["hand"],
            "my_groups":       state.get("my_groups", {"pongs": [], "kongs": [], "chows": []}),
            "opp_groups":      state.get("opp_groups", [{}, {}, {}]),
            "discards":        state["discarded"],
            "remaining":       state["remaining"],
            "candidate_tiles": [int(t) for t in candidate_tiles],
            "sims":            int(sims),
            "c_puct":          float(c_puct),
            "temperature":     float(temperature),
            "rollout_tail":    rollout_tail,
            "n_parallel":      int(n_parallel),
            "max_depth":       int(max_depth),
            "rollout_opp":     rollout_opp,
            "root_dirichlet_frac":  float(root_dirichlet_frac),
            "root_dirichlet_alpha": float(root_dirichlet_alpha),
        }
        if candidate_priors is not None:
            # Root PUCT prior over the candidates from the *decision* net, so the
            # tree searches around the strong net's beliefs rather than the
            # student rollout net's (which the server loads for the tail).
            cmd["candidate_priors"] = [float(p) for p in candidate_priors]
        if seed is not None:
            cmd["seed"] = int(seed)
        # Real discard history in rollout seat order (0 = us), same remap as
        # evaluate_batch — NN rollout policies read it.
        dbp = state.get("discarded_by_player")
        if dbp is not None:
            me = int(state["my_id"])
            cmd["discards_by_player"] = [dbp[(me + i) % 4] for i in range(4)]
        self._proc.stdin.write(json.dumps(cmd) + "\n")
        self._proc.stdin.flush()
        line = self._proc.stdout.readline()
        if not line:
            stderr = self._proc.stderr.read()
            raise RuntimeError(f"MCTSRolloutServer closed unexpectedly.\nstderr:\n{stderr}")
        return json.loads(line.strip())

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

    value_leaf_plies > 0 switches the rollouts to the CRN-preserving flat +
    value-leaf hybrid (issue #20): same paired-worlds estimator, but each
    rollout stops at our Nth discard decision and bootstraps the value net
    (client needs value_model= and nn_model=; self_policy is ignored for our
    seat in this mode).

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
        self_policy: str = "firstfelix",
        value_leaf_plies: int = 0,
    ) -> None:
        self.net            = net
        self.rollout_client = rollout_client
        self.n_worlds       = n_worlds
        self.top_k          = top_k
        self.z_threshold    = z_threshold
        self.min_gain       = min_gain
        self.device         = torch.device(device)
        self.self_policy    = self_policy
        self.value_leaf_plies = value_leaf_plies
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
            state, candidates, self.n_worlds, self_policy=self.self_policy,
            value_leaf_plies=self.value_leaf_plies,
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


# ── IS-MCTS tree-search policy improver ────────────────────────────────────────

class SearchPolicy:
    """
    Information-set MCTS policy, a drop-in replacement for PairedMCTSPolicy.

    Discard decisions run a real PUCT tree (MCTSRolloutClient.search) over the
    top-k NN candidates and act on the visit-count policy π(a) ∝ N(a)^(1/τ).
    Non-discard decisions use the NN argmax (as PairedMCTSPolicy does).

    Training targets (info dict) are emitted in PairedMCTSPolicy's schema so the
    existing datagen/distill pipeline needs no change (Phase 1):
      - mean_diffs = centered log-visits  (so softmax(mean_d / τ_floor) ∝ N^(1/τ_floor))
      - se_diffs   = const                (drives the per-sample τ to τ_floor)
    The raw visits/q/prior/value are also attached for analysis and the future
    outcome-value work.

    Parameters mirror PairedMCTSPolicy plus the search knobs. `deterministic`
    picks argmax-visits (eval); set False to sample from π (datagen exploration).
    """

    def __init__(
        self,
        net,
        rollout_client: MCTSRolloutClient,
        sims: int = 256,
        top_k: int = 6,
        c_puct: float = 1.5,
        temperature: float = 1.0,
        rollout_tail: str = "inf",
        n_parallel: int = 1,
        max_depth: int = 64,
        rollout_opp: str = "firstfelix",
        deterministic: bool = True,
        switch_margin: float = 0.5,
        dirichlet_frac: float = 0.0,
        dirichlet_alpha: float = 0.3,
        device: str = "cpu",
    ) -> None:
        self.net            = net
        self.rollout_client = rollout_client
        self.sims           = sims
        self.top_k          = top_k
        self.c_puct         = c_puct
        self.temperature    = temperature
        self.rollout_tail   = rollout_tail
        self.n_parallel     = n_parallel
        self.max_depth      = max_depth
        self.rollout_opp    = rollout_opp
        self.deterministic  = deterministic
        self.switch_margin  = switch_margin
        self.dirichlet_frac = dirichlet_frac
        self.dirichlet_alpha = dirichlet_alpha
        self.device         = torch.device(device)
        # Stats read by the eval harnesses.
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
        logits, _ = self._nn_logits(obs, decision, action_mask)
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

        # Candidate set: top-k valid tiles by NN logit, NN choice first.
        order = sorted(valid_tiles, key=lambda t: -logits[t])
        candidates = order[: self.top_k]
        if nn_action in valid_tiles and nn_action not in candidates:
            candidates = [nn_action] + candidates[:-1]
        base_idx = candidates.index(nn_action) if nn_action in candidates else 0

        # Root PUCT prior from THIS net (the strong decision net), so the tree
        # searches around its beliefs — not the student rollout net's.
        cand_logits = np.array([logits[t] for t in candidates], dtype=np.float64)
        cand_logits -= cand_logits.max()
        cand_priors = np.exp(cand_logits)
        cand_priors /= cand_priors.sum()

        resp = self.rollout_client.search(
            state, candidates, self.sims,
            c_puct=self.c_puct, temperature=self.temperature,
            rollout_tail=self.rollout_tail, n_parallel=self.n_parallel,
            max_depth=self.max_depth, rollout_opp=self.rollout_opp,
            root_dirichlet_frac=self.dirichlet_frac,
            root_dirichlet_alpha=self.dirichlet_alpha,
            candidate_priors=cand_priors.tolist(),
        )
        tiles  = [int(t) for t in resp["tiles"]]
        visits = np.asarray(resp["visits"], dtype=np.float64)
        qs     = np.asarray(resp["q"],      dtype=np.float64)
        policy = np.asarray(resp["policy"], dtype=np.float64)

        if not self.deterministic and policy.sum() > 0:
            # Datagen: sample from the visit policy for exploration.
            pick = int(np.random.choice(len(tiles), p=policy / policy.sum()))
        else:
            # Eval: conservative override — keep the net's move unless another
            # well-visited candidate's tree-Q beats it by a clear margin (mirrors
            # PairedMCTSPolicy's significance gate, using lookahead-backed Q). Raw
            # argmax-visits over noisy money Q makes too many bad switches.
            min_v = max(4.0, self.sims / (3.0 * len(tiles)))
            elig = [i for i in range(len(tiles)) if visits[i] >= min_v]
            if base_idx not in elig:
                elig.append(base_idx)
            best_i = max(elig, key=lambda i: qs[i])
            pick = best_i if (qs[best_i] - qs[base_idx] > self.switch_margin) else base_idx
        action = tiles[pick]

        # Phase-1 targets in PairedMCTSPolicy's schema: centered log-visits play
        # the role of paired ΔQ, a constant SE forces the distill τ to its floor,
        # so softmax(mean_d / τ_floor) ∝ N(a)^(1/τ_floor) — the visit-count target.
        log_v  = np.log(visits + 1.0)
        mean_d = (log_v - log_v[base_idx]).tolist()
        se_d   = [1.0] * len(tiles)

        info = {
            "nn_action":  nn_action,
            "switched":   action != nn_action,
            "mean_gain":  float(qs[pick] - qs[base_idx]),
            "candidates": tiles,
            "mean_diffs": mean_d,
            "se_diffs":   se_d,
            # Raw search stats for analysis / future value-target work.
            "visits":     visits.tolist(),
            "q":          qs.tolist(),
            "prior":      [float(p) for p in resp["prior"]],
            "value":      float(resp["value"]),
            "policy":     policy.tolist(),
        }
        if action != nn_action:
            self.n_switches += 1
        return action, info

