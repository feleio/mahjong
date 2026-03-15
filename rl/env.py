"""
MahjongEnv — OpenAI Gym-compatible environment wrapper.

Launches the Scala RLGymServer as a subprocess and communicates
over stdin/stdout using newline-delimited JSON.

Observation space
─────────────────
A flat float32 vector of length OBS_DIM (485):
  hand[34]            – count of each tile type in the agent's hand (0-4, normalised)
  my_pong[34]         – 1 if agent has a pong of tile i
  my_kong[34]         – 1 if agent has a kong of tile i
  my_chow[34]         – 1 if tile i is part of one of agent's chow groups
  opp{j}_pong[34]     – same for each of 3 opponents (j=0,1,2)
  opp{j}_kong[34]
  opp{j}_chow[34]
  discarded[34]       – total tiles discarded of each type (normalised)
  remaining[1]        – remaining tiles normalised to [0, 1]
  my_id_onehot[4]     – one-hot of agent's seat
  cur_pid_onehot[4]   – one-hot of current player

Action spaces (per decision type)
──────────────────────────────────
  discard    : Discrete(34)        – tile ID to discard (must be in hand)
  win        : Discrete(2)         – 0=pass, 1=win
  self_win   : Discrete(2)         – 0=pass, 1=win
  pong       : Discrete(2)         – 0=pass, 1=pong
  kong       : Discrete(2)         – 0=pass, 1=kong
  chow       : Discrete(4)         – 0=pass, 1=LEFT, 2=MIDDLE, 3=RIGHT
  self_kong  : Discrete(35)        – 0=pass, 1–34 = tile_id+1
"""

import json
import subprocess
from typing import Any, Dict, List, Optional, Tuple

import numpy as np

# ── Constants ─────────────────────────────────────────────────────────────────

NUM_TILE_TYPES = 34

# flat observation vector length
OBS_DIM = (
    NUM_TILE_TYPES              # hand
    + NUM_TILE_TYPES * 3        # my groups (pong / kong / chow)
    + NUM_TILE_TYPES * 3 * 3   # 3 opponents' groups (pong/kong/chow each)
    + NUM_TILE_TYPES            # discard pile
    + 1                         # remaining tiles (normalised)
    + 4                         # my_id one-hot
    + 4                         # cur_player_id one-hot
)  # = 485

DECISION_SPACES: Dict[str, int] = {
    "discard":   34,   # tile ID 0–33
    "win":        2,   # 0=pass, 1=win
    "self_win":   2,
    "pong":       2,
    "kong":       2,
    "chow":       4,   # 0=pass, 1=LEFT, 2=MIDDLE, 3=RIGHT
    "self_kong": 35,   # 0=pass, 1–34 = tile_id+1
}

TOTAL_TILES = 136  # used to normalise "remaining"


# ── State encoder ─────────────────────────────────────────────────────────────

def encode_state(state: Dict[str, Any]) -> np.ndarray:
    """Convert a raw state dict (from the Scala server) into a float32 vector."""
    obs = np.zeros(OBS_DIM, dtype=np.float32)
    ptr = 0

    def write_segment(arr: np.ndarray) -> None:
        nonlocal ptr
        n = len(arr)
        obs[ptr: ptr + n] = arr
        ptr += n

    # Hand counts (normalised by 4)
    hand = np.array(state["hand"], dtype=np.float32) / 4.0
    write_segment(hand)

    # My fixed groups
    my_groups = state.get("my_groups", {})

    my_pong = np.zeros(NUM_TILE_TYPES, dtype=np.float32)
    for t in my_groups.get("pongs", []):
        my_pong[int(t)] = 1.0
    write_segment(my_pong)

    my_kong = np.zeros(NUM_TILE_TYPES, dtype=np.float32)
    for t in my_groups.get("kongs", []):
        my_kong[int(t)] = 1.0
    write_segment(my_kong)

    my_chow = np.zeros(NUM_TILE_TYPES, dtype=np.float32)
    for chow_group in my_groups.get("chows", []):
        for t in chow_group:
            my_chow[int(t)] = 1.0
    write_segment(my_chow)

    # Opponents' fixed groups (3 opponents)
    opp_groups = state.get("opp_groups", [{}, {}, {}])
    for opp in (opp_groups + [{}, {}, {}])[:3]:  # pad to 3 if shorter
        op = np.zeros(NUM_TILE_TYPES, dtype=np.float32)
        for t in opp.get("pongs", []):
            op[int(t)] = 1.0
        write_segment(op)

        ok = np.zeros(NUM_TILE_TYPES, dtype=np.float32)
        for t in opp.get("kongs", []):
            ok[int(t)] = 1.0
        write_segment(ok)

        oc = np.zeros(NUM_TILE_TYPES, dtype=np.float32)
        for chow_group in opp.get("chows", []):
            for t in chow_group:
                oc[int(t)] = 1.0
        write_segment(oc)

    # Discard counts (normalised by 4)
    discarded = np.array(state["discarded"], dtype=np.float32) / 4.0
    write_segment(discarded)

    # Remaining tiles (normalised)
    write_segment(np.array([state["remaining"] / TOTAL_TILES], dtype=np.float32))

    # My seat one-hot
    my_oh = np.zeros(4, dtype=np.float32)
    my_oh[int(state["my_id"])] = 1.0
    write_segment(my_oh)

    # Current player one-hot
    cur_oh = np.zeros(4, dtype=np.float32)
    cur_oh[int(state["cur_player_id"])] = 1.0
    write_segment(cur_oh)

    assert ptr == OBS_DIM, f"Observation dim mismatch: {ptr} != {OBS_DIM}"
    return obs


def get_action_mask(decision: str, context: Dict[str, Any]) -> np.ndarray:
    """Return a boolean mask indicating which actions are legal."""
    n = DECISION_SPACES[decision]
    mask = np.zeros(n, dtype=bool)

    if decision == "discard":
        for t in context.get("valid_tiles", []):
            idx = int(t)
            if 0 <= idx < n:
                mask[idx] = True
    elif decision in ("win", "self_win", "pong", "kong"):
        mask[:] = True  # both pass and accept are always syntactically valid
    elif decision == "chow":
        mask[0] = True  # pass
        # ChowPosition IDs: LEFT=0, MIDDLE=1, RIGHT=2 → action indices 1,2,3
        for p in context.get("positions", []):
            idx = int(p) + 1
            if 1 <= idx < n:
                mask[idx] = True
    elif decision == "self_kong":
        mask[0] = True  # pass
        for t in context.get("valid_tiles", []):
            idx = int(t) + 1
            if 1 <= idx < n:
                mask[idx] = True
    return mask


# ── Environment ───────────────────────────────────────────────────────────────

class MahjongEnv:
    """
    Wraps the Scala RLGymServer as a gym-like environment.

    Parameters
    ----------
    jar_path : str
        Path to the assembled fat-JAR produced by ``sbt assembly``.
    java_bin : str
        Path to the ``java`` executable (default: ``"java"``).

    Example
    -------
    >>> env = MahjongEnv("target/scala-2.12/mahjong-assembly-0.1.0.jar")
    >>> obs, info = env.reset(seed=0)
    >>> while True:
    ...     mask  = info["action_mask"]
    ...     valid = np.where(mask)[0]
    ...     action = int(np.random.choice(valid))
    ...     obs, reward, done, info = env.step(action)
    ...     if done:
    ...         print("Reward:", reward)
    ...         break
    >>> env.close()
    """

    # Public constants
    obs_dim = OBS_DIM
    decision_spaces = DECISION_SPACES

    def __init__(self, jar_path: str, java_bin: str = "java") -> None:
        self.jar_path = jar_path
        self.java_bin = java_bin
        self._proc: Optional[subprocess.Popen] = None
        self._pending_decision: Optional[str] = None

    # ── Process management ─────────────────────────────────────────────────────

    def _ensure_server(self) -> None:
        if self._proc is not None and self._proc.poll() is None:
            return
        self._proc = subprocess.Popen(
            [self.java_bin, "-cp", self.jar_path,
             "io.fele.app.mahjong.rl.RLGymServer"],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            bufsize=1,
            text=True,
        )

    def _send(self, msg: Dict[str, Any]) -> None:
        assert self._proc is not None
        self._proc.stdin.write(json.dumps(msg) + "\n")
        self._proc.stdin.flush()

    def _recv(self) -> Dict[str, Any]:
        assert self._proc is not None
        line = self._proc.stdout.readline()
        if not line:
            stderr = self._proc.stderr.read()
            raise RuntimeError(
                f"Scala server closed unexpectedly.\nstderr:\n{stderr}"
            )
        return json.loads(line.strip())

    def close(self) -> None:
        """Terminate the Scala subprocess."""
        if self._proc is not None:
            try:
                self._proc.stdin.close()
                self._proc.wait(timeout=5)
            except Exception:
                self._proc.kill()
            self._proc = None

    # ── Gym interface ──────────────────────────────────────────────────────────

    def reset(self, seed: Optional[int] = None) -> Tuple[np.ndarray, Dict]:
        """
        Start a new game and return ``(observation, info)``.

        ``info`` keys:
            decision    – str, which decision the agent must make
            context     – dict with tile_id, score, valid_tiles, positions …
            action_mask – bool ndarray, True for each valid action
            state       – raw state dict
        """
        self._ensure_server()
        cmd: Dict[str, Any] = {"cmd": "reset"}
        if seed is not None:
            cmd["seed"] = seed
        self._send(cmd)
        return self._parse_observation(self._recv())

    def step(self, action: int) -> Tuple[np.ndarray, float, bool, Dict]:
        """
        Submit ``action`` (an integer index) and advance the game.

        Returns ``(observation, reward, done, info)``.
        When ``done=True``, ``observation`` is all-zeros and ``reward`` is
        the final game balance for the RL agent (positive = profit).
        Call ``reset()`` to start a new game.
        """
        if self._pending_decision is None:
            raise RuntimeError("Call reset() before step()")
        reply = self._action_to_json(self._pending_decision, action)
        self._send(reply)
        msg = self._recv()

        if msg["type"] == "observation":
            obs, info = self._parse_observation(msg)
            return obs, 0.0, False, info
        elif msg["type"] == "game_over":
            self._pending_decision = None
            reward = float(msg.get("reward", 0.0))
            info = {
                "decision":    None,
                "action_mask": None,
                "context":     {},
                "state":       msg.get("state"),
                "winner_ids":  msg.get("winner_ids", []),
                "loser_id":    msg.get("loser_id"),
                "is_self_win": msg.get("is_self_win", False),
            }
            return np.zeros(OBS_DIM, dtype=np.float32), reward, True, info
        else:
            raise RuntimeError(f"Unexpected server message type: {msg['type']}")

    # ── Internal helpers ───────────────────────────────────────────────────────

    def _parse_observation(self, msg: Dict[str, Any]) -> Tuple[np.ndarray, Dict]:
        assert msg["type"] == "observation", (
            f"Expected 'observation' message, got: {msg.get('type')}"
        )
        decision = msg["decision"]
        context  = msg.get("context", {})
        state    = msg["state"]

        self._pending_decision = decision

        obs = encode_state(state)
        info = {
            "decision":    decision,
            "context":     context,
            "action_mask": get_action_mask(decision, context),
            "state":       state,
        }
        return obs, info

    def _action_to_json(self, decision: str, action: int) -> Dict[str, Any]:
        """Map an integer action index to the JSON the Scala server expects."""
        if decision in ("win", "self_win", "pong", "kong"):
            return {"action": bool(action)}
        elif decision == "discard":
            return {"action": action}
        elif decision == "chow":
            # 0=pass → null, 1=LEFT(0), 2=MIDDLE(1), 3=RIGHT(2)
            pos_id = (action - 1) if action > 0 else None
            return {"action": pos_id}
        elif decision == "self_kong":
            # 0=pass → null, 1..34 → tile_id 0..33
            tile_id = (action - 1) if action > 0 else None
            return {"action": tile_id}
        else:
            raise ValueError(f"Unknown decision type: {decision!r}")

    # ── Context manager ────────────────────────────────────────────────────────

    def __enter__(self) -> "MahjongEnv":
        return self

    def __exit__(self, *_) -> None:
        self.close()

    def __repr__(self) -> str:
        return f"MahjongEnv(jar={self.jar_path!r}, obs_dim={OBS_DIM})"
