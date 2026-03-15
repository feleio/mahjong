"""
vec_env.py — Vectorised Mahjong environment for parallel rollout collection.

Runs N independent MahjongEnv instances in background threads.  Decisions
from all envs are batched together so the model performs a single (larger)
GPU forward pass per step instead of N individual batch-size-1 passes.

Typical speedup over a single env: ~N× throughput, higher GPU utilisation.

Usage
─────
    with VecMahjongEnv(jar, n_envs=4) as vec:
        # see PPOTrainer.collect_rollout_vec in ppo.py
        ...
"""

import queue
import threading
from typing import Any, Dict, List, Optional, Tuple

from env import MahjongEnv

# Each env thread sends one of these on the shared request queue:
#   (env_id, obs, info)  → main thread should run inference and send action
# Each env thread sends one of these on the shared result queue:
#   (env_id, obs, info, obs_next, reward, done, info_next)

_SENTINEL = object()  # poison-pill to stop worker threads


class VecMahjongEnv:
    """N parallel MahjongEnv instances driven by a single batched inference loop."""

    def __init__(
        self,
        jar_path: str,
        n_envs: int = 4,
        java_bin: str = "java",
        opponent: str = "chicken",
    ) -> None:
        self.n_envs   = n_envs
        self._envs    = [
            MahjongEnv(jar_path, java_bin=java_bin, opponent=opponent)
            for _ in range(n_envs)
        ]

        # env thread → main: requests for inference
        self._req_q: queue.Queue = queue.Queue()

        # main → env thread i: the chosen action
        self._act_qs: List[queue.Queue] = [queue.Queue() for _ in range(n_envs)]

        # env thread → main: (env_id, obs_before, info_before, obs_after, reward, done, info_after)
        self._res_q: queue.Queue = queue.Queue()

        self._stop = threading.Event()
        self._threads = [
            threading.Thread(target=self._run, args=(i,), daemon=True)
            for i in range(n_envs)
        ]
        for t in self._threads:
            t.start()

    # ── Worker ────────────────────────────────────────────────────────────────

    def _run(self, env_id: int) -> None:
        env  = self._envs[env_id]
        seed = env_id

        while not self._stop.is_set():
            try:
                obs, info = env.reset(seed=seed)
            except Exception:
                if self._stop.is_set():
                    return
                raise
            seed += self.n_envs

            while not self._stop.is_set():
                # Ask main thread for an action
                self._req_q.put((env_id, obs, info))

                # Wait for the action (with timeout so we can check stop flag)
                action = None
                while action is None:
                    try:
                        action = self._act_qs[env_id].get(timeout=5)
                    except queue.Empty:
                        if self._stop.is_set():
                            return

                obs_next, reward, done, info_next = env.step(action)

                # Report the full transition (obs_before included for easy bookkeeping)
                self._res_q.put((env_id, obs, info, obs_next, reward, done, info_next))

                if done:
                    break
                obs, info = obs_next, info_next

    # ── Main-thread API ───────────────────────────────────────────────────────

    def get_decisions(self) -> List[Tuple]:
        """
        Block until at least one env needs an action, then drain the queue.
        Returns a list of (env_id, obs, info).
        """
        items = []
        # Block for the first item
        try:
            items.append(self._req_q.get(timeout=30))
        except queue.Empty:
            return items
        # Drain any immediately available extras (no blocking)
        while True:
            try:
                items.append(self._req_q.get_nowait())
            except queue.Empty:
                break
        return items

    def send_actions(self, env_id_action_pairs: List[Tuple[int, int]]) -> None:
        """Deliver actions to the respective env threads."""
        for env_id, action in env_id_action_pairs:
            self._act_qs[env_id].put(action)

    def get_results(self, n: int) -> List[Tuple]:
        """
        Collect exactly n step results.
        Each result is (env_id, obs, info, obs_next, reward, done, info_next).
        """
        results = []
        while len(results) < n:
            try:
                results.append(self._res_q.get(timeout=30))
            except queue.Empty:
                if self._stop.is_set():
                    break
        return results

    # ── Lifecycle ─────────────────────────────────────────────────────────────

    def close(self) -> None:
        self._stop.set()
        for env in self._envs:
            try:
                env.close()
            except Exception:
                pass

    def __enter__(self) -> "VecMahjongEnv":
        return self

    def __exit__(self, *args: Any) -> None:
        self.close()
