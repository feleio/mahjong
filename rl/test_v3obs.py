"""
test_v3obs.py — Parity test: Scala V3Obs.encode must reproduce Python
encode_state(version=3) exactly (within float32 tolerance) on real states.

Plays a few games vs FirstFelix, and for every decision sends the raw state
dict to the rollout server's `encode_v3` command, comparing the returned
vector with the local encoding.
"""

import json
import subprocess
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

from env import MahjongEnv, encode_state
from model import MahjongAgent

JAR = "target/scala-2.12/mahjong-assembly-0.1.0.jar"


def main():
    proc = subprocess.Popen(
        ["java", "-cp", JAR, "io.fele.app.mahjong.rl.MCTSRolloutServer"],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL, text=True, bufsize=1)

    def scala_encode(state, context):
        cmd = {
            "cmd": "encode_v3",
            "hand": state["hand"],
            "my_groups": state["my_groups"],
            "opp_groups": state["opp_groups"],
            "discarded_by_player": state["discarded_by_player"],
            "remaining": state["remaining"],
            "my_id": state["my_id"],
            "cur_player_id": state["cur_player_id"],
        }
        if context and context.get("tile_id") is not None:
            cmd["context_tile"] = int(context["tile_id"])
        proc.stdin.write(json.dumps(cmd) + "\n")
        proc.stdin.flush()
        return np.array(json.loads(proc.stdout.readline())["obs"],
                        dtype=np.float32)

    env = MahjongEnv(JAR, opponent="firstfelix", obs_version=3)
    agent = MahjongAgent.load("rl/checkpoints/exit_v3_r3_soft/exit_final.pt",
                              device="cpu")

    n_checked, worst = 0, 0.0
    for seed in range(20):
        _, info = env.reset(seed=7_777_000 + seed)
        while True:
            state, context = info["state"], info["context"]
            py = encode_state(state, version=3, context=context)
            sc = scala_encode(state, context)
            err = float(np.abs(py - sc).max())
            worst = max(worst, err)
            if err > 1e-5:
                bad = np.where(np.abs(py - sc) > 1e-5)[0]
                print(f"MISMATCH seed={seed} decision={info['decision']} "
                      f"dims={bad[:10]} py={py[bad[:5]]} sc={sc[bad[:5]]}")
                return 1
            n_checked += 1
            action = agent.select_action(py, info["decision"],
                                         info["action_mask"],
                                         deterministic=True)
            _, _, done, info = env.step(action)
            if done:
                break

    print(f"PARITY OK: {n_checked} decisions checked, max err {worst:.2e}")
    env.close()
    proc.kill()
    return 0


if __name__ == "__main__":
    sys.exit(main())
