"""
test_v4obs.py — Parity test: Scala V4Obs.encode must reproduce Python
encode_state(version=4) exactly (within float32 tolerance) on real states.

Plays random-valid-action games vs FirstFelix, and for every decision sends
the raw state dict to the rollout server's `encode_v4` command, comparing the
returned vector with the local encoding. Random play (rather than a policy)
maximises the diversity of discard sequences the order-planes see.

Also asserts the v3 prefix is untouched: obs_v4[:759] == encode_state(v3).
"""

import json
import subprocess
import sys
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

from env import MahjongEnv, encode_state, OBS_DIM_V3  # noqa: E402

JAR = "target/scala-2.12/mahjong-assembly-0.1.0.jar"


def main():
    proc = subprocess.Popen(
        ["java", "-cp", JAR, "io.fele.app.mahjong.rl.MCTSRolloutServer"],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE,
        stderr=subprocess.DEVNULL, text=True, bufsize=1)

    def scala_encode(state, context):
        cmd = {
            "cmd": "encode_v4",
            "hand": state["hand"],
            "my_groups": state["my_groups"],
            "opp_groups": state["opp_groups"],
            "discarded_by_player": state["discarded_by_player"],
            "discard_seq": state["discard_seq"],
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

    env = MahjongEnv(JAR, opponent="firstfelix", obs_version=4)
    rng = np.random.default_rng(0)

    n_checked, worst, max_seq = 0, 0.0, 0
    for seed in range(30):
        try:
            _, info = env.reset(seed=6_666_000 + seed)
        except AssertionError:
            continue
        while True:
            state, context = info["state"], info["context"]
            max_seq = max(max_seq, len(state["discard_seq"]))
            py = encode_state(state, version=4, context=context)
            v3 = encode_state(state, version=3, context=context)
            assert np.array_equal(py[:OBS_DIM_V3], v3), \
                f"v4 changed the v3 prefix (seed={seed})"
            sc = scala_encode(state, context)
            err = float(np.abs(py - sc).max())
            worst = max(worst, err)
            if err > 1e-5:
                bad = np.where(np.abs(py - sc) > 1e-5)[0]
                print(f"MISMATCH seed={seed} decision={info['decision']} "
                      f"n_seq={len(state['discard_seq'])} dims={bad[:10]} "
                      f"py={py[bad[:5]]} sc={sc[bad[:5]]}")
                return 1
            n_checked += 1
            action = int(rng.choice(np.where(info["action_mask"])[0]))
            _, _, done, info = env.step(action)
            if done:
                break

    print(f"PARITY OK: {n_checked} decisions checked, max err {worst:.2e}, "
          f"longest discard_seq {max_seq}")
    env.close()
    proc.kill()
    return 0


if __name__ == "__main__":
    sys.exit(main())
