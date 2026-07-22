"""
eval_teacher_gap.py — measure the TRUE exploit gap: teacher vs frozen champions.

The freeze-trunk exploit probe showed head-only PPO cannot beat the champion
(gap ~0). But the project already owns a stronger policy: the TEACHER
(champion + 64-world CRN search). This measures the teacher's money at seat 0
against 3 frozen champions on the SAME 400-seed eval block the probe used, so
the result is directly comparable to the probe's raw-champion baseline
(+3.670 ± 1.230). teacher_money − baseline = the real, already-reachable
exploit gap — i.e. the headroom distillation is failing to transfer.

    rl/venv/bin/python rl/eval_teacher_gap.py \
        --champion rl/checkpoints/best_raw_net.pt \
        --nn-model rl/checkpoints/student/student48_sp1b.onnx \
        --n-games 400 --n-workers 4
"""

import argparse
import sys
import time
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))

_W = {}


def _worker_init(jar, ckpt, nn_model, n_worlds, top_k, z, min_gain, threads):
    import torch
    torch.set_num_threads(1)
    from env import encode_state, get_action_mask  # noqa: F401
    from model import MahjongAgent
    from mcts import MCTSRolloutClient, PairedMCTSPolicy
    from self_play import TrueSelfPlayEnv

    agent = MahjongAgent.load(ckpt, device="cpu")
    assert agent.net.obs_dim == 759, "expects a v3 net"
    client = MCTSRolloutClient(jar_path=jar, rollout_opp="firstfelix",
                               nn_model=nn_model, rollout_threads=threads)
    _W["teacher"] = PairedMCTSPolicy(
        net=agent.net, rollout_client=client, n_worlds=n_worlds, top_k=top_k,
        z_threshold=z, min_gain=min_gain, device="cpu", self_policy="nn")
    _W["champ"] = agent
    _W["sp"] = TrueSelfPlayEnv(jar)


def _play_seed(seed):
    from env import encode_state, get_action_mask
    sp, teacher, champ = _W["sp"], _W["teacher"], _W["champ"]
    n_sw = n_dec = 0
    msg = sp.reset(seed=seed)
    while True:
        if msg["type"] == "game_over":
            return float(msg["rewards"]["0"]), n_sw, n_dec
        seat = int(msg["seat_id"])
        decision, context = msg["decision"], msg.get("context", {})
        obs = encode_state(msg["state"], version=3, context=context)
        mask = get_action_mask(decision, context)
        if seat == 0:
            action, ainfo = teacher.get_action(obs, msg["state"], decision, context, mask)
            if "mean_gain" in ainfo:
                n_dec += 1
                n_sw += bool(ainfo.get("switched"))
        else:
            action = champ.select_action(obs, decision, mask, deterministic=True)
        msg = sp.step(decision, int(action))


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--champion", required=True)
    p.add_argument("--nn-model", required=True)
    p.add_argument("--n-worlds", type=int, default=64)
    p.add_argument("--top-k", type=int, default=6)
    p.add_argument("--z-threshold", type=float, default=1.5)
    p.add_argument("--min-gain", type=float, default=0.25)
    p.add_argument("--n-games", type=int, default=400)
    p.add_argument("--n-workers", type=int, default=4)
    p.add_argument("--rollout-threads", type=int, default=3)
    p.add_argument("--seed-offset", type=int, default=70_000_000,
                   help="MUST match the exploit-probe eval block for pairing")
    p.add_argument("--baseline", type=float, default=3.670,
                   help="raw-champion money on the same block (exploit probe)")
    args = p.parse_args()

    seeds = list(range(args.seed_offset, args.seed_offset + args.n_games))
    money, sw, dec = [], 0, 0
    t0 = time.time()
    print(f"teacher = {args.champion} + {args.n_worlds}w/top{args.top_k} search, "
          f"table = 3x frozen champion, n={args.n_games}, seeds@{args.seed_offset}", flush=True)
    with ProcessPoolExecutor(
        max_workers=args.n_workers, initializer=_worker_init,
        initargs=(args.jar, args.champion, args.nn_model, args.n_worlds,
                  args.top_k, args.z_threshold, args.min_gain, args.rollout_threads),
    ) as pool:
        for r, s, d in pool.map(_play_seed, seeds, chunksize=2):
            money.append(r); sw += s; dec += d
            n = len(money)
            if n % 25 == 0:
                m = np.array(money)
                print(f"[{n}/{args.n_games}] {time.time()-t0:.0f}s  "
                      f"teacher={m.mean():+.2f}±{m.std()/np.sqrt(n):.2f}  "
                      f"gap={m.mean()-args.baseline:+.2f}  "
                      f"switch={sw}/{dec} ({sw/max(1,dec):.1%})", flush=True)

    m = np.array(money)
    se = m.std() / np.sqrt(len(m))
    print(f"\nTEACHER on frozen-champion table: {m.mean():+.3f} ± {se:.3f}/game  "
          f"win {np.mean(m>0):.1%}  switch {sw}/{dec} ({sw/max(1,dec):.1%})")
    print(f"TRUE EXPLOIT GAP (teacher − raw champion {args.baseline:+.3f}): "
          f"{m.mean()-args.baseline:+.3f} ± {se:.3f}/game")


if __name__ == "__main__":
    main()
