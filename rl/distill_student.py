"""
distill_student.py — Distill the big CNN into a small fast rollout policy
(AlphaGo-style). The student matches the teacher's soft action distributions
on states drawn from existing data shards; it only needs to be a faithful,
fast stand-in for rollouts, not a better player.

Usage
─────
    rl/venv/bin/python rl/distill_student.py \
        --teacher rl/checkpoints/exit_v3_r3_soft/exit_final.pt \
        --data rl/data/ff_v3c rl/data/exit_v3 rl/data/exit_v3_r2 rl/data/exit_v3_r3 \
        --channels 48 --n-blocks 2 --epochs 4 \
        --out rl/checkpoints/student/student.pt
"""

import argparse
import sys
from collections import defaultdict
from pathlib import Path

import numpy as np
import torch
import torch.nn.functional as F

sys.path.insert(0, str(Path(__file__).parent))

from env import DECISION_SPACES, OBS_DIM_V3
from model import MahjongAgent, MahjongConvNet, load_net

DECISIONS = list(DECISION_SPACES.keys())


def iter_shards(dirs):
    for d in dirs:
        for shard in sorted(Path(d).glob("*.npz")):
            yield np.load(shard)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--teacher", required=True)
    p.add_argument("--data", nargs="+", required=True)
    p.add_argument("--channels", type=int, default=48)
    p.add_argument("--n-blocks", type=int, default=2)
    p.add_argument("--epochs", type=int, default=4)
    p.add_argument("--batch-size", type=int, default=2048)
    p.add_argument("--lr", type=float, default=3e-4)
    p.add_argument("--max-per-decision", type=int, default=900_000)
    p.add_argument("--balance-min", type=int, default=150_000)
    p.add_argument("--out", required=True)
    p.add_argument("--device", default="cuda")
    args = p.parse_args()

    device = torch.device(args.device)
    teacher, _ = load_net(args.teacher, device=str(device))
    teacher.eval()

    student = MahjongConvNet(obs_dim=OBS_DIM_V3, channels=args.channels,
                             n_blocks=args.n_blocks).to(device)
    opt = torch.optim.Adam(student.parameters(), lr=args.lr)

    # ── Load obs per decision (labels come from the teacher, not the data) ────
    obs_by_dec = defaultdict(list)
    mask_by_dec = defaultdict(list)
    counts = defaultdict(int)
    for d in iter_shards(args.data):
        for dec in DECISIONS:
            key = f"{dec}_obs"
            if key not in d.files or counts[dec] >= args.max_per_decision:
                continue
            obs = d[key]
            if obs.shape[1] != OBS_DIM_V3:
                continue
            take = min(len(obs), args.max_per_decision - counts[dec])
            # keep fp16 in RAM (float32 for 2.7M×759 obs OOMs a 32GB box);
            # batches are upcast on the GPU
            obs_by_dec[dec].append(obs[:take].astype(np.float16))
            mask_by_dec[dec].append(d[f"{dec}_mask"][:take])
            counts[dec] += take

    data = {}
    for dec in list(obs_by_dec.keys()):
        o = np.concatenate(obs_by_dec[dec])
        del obs_by_dec[dec]
        m = np.concatenate(mask_by_dec[dec])
        del mask_by_dec[dec]
        # oversample rare decision types
        rep = int(np.clip(args.balance_min // max(len(o), 1), 1, 25))
        data[dec] = (o, m, rep)
        print(f"{dec:>10}: {len(o):>9,} samples  x{rep}")
    sys.stdout.flush()

    steps = 0
    for epoch in range(args.epochs):
        tot = defaultdict(list)
        order = []
        for dec, (o, m, rep) in data.items():
            n = len(o)
            for _ in range(rep):
                idx = np.random.permutation(n)
                for s in range(0, n, args.batch_size):
                    order.append((dec, idx[s:s + args.batch_size]))
        np.random.shuffle(order)

        for dec, bidx in order:
            o, m, _ = data[dec]
            obs_t = torch.tensor(o[bidx], device=device).float()
            mask_t = torch.tensor(m[bidx], device=device)
            with torch.no_grad():
                t_logits, t_value = teacher.forward(obs_t, dec, None)
                t_logits = t_logits.masked_fill(~mask_t, -30.0)
                t_prob = torch.softmax(t_logits, dim=-1)
            s_logits, s_value = student.forward(obs_t, dec, None)
            s_logp = torch.log_softmax(
                s_logits.masked_fill(~mask_t, -30.0), dim=-1)
            loss = (-(t_prob * s_logp).sum(-1).mean()
                    + 0.5 * F.mse_loss(s_value, t_value))
            opt.zero_grad()
            loss.backward()
            opt.step()
            steps += 1
            with torch.no_grad():
                s_masked = s_logits.masked_fill(~mask_t, -30.0)
                agree = (s_masked.argmax(-1) == t_logits.argmax(-1)).float()
            tot[f"{dec}_agree"].append(agree.mean().item())
            tot["loss"].append(loss.item())

        msg = f"epoch {epoch + 1}: loss={np.mean(tot['loss']):.4f} " + " ".join(
            f"{dec}={np.mean(tot[f'{dec}_agree']):.1%}"
            for dec in data if f"{dec}_agree" in tot)
        print(msg)
        sys.stdout.flush()

    out = Path(args.out)
    out.parent.mkdir(parents=True, exist_ok=True)
    agent = MahjongAgent.__new__(MahjongAgent)
    agent.device, agent.arch, agent.net = device, "conv", student
    agent.save(str(out))
    print(f"saved -> {out}")


if __name__ == "__main__":
    main()
