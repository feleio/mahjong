"""
value_train.py — Train the student's value head on Monte-Carlo outcomes.

Loads a student checkpoint, FREEZES the trunk and all policy heads (so the
rollout policy the tree depends on is untouched), and trains ONLY the value
head to regress the final seat-0 balance produced by value_datagen.py. The
result is re-exported to ONNX and used as the tree's leaf value (rolloutTail
= zero), replacing the high-variance full Monte-Carlo rollout.

Accuracy gate: held-out Pearson correlation and MSE vs the constant-mean
baseline. If the value head cannot predict outcomes, the bootstrap pivot won't
help — better to learn that here than after a costly re-gate.

Usage
─────
    rl/venv/bin/python rl/value_train.py \
        --student rl/checkpoints/student/student48.pt \
        --data rl/data/value_v1/value_data.npz \
        --epochs 30 --out rl/checkpoints/student/student48_val.pt
"""

import argparse
import sys
from pathlib import Path

import numpy as np
import torch
import torch.nn.functional as F

sys.path.insert(0, str(Path(__file__).parent))

from model import MahjongAgent


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--student", required=True)
    p.add_argument("--data", required=True, nargs="+")
    p.add_argument("--epochs", type=int, default=30)
    p.add_argument("--batch", type=int, default=1024)
    p.add_argument("--lr", type=float, default=1e-3)
    p.add_argument("--val-frac", type=float, default=0.1)
    p.add_argument("--unfreeze", action="store_true",
                   help="train the whole net (value ceiling probe), not just the head")
    p.add_argument("--out", required=True)
    args = p.parse_args()

    obs_list, val_list = [], []
    for d in args.data:
        z = np.load(d)
        obs_list.append(z["obs"]); val_list.append(z["value"])
    obs = np.concatenate(obs_list).astype(np.float32)
    val = np.concatenate(val_list).astype(np.float32)
    print(f"loaded {len(val)} samples  value mean={val.mean():+.3f} std={val.std():.3f}")

    # Deterministic shuffle + split.
    rng = np.random.default_rng(0)
    perm = rng.permutation(len(val))
    obs, val = obs[perm], val[perm]
    n_val = int(len(val) * args.val_frac)
    tr_obs, tr_val = obs[n_val:], val[n_val:]
    va_obs, va_val = obs[:n_val], val[:n_val]

    dev = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    agent = MahjongAgent.load(args.student, device="cpu")
    net = agent.net.to(dev)
    net.train()
    if args.unfreeze:
        # Ceiling probe: train the whole value path (may disturb the policy — do
        # not ship this checkpoint's policy heads).
        for prm in net.parameters():
            prm.requires_grad = True
        opt = torch.optim.Adam(net.parameters(), lr=args.lr)
    else:
        # Freeze everything, then unfreeze only the value head.
        for prm in net.parameters():
            prm.requires_grad = False
        for prm in net.value_head.parameters():
            prm.requires_grad = True
        opt = torch.optim.Adam(net.value_head.parameters(), lr=args.lr)

    tr_obs_t = torch.tensor(tr_obs).to(dev); tr_val_t = torch.tensor(tr_val).to(dev)
    va_obs_t = torch.tensor(va_obs).to(dev); va_val_t = torch.tensor(va_val).to(dev)

    def val_pred(obs_t):
        preds = []
        with torch.no_grad():
            for i in range(0, len(obs_t), 4096):
                _, v = net.forward(obs_t[i:i + 4096], "discard", None)
                preds.append(v)
        return torch.cat(preds)

    baseline_mse = float(((va_val_t - tr_val_t.mean()) ** 2).mean())
    n = len(tr_val_t)
    for epoch in range(args.epochs):
        net.value_head.train()
        idx = torch.randperm(n)
        tot = 0.0
        for i in range(0, n, args.batch):
            b = idx[i:i + args.batch]
            _, v = net.forward(tr_obs_t[b], "discard", None)
            loss = F.mse_loss(v, tr_val_t[b])
            opt.zero_grad(); loss.backward(); opt.step()
            tot += loss.item() * len(b)
        # Held-out metrics.
        vp = val_pred(va_obs_t)
        va_mse = float(((vp - va_val_t) ** 2).mean())
        corr = float(np.corrcoef(vp.cpu().numpy(), va_val.astype(np.float64))[0, 1])
        print(f"epoch {epoch+1:2d}: train_mse={tot/n:7.3f}  "
              f"val_mse={va_mse:7.3f}  (baseline {baseline_mse:7.3f})  "
              f"corr={corr:.3f}", flush=True)

    r2 = 1.0 - va_mse / baseline_mse
    print(f"\nACCURACY GATE: val corr={corr:.3f}  R^2={r2:.3f}  "
          f"(val_mse {va_mse:.2f} vs baseline {baseline_mse:.2f})")
    net.to("cpu")
    agent.save(args.out)
    print(f"saved -> {args.out}")


if __name__ == "__main__":
    main()
