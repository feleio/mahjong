"""
exit_distill.py — Distill Expert-Iteration data into the imitation network.

Loads .npz shards produced by exit_datagen.py, warm-starts from an imitation
checkpoint, and fine-tunes with cross-entropy. Discard samples where MCTS
overrode the imitation policy ("switched") carry a higher sample weight —
they are the actual policy-improvement signal; everything else anchors the
network to its existing behaviour.

Usage
─────
    rl/venv/bin/python rl/exit_distill.py \
        --data-dir rl/data/exit_v1 \
        --checkpoint rl/checkpoints/imitation_v2/imitation_final.pt \
        --save-dir rl/checkpoints/exit_v1 \
        --epochs 6 --lr 1e-4 --switch-weight 3.0 --device cuda
"""

import argparse
import sys
from collections import defaultdict
from pathlib import Path

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import DataLoader, TensorDataset

sys.path.insert(0, str(Path(__file__).parent))
from env import OBS_DIM
from model import MahjongNet


def load_shards(data_dir: Path) -> dict:
    """decision -> dict(obs, act, mask, switched) concatenated across shards."""
    shards = sorted(data_dir.glob("shard_*.npz"))
    if not shards:
        raise SystemExit(f"No shards found in {data_dir}")
    per_dec = defaultdict(lambda: defaultdict(list))
    for path in shards:
        z = np.load(path)
        decs = {k.rsplit("_", 1)[0] for k in z.files}
        for dec in decs:
            for field in ("obs", "act", "mask", "switched"):
                per_dec[dec][field].append(z[f"{dec}_{field}"])
    data = {}
    for dec, fields in per_dec.items():
        data[dec] = {f: np.concatenate(v, axis=0) for f, v in fields.items()}
    print(f"Loaded {len(shards)} shards from {data_dir}:")
    for dec, d in sorted(data.items()):
        n_sw = int(d["switched"].sum())
        print(f"  {dec:10s}: {len(d['act']):>9,} samples"
              + (f"  ({n_sw:,} switched, {n_sw / len(d['act']):.1%})"
                 if dec == "discard" else ""))
    return data


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir",      default="rl/data/exit_v1")
    p.add_argument("--checkpoint",    default="rl/checkpoints/imitation_v2/imitation_final.pt")
    p.add_argument("--save-dir",      default="rl/checkpoints/exit_v1")
    p.add_argument("--epochs",        type=int,   default=6)
    p.add_argument("--lr",            type=float, default=1e-4)
    p.add_argument("--batch-size",    type=int,   default=512)
    p.add_argument("--switch-weight", type=float, default=3.0)
    p.add_argument("--device",        default="cuda")
    args = p.parse_args()

    device   = torch.device(args.device)
    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    data = load_shards(Path(args.data_dir))

    ckpt = torch.load(args.checkpoint, map_location="cpu", weights_only=False)
    hidden_size = ckpt.get("hidden_size", 512)
    n_layers    = ckpt.get("n_layers", 2)
    net = MahjongNet(obs_dim=ckpt.get("obs_dim", OBS_DIM),
                     hidden_size=hidden_size, n_layers=n_layers)
    net.load_state_dict(ckpt["net_state"])
    net = net.to(device)
    print(f"Warm-started from {args.checkpoint} "
          f"(hidden={hidden_size}, layers={n_layers})")

    optimizer = torch.optim.Adam(net.parameters(), lr=args.lr, weight_decay=1e-5)
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=args.epochs, eta_min=args.lr / 10)

    datasets = {}
    for dec, d in data.items():
        w = np.ones(len(d["act"]), dtype=np.float32)
        if dec == "discard":
            w[d["switched"]] = args.switch_weight
        datasets[dec] = TensorDataset(
            torch.tensor(d["obs"], dtype=torch.float32),
            torch.tensor(d["act"], dtype=torch.int64),
            torch.tensor(d["mask"], dtype=torch.bool),
            torch.tensor(w),
        )

    print(f"\nTraining {args.epochs} epochs, lr={args.lr}, "
          f"switch_weight={args.switch_weight}")
    for epoch in range(args.epochs):
        net.train()
        ep_loss, ep_correct, ep_total = (defaultdict(float), defaultdict(int),
                                         defaultdict(int))
        sw_correct, sw_total = 0, 0

        for dec, dataset in datasets.items():
            loader = DataLoader(dataset, batch_size=args.batch_size, shuffle=True)
            for obs_b, act_b, mask_b, w_b in loader:
                obs_b, act_b = obs_b.to(device), act_b.to(device)
                mask_b, w_b  = mask_b.to(device), w_b.to(device)

                logits, _ = net(obs_b, dec, mask_b)
                loss = (F.cross_entropy(logits, act_b, reduction="none") * w_b).mean()

                optimizer.zero_grad()
                loss.backward()
                nn.utils.clip_grad_norm_(net.parameters(), 1.0)
                optimizer.step()

                ep_loss[dec] += loss.item() * len(obs_b)
                preds = logits.argmax(dim=-1)
                ep_correct[dec] += (preds == act_b).sum().item()
                ep_total[dec]   += len(obs_b)
                if dec == "discard":
                    sw = w_b > 1.0
                    sw_correct += (preds[sw] == act_b[sw]).sum().item()
                    sw_total   += int(sw.sum().item())

        scheduler.step()
        parts = [f"{d}:{ep_correct[d]/max(ep_total[d],1):.1%}" for d in sorted(datasets)]
        print(f"  epoch {epoch+1:>2}/{args.epochs} "
              f"loss={sum(ep_loss.values())/sum(ep_total.values()):.4f} "
              f"switched_acc={sw_correct/max(sw_total,1):.1%} "
              f"[{', '.join(parts)}] lr={scheduler.get_last_lr()[0]:.1e}")
        sys.stdout.flush()

        ckpt_out = {
            "net_state":   net.state_dict(),
            "obs_dim":     OBS_DIM,
            "hidden_size": hidden_size,
            "n_layers":    n_layers,
            "epoch":       epoch + 1,
            "source":      f"exit_distill from {args.checkpoint}",
        }
        torch.save(ckpt_out, str(save_dir / f"exit_epoch{epoch+1}.pt"))

    torch.save(ckpt_out, str(save_dir / "exit_final.pt"))
    print(f"\nSaved final checkpoint → {save_dir / 'exit_final.pt'}")


if __name__ == "__main__":
    main()
