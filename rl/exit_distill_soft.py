"""
exit_distill_soft.py — Distill ExIt data using soft Q-derived policy targets.

Round 1 (exit_distill.py) showed hard one-hot labels from MCTS overrides are
too noisy to learn: only 13.9% of switched labels were reproduced and the
evaluated policy did not improve. This script instead trains the discard head
toward a Boltzmann distribution over the evaluated candidates:

    target(c) ∝ exp( ΔQ(c) / τ_i )        (ΔQ = paired mean diff vs NN choice)

with a per-sample temperature τ_i = max(tau_floor, tau_scale · mean SE_i) so
noisy evaluations produce flat (uninformative) targets and confident ones
produce sharp targets. Magnitude and confidence of the MCTS evidence are both
encoded, instead of a hard 0/1 label.

Requires shards from exit_datagen.py with Q-data (discard_cands / _mean_d /
_se_d keys — i.e. rl/data/exit_v2 or later).

Usage
─────
    rl/venv/bin/python rl/exit_distill_soft.py \
        --data-dir rl/data/exit_v2 \
        --checkpoint rl/checkpoints/imitation_v2/imitation_final.pt \
        --save-dir rl/checkpoints/exit_v2_soft \
        --epochs 6 --lr 1e-4 --device cuda
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
    shards = sorted(data_dir.glob("shard_*.npz"))
    if not shards:
        raise SystemExit(f"No shards found in {data_dir}")
    per_dec = defaultdict(lambda: defaultdict(list))
    for path in shards:
        z = np.load(path)
        for key in z.files:
            dec, field = key.rsplit("_", 1)
            # discard_mean_d / discard_se_d split as ('discard_mean','d') — fix:
            if dec.endswith("_mean") or dec.endswith("_se"):
                dec, field = dec.rsplit("_", 1)[0], dec.rsplit("_", 1)[1] + "_d"
            per_dec[dec][field].append(z[key])
    data = {}
    for dec, fields in per_dec.items():
        data[dec] = {f: np.concatenate(v, axis=0) for f, v in fields.items()}
    print(f"Loaded {len(shards)} shards from {data_dir}:")
    for dec, d in sorted(data.items()):
        extra = ""
        if dec == "discard":
            n_sw = int(d["switched"].sum())
            extra = f"  ({n_sw:,} switched, {n_sw / len(d['act']):.1%}, Q-data: {'cands' in d})"
        print(f"  {dec:10s}: {len(d['act']):>9,} samples{extra}")
    return data


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir",   default="rl/data/exit_v2")
    p.add_argument("--checkpoint", default="rl/checkpoints/imitation_v2/imitation_final.pt")
    p.add_argument("--save-dir",   default="rl/checkpoints/exit_v2_soft")
    p.add_argument("--epochs",     type=int,   default=6)
    p.add_argument("--lr",         type=float, default=1e-4)
    p.add_argument("--batch-size", type=int,   default=512)
    p.add_argument("--tau-floor",  type=float, default=1.0)
    p.add_argument("--tau-scale",  type=float, default=1.0)
    p.add_argument("--switched-weight", type=float, default=1.0,
                   help="loss weight for search-SWITCHED discard samples (the ~14% "
                        "of data carrying the teacher's edge; 1.0 = original recipe)")
    p.add_argument("--device",     default="cuda")
    args = p.parse_args()

    device   = torch.device(args.device)
    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    data = load_shards(Path(args.data_dir))
    if "cands" not in data["discard"]:
        raise SystemExit("Shards lack Q-data (discard_cands); regenerate with new exit_datagen.py")

    from model import load_net
    net, net_meta = load_net(args.checkpoint, device=args.device)
    print(f"Warm-started from {args.checkpoint} ({net_meta})")

    optimizer = torch.optim.Adam(net.parameters(), lr=args.lr, weight_decay=1e-5)
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=args.epochs, eta_min=args.lr / 10)

    # ── Precompute soft targets for discard ──────────────────────────────────
    d = data["discard"]
    cands  = torch.tensor(d["cands"],  dtype=torch.int64)   # (N, K), -1 = pad
    mean_d = torch.tensor(d["mean_d"], dtype=torch.float32) # (N, K)
    se_d   = torch.tensor(d["se_d"],   dtype=torch.float32) # (N, K), pad = 1e9
    valid  = cands >= 0
    se_masked = torch.where(valid, se_d, torch.zeros_like(se_d))
    n_valid   = valid.sum(dim=1).clamp(min=1).float()
    tau = (args.tau_scale * se_masked.sum(dim=1) / n_valid).clamp(min=args.tau_floor)
    scores = torch.where(valid, mean_d / tau.unsqueeze(1),
                         torch.full_like(mean_d, -1e9))
    targets = F.softmax(scores, dim=1)                       # (N, K), pads → 0

    datasets = {}
    for dec, dd in data.items():
        base = [
            torch.tensor(dd["obs"], dtype=torch.float32),
            torch.tensor(dd["act"], dtype=torch.int64),
            torch.tensor(dd["mask"], dtype=torch.bool),
        ]
        if dec == "discard":
            base += [cands.clamp(min=0), targets,
                     torch.tensor(d["switched"], dtype=torch.bool)]
        datasets[dec] = TensorDataset(*base)

    print(f"\nTraining {args.epochs} epochs, lr={args.lr}, "
          f"tau_floor={args.tau_floor}, tau_scale={args.tau_scale}")
    print(f"  mean tau={tau.mean():.2f}  "
          f"sharp targets (max>0.5): {(targets.max(dim=1).values > 0.5).float().mean():.1%}")

    for epoch in range(args.epochs):
        net.train()
        ep_loss, ep_n = defaultdict(float), defaultdict(int)
        agree_nn, agree_sw, n_sw = 0, 0, 0

        for dec, dataset in datasets.items():
            loader = DataLoader(dataset, batch_size=args.batch_size, shuffle=True)
            for batch in loader:
                batch = [b.to(device) for b in batch]
                if dec == "discard":
                    obs_b, act_b, mask_b, cand_b, tgt_b, sw_b = batch
                    logits, _ = net(obs_b, dec, mask_b)
                    logp = F.log_softmax(logits, dim=-1)             # (B, 34)
                    logp_c = torch.gather(logp, 1, cand_b)           # (B, K)
                    # padded candidates: tgt=0 but logp=-inf → mask to avoid 0*inf=nan
                    contrib = torch.where(tgt_b > 0, tgt_b * logp_c,
                                          torch.zeros_like(logp_c))
                    per_sample = -contrib.sum(dim=1)                 # (B,)
                    if args.switched_weight != 1.0:
                        w = torch.where(sw_b,
                                        torch.full_like(per_sample, args.switched_weight),
                                        torch.ones_like(per_sample))
                        loss = (per_sample * w).sum() / w.sum()
                    else:
                        loss = per_sample.mean()
                    preds = logits.argmax(dim=-1)
                    agree_nn += (preds == act_b).sum().item()
                    agree_sw += (preds[sw_b] == act_b[sw_b]).sum().item()
                    n_sw     += int(sw_b.sum().item())
                else:
                    obs_b, act_b, mask_b = batch
                    logits, _ = net(obs_b, dec, mask_b)
                    loss = F.cross_entropy(logits, act_b)

                optimizer.zero_grad()
                loss.backward()
                nn.utils.clip_grad_norm_(net.parameters(), 1.0)
                optimizer.step()
                ep_loss[dec] += loss.item() * len(batch[0])
                ep_n[dec]    += len(batch[0])

        scheduler.step()
        print(f"  epoch {epoch+1:>2}/{args.epochs} "
              f"discard_loss={ep_loss['discard']/max(ep_n['discard'],1):.4f} "
              f"act_agree={agree_nn/max(ep_n['discard'],1):.1%} "
              f"switched_agree={agree_sw/max(n_sw,1):.1%} "
              f"lr={scheduler.get_last_lr()[0]:.1e}")
        sys.stdout.flush()

        ckpt_out = dict(net_meta,
            net_state=net.state_dict(), epoch=epoch + 1,
            source=f"exit_distill_soft from {args.checkpoint}",
        )
        torch.save(ckpt_out, str(save_dir / f"exit_epoch{epoch+1}.pt"))

    torch.save(ckpt_out, str(save_dir / "exit_final.pt"))
    print(f"\nSaved final checkpoint → {save_dir / 'exit_final.pt'}")


if __name__ == "__main__":
    main()
