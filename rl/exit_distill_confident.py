"""
exit_distill_confident.py — Round-3 distillation: hard labels, confident switches only.

Round 1 (hard labels, all switches) and round 2 (soft Q targets) both failed
to transfer the MCTS advantage. Round 3 hypothesis: most switches are marginal
and act as label noise; only the confidently-better overrides are learnable.

  discard label = MCTS choice   if switched AND ΔQ > max(2·SE, min_gain_hard)
                = imitation_v2's own argmax (anchor) otherwise
  other heads   = self-labels (anchor)

Usage
─────
    rl/venv/bin/python rl/exit_distill_confident.py \
        --data-dir rl/data/exit_v2 \
        --checkpoint rl/checkpoints/imitation_v2/imitation_final.pt \
        --save-dir rl/checkpoints/exit_v3_conf --device cuda
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
from exit_distill_soft import load_shards


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir",      default="rl/data/exit_v2")
    p.add_argument("--checkpoint",    default="rl/checkpoints/imitation_v2/imitation_final.pt")
    p.add_argument("--save-dir",      default="rl/checkpoints/exit_v3_conf")
    p.add_argument("--epochs",        type=int,   default=4)
    p.add_argument("--lr",            type=float, default=5e-5)
    p.add_argument("--batch-size",    type=int,   default=512)
    p.add_argument("--z-hard",        type=float, default=2.0)
    p.add_argument("--min-gain-hard", type=float, default=1.0)
    p.add_argument("--switch-weight", type=float, default=5.0)
    p.add_argument("--device",        default="cuda")
    args = p.parse_args()

    device   = torch.device(args.device)
    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    data = load_shards(Path(args.data_dir))
    d = data["discard"]
    if "cands" not in d:
        raise SystemExit("Shards lack Q-data")

    cands  = d["cands"]      # (N, K), -1 pad; col 0 = NN/base choice
    mean_d = d["mean_d"]
    se_d   = d["se_d"]
    act    = d["act"].copy()
    switched = d["switched"]

    # Chosen candidate's ΔQ/SE for switched samples
    N = len(act)
    chosen_col = np.zeros(N, dtype=np.int64)
    for k in range(cands.shape[1]):
        chosen_col = np.where(cands[:, k] == act, k, chosen_col)
    gain = mean_d[np.arange(N), chosen_col]
    se   = se_d[np.arange(N), chosen_col]

    confident = switched & (gain > np.maximum(args.z_hard * se, args.min_gain_hard))
    # Non-confident switched samples: revert label to the imitation choice
    base_act = cands[:, 0]
    act = np.where(switched & ~confident, base_act, act)

    print(f"Switched: {switched.sum():,} / {N:,} "
          f"→ confident hard labels: {confident.sum():,} "
          f"({confident.sum() / max(switched.sum(), 1):.1%} of switches, "
          f"{confident.sum() / N:.2%} of discards)")
    print(f"  confident mean gain: {gain[confident].mean():.2f} "
          f"(vs all-switch mean {gain[switched].mean():.2f})")

    weights = np.ones(N, dtype=np.float32)
    weights[confident] = args.switch_weight

    from model import load_net
    net, net_meta = load_net(args.checkpoint, device=args.device)
    print(f"Warm-started from {args.checkpoint} ({net_meta})")

    optimizer = torch.optim.Adam(net.parameters(), lr=args.lr, weight_decay=1e-5)
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=args.epochs, eta_min=args.lr / 10)

    datasets = {}
    for dec, dd in data.items():
        if dec == "discard":
            datasets[dec] = TensorDataset(
                torch.tensor(dd["obs"], dtype=torch.float32),
                torch.tensor(act, dtype=torch.int64),
                torch.tensor(dd["mask"], dtype=torch.bool),
                torch.tensor(weights),
                torch.tensor(confident, dtype=torch.bool),
            )
        else:
            n = len(dd["act"])
            datasets[dec] = TensorDataset(
                torch.tensor(dd["obs"], dtype=torch.float32),
                torch.tensor(dd["act"], dtype=torch.int64),
                torch.tensor(dd["mask"], dtype=torch.bool),
                torch.ones(n),
                torch.zeros(n, dtype=torch.bool),
            )

    print(f"\nTraining {args.epochs} epochs, lr={args.lr}, "
          f"switch_weight={args.switch_weight}")
    for epoch in range(args.epochs):
        net.train()
        ep_loss, ep_n = 0.0, 0
        agree, conf_agree, n_conf = 0, 0, 0
        for dec, dataset in datasets.items():
            loader = DataLoader(dataset, batch_size=args.batch_size, shuffle=True)
            for obs_b, act_b, mask_b, w_b, conf_b in loader:
                obs_b, act_b = obs_b.to(device), act_b.to(device)
                mask_b, w_b, conf_b = mask_b.to(device), w_b.to(device), conf_b.to(device)
                logits, _ = net(obs_b, dec, mask_b)
                loss = (F.cross_entropy(logits, act_b, reduction="none") * w_b).mean()
                optimizer.zero_grad()
                loss.backward()
                nn.utils.clip_grad_norm_(net.parameters(), 1.0)
                optimizer.step()
                ep_loss += loss.item() * len(obs_b)
                ep_n    += len(obs_b)
                preds = logits.argmax(dim=-1)
                if dec == "discard":
                    agree      += (preds == act_b).sum().item()
                    conf_agree += (preds[conf_b] == act_b[conf_b]).sum().item()
                    n_conf     += int(conf_b.sum().item())
        scheduler.step()
        print(f"  epoch {epoch+1}/{args.epochs} loss={ep_loss/ep_n:.4f} "
              f"discard_agree={agree/len(act):.1%} "
              f"confident_agree={conf_agree/max(n_conf,1):.1%} "
              f"lr={scheduler.get_last_lr()[0]:.1e}")
        sys.stdout.flush()
        torch.save(dict(net_meta,
            net_state=net.state_dict(), epoch=epoch + 1,
            source=f"exit_distill_confident from {args.checkpoint}",
        ), str(save_dir / f"exit_epoch{epoch+1}.pt"))

    import shutil
    shutil.copy(save_dir / f"exit_epoch{args.epochs}.pt", save_dir / "exit_final.pt")
    print(f"\nSaved final checkpoint → {save_dir / 'exit_final.pt'}")


if __name__ == "__main__":
    main()
