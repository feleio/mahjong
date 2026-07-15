"""
exit_distill_v4.py — soft-target ExIt distillation + danger auxiliary heads
for the v4 obs retrain (issue #21).

Same soft Q-derived discard targets as exit_distill_soft.py, plus on EVERY
decision row two supervised auxiliary losses from the simulator ground truth
logged by exit_datagen4.py:

    L = L_policy + λ_t · BCE(tenpai_logits, opp_tenpai)
                 + λ_w · BCE(wait_logits,   opp_waits)     [pos-weighted]

Both BCEs use pos_weight = n_neg/n_pos computed from the corpus — tenpai is
~5% positive and waits ~1.3/34 tiles, so unweighted BCE would collapse to
all-negative. A held-out shard fraction reports tenpai/wait quality per
epoch (the standalone head-quality gate of #21).

The base checkpoint is the v3 champion: it is warm-started into the v4 net
via warm_start_v4_from_v3 (zero-init new input channels → exactly the
champion's function at epoch 0, see model.py). Pass a v4 checkpoint to
resume instead.

Usage
─────
    rl/venv/bin/python rl/exit_distill_v4.py \
        --data-dir rl/data/exit_v4 \
        --checkpoint rl/checkpoints/exit_sp1b_soft/exit_final.pt \
        --save-dir rl/checkpoints/exit_v4 \
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
from env import OBS_DIM_V3, OBS_DIM_V4  # noqa: E402


def load_shards(data_dir: Path, val_frac: float) -> tuple:
    shards = sorted(data_dir.glob("shard_*.npz"))
    if not shards:
        raise SystemExit(f"No shards found in {data_dir}")
    n_val = max(1, int(len(shards) * val_frac)) if len(shards) > 1 else 0
    splits = {"train": shards[: len(shards) - n_val],
              "val": shards[len(shards) - n_val:]}
    out = {}
    for split, paths in splits.items():
        per_dec = defaultdict(lambda: defaultdict(list))
        for path in paths:
            z = np.load(path)
            for key in z.files:
                dec, field = key.rsplit("_", 1)
                if dec.endswith("_mean") or dec.endswith("_se"):
                    dec, field = dec.rsplit("_", 1)[0], dec.rsplit("_", 1)[1] + "_d"
                per_dec[dec][field].append(z[key])
        out[split] = {dec: {f: np.concatenate(v, axis=0) for f, v in fields.items()}
                      for dec, fields in per_dec.items()}
    print(f"Loaded {len(shards)} shards from {data_dir} "
          f"({len(splits['train'])} train / {len(splits['val'])} val):")
    for dec, d in sorted(out["train"].items()):
        assert "tenpai" in d and "waits" in d, \
            f"'{dec}' rows lack danger labels — regenerate with exit_datagen4.py"
        print(f"  {dec:10s}: {len(d['act']):>9,} train samples  "
              f"tenpai+ {d['tenpai'].mean():.1%}  waits+ {d['waits'].mean():.2%}")
    return out["train"], out["val"]


def danger_losses(tp_logits, w_logits, tp_b, w_b, tp_posw, w_posw):
    l_t = F.binary_cross_entropy_with_logits(tp_logits, tp_b, pos_weight=tp_posw)
    l_w = F.binary_cross_entropy_with_logits(w_logits, w_b, pos_weight=w_posw)
    return l_t, l_w


@torch.no_grad()
def eval_danger(net, val_data, device, batch_size=1024):
    """Head quality on held-out rows: tenpai AUC + wait hit@1 among tenpai."""
    net.eval()
    tps, tls, hit1, n_tp = [], [], 0, 0
    for dec, dd in val_data.items():
        obs = torch.tensor(dd["obs"], dtype=torch.float32)
        tp = torch.tensor(dd["tenpai"], dtype=torch.float32)
        wt = torch.tensor(dd["waits"], dtype=torch.float32)
        for i in range(0, len(obs), batch_size):
            o = obs[i:i + batch_size].to(device)
            tpl, wl = net.forward_danger(o)
            tps.append(torch.sigmoid(tpl).cpu().flatten())
            tls.append(tp[i:i + batch_size].flatten())
            # wait hit@1: for each truly-tenpai opponent, does the head's
            # top-1 wait tile belong to the true wait set?
            top1 = wl.argmax(dim=2).cpu()                      # (B, 3)
            w_true = wt[i:i + batch_size]                      # (B, 3, 34)
            t_true = tp[i:i + batch_size].bool()               # (B, 3)
            sel = w_true.gather(2, top1.unsqueeze(-1)).squeeze(-1)  # (B, 3)
            hit1 += int(sel[t_true].sum().item())
            n_tp += int(t_true.sum().item())
    p = torch.cat(tps).numpy()
    y = torch.cat(tls).numpy()
    # AUC via rank statistic (no sklearn dependency)
    order = np.argsort(p)
    ranks = np.empty_like(order, dtype=np.float64)
    ranks[order] = np.arange(1, len(p) + 1)
    n_pos, n_neg = int(y.sum()), int((1 - y).sum())
    auc = ((ranks[y > 0.5].sum() - n_pos * (n_pos + 1) / 2) / max(1, n_pos * n_neg))
    return auc, hit1 / max(1, n_tp), n_pos


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--data-dir",   default="rl/data/exit_v4")
    p.add_argument("--checkpoint", required=True,
                   help="v3 conv champion (warm-started into v4) or a v4 ckpt to resume")
    p.add_argument("--save-dir",   required=True)
    p.add_argument("--epochs",     type=int,   default=6)
    p.add_argument("--lr",         type=float, default=1e-4)
    p.add_argument("--batch-size", type=int,   default=512)
    p.add_argument("--tau-floor",  type=float, default=1.0)
    p.add_argument("--tau-scale",  type=float, default=1.0)
    p.add_argument("--lambda-tenpai", type=float, default=0.3)
    p.add_argument("--lambda-waits",  type=float, default=0.3)
    p.add_argument("--posw-cap", type=float, default=100.0,
                   help="cap on BCE pos_weight — the raw neg/pos ratio for waits"
                        " is ~450 and uncapped weighting destabilises training")
    p.add_argument("--val-frac",   type=float, default=0.05)
    p.add_argument("--device",     default="cuda")
    args = p.parse_args()

    device   = torch.device(args.device)
    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    train_data, val_data = load_shards(Path(args.data_dir), args.val_frac)
    if "cands" not in train_data["discard"]:
        raise SystemExit("Shards lack Q-data (discard_cands)")
    assert train_data["discard"]["obs"].shape[1] == OBS_DIM_V4, \
        f"expected v4 obs ({OBS_DIM_V4}), got {train_data['discard']['obs'].shape[1]}"

    from model import MahjongConvNet, load_net, warm_start_v4_from_v3
    ckpt_meta = torch.load(args.checkpoint, map_location="cpu",
                           weights_only=False)
    if ckpt_meta.get("obs_dim") == OBS_DIM_V3:
        net = MahjongConvNet(obs_dim=OBS_DIM_V4,
                             channels=ckpt_meta.get("channels", 128),
                             n_blocks=ckpt_meta.get("n_blocks", 6)).to(device)
        warm_start_v4_from_v3(net, args.checkpoint)
        net_meta = {"obs_dim": OBS_DIM_V4, "arch": "conv",
                    "channels": net.channels, "n_blocks": net.n_blocks}
        print(f"Warm-started v4 net from v3 champion {args.checkpoint}")
    else:
        net, net_meta = load_net(args.checkpoint, device=args.device)
        assert getattr(net, "is_v4", False), "resume checkpoint must be a v4 conv net"
        print(f"Resumed v4 net from {args.checkpoint}")
    del ckpt_meta

    optimizer = torch.optim.Adam(net.parameters(), lr=args.lr, weight_decay=1e-5)
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=args.epochs, eta_min=args.lr / 10)

    # ── Class-imbalance pos_weights from the corpus ───────────────────────────
    all_tp = np.concatenate([d["tenpai"].ravel() for d in train_data.values()])
    all_w  = np.concatenate([d["waits"].ravel() for d in train_data.values()])
    tp_posw = torch.tensor(min(args.posw_cap,
                               (1 - all_tp.mean()) / max(all_tp.mean(), 1e-6)),
                           dtype=torch.float32, device=device)
    w_posw  = torch.tensor(min(args.posw_cap,
                               (1 - all_w.mean()) / max(all_w.mean(), 1e-6)),
                           dtype=torch.float32, device=device)
    print(f"pos_weight: tenpai {tp_posw.item():.1f}  waits {w_posw.item():.1f}")

    # ── Precompute soft targets for discard ──────────────────────────────────
    d = train_data["discard"]
    cands  = torch.tensor(d["cands"],  dtype=torch.int64)
    mean_d = torch.tensor(d["mean_d"], dtype=torch.float32)
    se_d   = torch.tensor(d["se_d"],   dtype=torch.float32)
    valid  = cands >= 0
    se_masked = torch.where(valid, se_d, torch.zeros_like(se_d))
    n_valid   = valid.sum(dim=1).clamp(min=1).float()
    tau = (args.tau_scale * se_masked.sum(dim=1) / n_valid).clamp(min=args.tau_floor)
    scores = torch.where(valid, mean_d / tau.unsqueeze(1),
                         torch.full_like(mean_d, -1e9))
    targets = F.softmax(scores, dim=1)

    datasets = {}
    for dec, dd in train_data.items():
        base = [
            torch.tensor(dd["obs"], dtype=torch.float32),
            torch.tensor(dd["act"], dtype=torch.int64),
            torch.tensor(dd["mask"], dtype=torch.bool),
            torch.tensor(dd["tenpai"], dtype=torch.float32),
            torch.tensor(dd["waits"], dtype=torch.float32),
        ]
        if dec == "discard":
            base += [cands.clamp(min=0), targets,
                     torch.tensor(d["switched"], dtype=torch.bool)]
        datasets[dec] = TensorDataset(*base)

    print(f"\nTraining {args.epochs} epochs, lr={args.lr}, "
          f"λ_t={args.lambda_tenpai}, λ_w={args.lambda_waits}")
    print(f"  mean tau={tau.mean():.2f}  "
          f"sharp targets (max>0.5): {(targets.max(dim=1).values > 0.5).float().mean():.1%}")

    for epoch in range(args.epochs):
        net.train()
        ep_loss, ep_n = defaultdict(float), defaultdict(int)
        ep_t = ep_w = 0.0
        agree_nn, agree_sw, n_sw = 0, 0, 0

        for dec, dataset in datasets.items():
            loader = DataLoader(dataset, batch_size=args.batch_size, shuffle=True)
            for batch in loader:
                batch = [b.to(device) for b in batch]
                if dec == "discard":
                    obs_b, act_b, mask_b, tp_b, w_b, cand_b, tgt_b, sw_b = batch
                    logits, _, tpl, wl = net.forward_with_danger(obs_b, dec, mask_b)
                    logp = F.log_softmax(logits, dim=-1)
                    logp_c = torch.gather(logp, 1, cand_b)
                    contrib = torch.where(tgt_b > 0, tgt_b * logp_c,
                                          torch.zeros_like(logp_c))
                    loss_pol = -contrib.sum(dim=1).mean()
                    preds = logits.argmax(dim=-1)
                    agree_nn += (preds == act_b).sum().item()
                    agree_sw += (preds[sw_b] == act_b[sw_b]).sum().item()
                    n_sw     += int(sw_b.sum().item())
                else:
                    obs_b, act_b, mask_b, tp_b, w_b = batch
                    logits, _, tpl, wl = net.forward_with_danger(obs_b, dec, mask_b)
                    loss_pol = F.cross_entropy(logits, act_b)

                l_t, l_w = danger_losses(tpl, wl, tp_b, w_b, tp_posw, w_posw)
                loss = loss_pol + args.lambda_tenpai * l_t + args.lambda_waits * l_w

                optimizer.zero_grad()
                loss.backward()
                nn.utils.clip_grad_norm_(net.parameters(), 1.0)
                optimizer.step()
                bsz = len(batch[0])
                ep_loss[dec] += loss_pol.item() * bsz
                ep_n[dec]    += bsz
                ep_t += l_t.item() * bsz
                ep_w += l_w.item() * bsz

        scheduler.step()
        n_tot = sum(ep_n.values())
        auc, hit1, n_pos = (eval_danger(net, val_data, device)
                            if val_data else (float("nan"), float("nan"), 0))
        print(f"  epoch {epoch+1:>2}/{args.epochs} "
              f"discard_loss={ep_loss['discard']/max(ep_n['discard'],1):.4f} "
              f"act_agree={agree_nn/max(ep_n['discard'],1):.1%} "
              f"switched_agree={agree_sw/max(n_sw,1):.1%} "
              f"tenpai_bce={ep_t/n_tot:.4f} waits_bce={ep_w/n_tot:.4f} "
              f"| val tenpai_AUC={auc:.3f} wait_hit@1={hit1:.1%} (n+={n_pos}) "
              f"lr={scheduler.get_last_lr()[0]:.1e}")
        sys.stdout.flush()

        ckpt_out = dict(net_meta,
            net_state=net.state_dict(), epoch=epoch + 1,
            source=f"exit_distill_v4 from {args.checkpoint}",
        )
        torch.save(ckpt_out, str(save_dir / f"exit_epoch{epoch+1}.pt"))

    torch.save(ckpt_out, str(save_dir / "exit_final.pt"))
    print(f"\nSaved final checkpoint → {save_dir / 'exit_final.pt'}")


if __name__ == "__main__":
    main()
