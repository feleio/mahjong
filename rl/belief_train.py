"""
belief_train.py — opponent hand-belief model + the go/no-go validation.

Trains a small MLP to predict an opponent's true 13-tile dynamic hand from their
PUBLIC trail (their discards, melds, hand size, wall remaining), then compares
it head-to-head with the UNIFORM assumption the search currently makes when it
determinizes hidden tiles.

The decision metric: does the model concentrate probability on the tiles the
opponent actually holds better than uniform? If top-k recall and cross-entropy
beat uniform by a clear margin, belief-state determinization is worth building
into MCTSRolloutServer. If not, uniform is fine and we drop the lever.

    rl/venv/bin/python rl/belief_train.py --data rl/data/belief_ff.jsonl \
        --out rl/checkpoints/belief/ff.pt
"""

import argparse
import json
import sys
import time
from pathlib import Path

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F


def load(path, limit=None):
    disc, pong, kong, chow, scal, hand = [], [], [], [], [], []
    with open(path) as fh:
        for i, line in enumerate(fh):
            if limit and i >= limit:
                break
            r = json.loads(line)
            hs = r["hand_size"]
            if hs <= 0:
                continue
            disc.append(r["disc"]); pong.append(r["pong"])
            kong.append(r["kong"]); chow.append(r["chow"])
            scal.append([hs / 13.0, r["remaining"] / 136.0])
            hand.append(r["hand"])
    disc = np.array(disc, np.float32); pong = np.array(pong, np.float32)
    kong = np.array(kong, np.float32); chow = np.array(chow, np.float32)
    scal = np.array(scal, np.float32); hand = np.array(hand, np.float32)
    X = np.concatenate([disc / 4, pong, kong, chow, scal], axis=1)   # (N,138)
    # tiles this player could still hold (their own copies not yet public)
    avail = np.clip(4 - disc - 3 * pong - 4 * kong - chow, 0, 4).astype(np.float32)
    return X, hand, avail


class BeliefNet(nn.Module):
    def __init__(self, in_dim=138, h=256):
        super().__init__()
        self.net = nn.Sequential(
            nn.Linear(in_dim, h), nn.ReLU(),
            nn.Linear(h, h), nn.ReLU(),
            nn.Linear(h, 34))

    def forward(self, x, avail):
        logits = self.net(x)
        logits = logits.masked_fill(avail <= 0, -30.0)  # can't hold unavailable
        return logits


def metrics(p, hand, avail, ks=(6, 10)):
    """p: (N,34) prob dist; hand: true counts; avail: possible copies."""
    hs = hand.sum(1, keepdim=True)
    tgt = hand / hs.clamp(min=1)
    ce = -(tgt * torch.log(p.clamp(min=1e-9))).sum(1).mean().item()
    out = {"ce": ce}
    # top-k recall: fraction of the opponent's tile-types captured in top-k
    held = (hand > 0).float()
    nheld = held.sum(1).clamp(min=1)
    for k in ks:
        topk = p.topk(k, dim=1).indices
        hit = torch.gather(held, 1, topk).sum(1)
        out[f"recall@{k}"] = (hit / nheld.clamp(max=k)).mean().item()
    return out


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--data", required=True)
    p.add_argument("--limit", type=int, default=None)
    p.add_argument("--epochs", type=int, default=6)
    p.add_argument("--batch-size", type=int, default=4096)
    p.add_argument("--lr", type=float, default=1e-3)
    p.add_argument("--device", default="cuda")
    p.add_argument("--out", default="rl/checkpoints/belief/ff.pt")
    args = p.parse_args()

    t0 = time.time()
    X, hand, avail = load(args.data, args.limit)
    print(f"loaded {len(X):,} rows in {time.time()-t0:.0f}s")
    n = len(X); nval = n // 20
    perm = np.random.RandomState(0).permutation(n)
    vi, ti = perm[:nval], perm[nval:]
    dev = torch.device(args.device)

    def to(idx):
        return (torch.tensor(X[idx], device=dev),
                torch.tensor(hand[idx], device=dev),
                torch.tensor(avail[idx], device=dev))
    Xv, Hv, Av = to(vi)

    # ── Uniform baseline (what the search does now) ──────────────────────────
    p_unif = (Av > 0).float() * Av
    p_unif = p_unif / p_unif.sum(1, keepdim=True).clamp(min=1e-9)
    base = metrics(p_unif, Hv, Av)
    print(f"UNIFORM baseline:  ce={base['ce']:.3f}  "
          f"recall@6={base['recall@6']:.1%}  recall@10={base['recall@10']:.1%}")

    net = BeliefNet().to(dev)
    opt = torch.optim.Adam(net.parameters(), lr=args.lr)
    for ep in range(args.epochs):
        net.train()
        order = np.random.permutation(len(ti))
        losses = []
        for s in range(0, len(order), args.batch_size):
            b = ti[order[s:s + args.batch_size]]
            xb, hb, ab = to(b)
            logits = net(xb, ab)
            logp = F.log_softmax(logits, dim=1)
            tgt = hb / hb.sum(1, keepdim=True).clamp(min=1)
            loss = -(tgt * logp).sum(1).mean()
            opt.zero_grad(); loss.backward(); opt.step()
            losses.append(loss.item())
        net.eval()
        with torch.no_grad():
            pv = F.softmax(net(Xv, Av), dim=1)
            m = metrics(pv, Hv, Av)
        print(f"epoch {ep+1}: loss={np.mean(losses):.3f}  MODEL ce={m['ce']:.3f}  "
              f"recall@6={m['recall@6']:.1%}  recall@10={m['recall@10']:.1%}")

    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    torch.save({"net_state": net.state_dict()}, args.out)
    print(f"\nsaved -> {args.out}")
    print(f"VERDICT: model recall@6 {m['recall@6']:.1%} vs uniform "
          f"{base['recall@6']:.1%}  (lift {m['recall@6']-base['recall@6']:+.1%}); "
          f"ce {m['ce']:.3f} vs {base['ce']:.3f}")


if __name__ == "__main__":
    main()
