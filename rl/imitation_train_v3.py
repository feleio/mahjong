"""
imitation_train_v3.py — Imitation learning from FirstFelix with obs v3 + CNN.

Differences vs imitation_train.py (v2):
  * obs v3 (725 dims): includes shanten/ukeire feature planes computed in Scala
  * MahjongConvNet: residual 1D CNN over the 34-tile axis
  * parallel data collection (N FFDataServer processes → npz shards on disk)
  * obs stored as float16 shards to fit 100k games in RAM

Usage
─────
    rl/venv/bin/python rl/imitation_train_v3.py \
        --n-games 100000 --n-workers 10 --epochs 20 \
        --data-dir rl/data/ff_v3 --save-dir rl/checkpoints/imitation_v3 \
        --device cuda
"""

import argparse
import json
import subprocess
import sys
import time
from collections import defaultdict
from concurrent.futures import ProcessPoolExecutor
from pathlib import Path

import numpy as np

sys.path.insert(0, str(Path(__file__).parent))


# ── Parallel data collection ─────────────────────────────────────────────────

def collect_worker(args_tuple):
    """One worker: run FFDataServer, encode v3 obs, write npz shards."""
    worker_id, jar, n_games, seed, data_dir, games_per_shard = args_tuple
    from env import encode_state, get_action_mask

    proc = subprocess.Popen(
        ["java",
         "-Dlogback.statusListenerClass=ch.qos.logback.core.status.NopStatusListener",
         "-cp", jar, "io.fele.app.mahjong.rl.FFDataServer"],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        bufsize=1, text=True,
    )
    proc.stdin.write(json.dumps({"cmd": "collect", "n_games": n_games,
                                 "seed": seed}) + "\n")
    proc.stdin.flush()

    data_dir = Path(data_dir)
    buf = defaultdict(list)
    shard_idx, games_done, n_dec = 0, 0, 0
    rewards = []

    def flush():
        nonlocal shard_idx, buf
        if not any(buf.values()):
            return
        out = {}
        for dec, samples in buf.items():
            out[f"{dec}_obs"]  = np.array([s[0] for s in samples], dtype=np.float16)
            out[f"{dec}_act"]  = np.array([s[1] for s in samples], dtype=np.int64)
            out[f"{dec}_mask"] = np.array([s[2] for s in samples], dtype=bool)
        np.savez_compressed(data_dir / f"shard_w{worker_id:02d}_{shard_idx:04d}.npz", **out)
        buf = defaultdict(list)
        shard_idx += 1

    for line in proc.stdout:
        msg = json.loads(line.strip())
        if msg["type"] == "decision":
            dec  = msg["decision"]
            obs  = encode_state(msg["state"], version=3, context=msg["context"])
            mask = get_action_mask(dec, msg["context"])
            buf[dec].append((obs.astype(np.float16), int(msg["action"]), mask))
            n_dec += 1
        elif msg["type"] == "game_over":
            rewards.append(msg["reward"])
            games_done += 1
            if games_done % games_per_shard == 0:
                flush()
        elif msg["type"] == "batch_done":
            break
    flush()
    proc.stdin.close()
    proc.wait(timeout=10)
    return worker_id, games_done, n_dec, float(np.mean([r > 0 for r in rewards]))


def collect_parallel(jar, n_games, n_workers, data_dir):
    data_dir = Path(data_dir)
    data_dir.mkdir(parents=True, exist_ok=True)
    per = [n_games // n_workers + (1 if w < n_games % n_workers else 0)
           for w in range(n_workers)]
    seeds = np.cumsum([0] + per[:-1])
    jobs = [(w, jar, per[w], int(seeds[w]), str(data_dir), 2000)
            for w in range(n_workers)]
    t0 = time.time()
    with ProcessPoolExecutor(max_workers=n_workers) as pool:
        for wid, games, ndec, winr in pool.map(collect_worker, jobs):
            print(f"  worker {wid}: {games} games, {ndec:,} decisions, "
                  f"FF win% {winr:.1%}")
            sys.stdout.flush()
    print(f"Collection done in {time.time() - t0:.0f}s")


# ── Training ─────────────────────────────────────────────────────────────────

def load_all_shards(data_dir):
    import torch
    shards = sorted(Path(data_dir).glob("shard_*.npz"))
    if not shards:
        raise SystemExit(f"no shards in {data_dir}")
    per_dec = defaultdict(lambda: defaultdict(list))
    for p in shards:
        z = np.load(p)
        for key in z.files:
            dec, field = key.rsplit("_", 1)
            per_dec[dec][field].append(z[key])
    data = {}
    for dec, f in per_dec.items():
        data[dec] = {
            "obs":  torch.tensor(np.concatenate(f["obs"]), dtype=torch.float16),
            "act":  torch.tensor(np.concatenate(f["act"]), dtype=torch.int64),
            "mask": torch.tensor(np.concatenate(f["mask"]), dtype=torch.bool),
        }
        print(f"  {dec:10s}: {len(data[dec]['act']):>9,} samples")
    return data


def train(args):
    import torch
    import torch.nn as nn
    import torch.nn.functional as F
    from env import OBS_DIM_V3
    from model import MahjongConvNet

    device = torch.device(args.device)
    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    print("Loading shards…")
    data = load_all_shards(args.data_dir)

    net = MahjongConvNet(obs_dim=OBS_DIM_V3, channels=args.channels,
                         n_blocks=args.n_blocks).to(device)
    print(f"MahjongConvNet: {sum(p.numel() for p in net.parameters()):,} params")

    optimizer = torch.optim.Adam(net.parameters(), lr=args.lr, weight_decay=1e-5)
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=args.epochs, eta_min=args.lr / 20)

    # Oversampling factor per decision type: rare heads (win/kong/self_win)
    # must not be drowned by 2M discard samples through the shared trunk.
    repeats = {dec: int(np.clip(args.balance_min // max(len(d["act"]), 1), 1, 25))
               for dec, d in data.items()}
    print("Oversampling:", {d: r for d, r in repeats.items() if r > 1})

    for epoch in range(args.epochs):
        net.train()
        ep_loss, ep_correct, ep_total = (defaultdict(float), defaultdict(int),
                                         defaultdict(int))
        for dec, d in data.items():
            n = len(d["act"])
            perm = torch.cat([torch.randperm(n) for _ in range(repeats[dec])])
            for i in range(0, n, args.batch_size):
                idx = perm[i:i + args.batch_size]
                obs_b  = d["obs"][idx].to(device).float()
                act_b  = d["act"][idx].to(device)
                mask_b = d["mask"][idx].to(device)
                logits, _ = net(obs_b, dec, mask_b)
                loss = F.cross_entropy(logits, act_b)
                optimizer.zero_grad()
                loss.backward()
                nn.utils.clip_grad_norm_(net.parameters(), 1.0)
                optimizer.step()
                ep_loss[dec]    += loss.item() * len(idx)
                ep_correct[dec] += (logits.argmax(-1) == act_b).sum().item()
                ep_total[dec]   += len(idx)
        scheduler.step()

        tot_c = sum(ep_correct.values()); tot_n = sum(ep_total.values())
        parts = [f"{d}:{ep_correct[d]/ep_total[d]:.0%}" for d in sorted(data)]
        print(f"  epoch {epoch+1:>2}/{args.epochs} "
              f"loss={sum(ep_loss.values())/tot_n:.4f} acc={tot_c/tot_n:.2%} "
              f"[{', '.join(parts)}] lr={scheduler.get_last_lr()[0]:.1e}")
        sys.stdout.flush()

        if (epoch + 1) % args.save_every == 0 or epoch + 1 == args.epochs:
            torch.save({
                "net_state": net.state_dict(),
                "obs_dim":   OBS_DIM_V3,
                "arch":      "conv",
                "channels":  args.channels,
                "n_blocks":  args.n_blocks,
                "epoch":     epoch + 1,
            }, str(save_dir / f"imitation_epoch{epoch+1}.pt"))

    torch.save({
        "net_state": net.state_dict(),
        "obs_dim":   OBS_DIM_V3,
        "arch":      "conv",
        "channels":  args.channels,
        "n_blocks":  args.n_blocks,
        "epoch":     args.epochs,
    }, str(save_dir / "imitation_final.pt"))
    print(f"Saved → {save_dir / 'imitation_final.pt'}")


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar",        default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--n-games",    type=int, default=100_000)
    p.add_argument("--n-workers",  type=int, default=10)
    p.add_argument("--data-dir",   default="rl/data/ff_v3")
    p.add_argument("--save-dir",   default="rl/checkpoints/imitation_v3")
    p.add_argument("--epochs",     type=int,   default=20)
    p.add_argument("--lr",         type=float, default=1e-3)
    p.add_argument("--batch-size", type=int,   default=512)
    p.add_argument("--channels",   type=int,   default=128)
    p.add_argument("--n-blocks",   type=int,   default=6)
    p.add_argument("--device",     default="cuda")
    p.add_argument("--skip-collect", action="store_true")
    p.add_argument("--balance-min", type=int, default=150_000,
                   help="oversample rare decision types up to ~this many samples/epoch")
    p.add_argument("--save-every",  type=int, default=2)
    args = p.parse_args()

    if not args.skip_collect:
        print("=" * 60)
        print(f"Collecting {args.n_games} FF games with {args.n_workers} workers")
        print("=" * 60)
        collect_parallel(args.jar, args.n_games, args.n_workers, args.data_dir)

    print("\n" + "=" * 60)
    print("Training")
    print("=" * 60)
    train(args)


if __name__ == "__main__":
    main()
