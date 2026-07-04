"""
imitation_train.py — Train a model to imitate FirstFelix via supervised learning.

Collects decision data from FFDataServer (FF vs FF games), then trains
a neural network to predict FF's actions using cross-entropy loss.

Usage
─────
    python rl/imitation_train.py \
        --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \
        --n-games 50000 \
        --epochs 20 \
        --save-dir rl/checkpoints/imitation_v1
"""

import argparse
import json
import subprocess
import sys
import time
from collections import defaultdict
from pathlib import Path

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F
from torch.utils.data import DataLoader, TensorDataset

sys.path.insert(0, str(Path(__file__).parent))

from env import OBS_DIM, DECISION_SPACES, encode_state, get_action_mask
from model import MahjongNet


# ── Data Collection ──────────────────────────────────────────────────────────

def collect_ff_data(jar_path: str, n_games: int, seed: int = 0,
                    java_bin: str = "java") -> dict:
    """Run FFDataServer and collect all decisions."""
    proc = subprocess.Popen(
        [java_bin,
         "-Dlogback.statusListenerClass=ch.qos.logback.core.status.NopStatusListener",
         "-cp", jar_path,
         "io.fele.app.mahjong.rl.FFDataServer"],
        stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
        bufsize=1, text=True,
    )

    # Send collect command
    cmd = json.dumps({"cmd": "collect", "n_games": n_games, "seed": seed})
    proc.stdin.write(cmd + "\n")
    proc.stdin.flush()

    # Collect data grouped by decision type
    data = defaultdict(list)  # decision -> [(obs, action, mask)]
    game_rewards = []
    n_decisions = 0

    t0 = time.time()
    for line in proc.stdout:
        msg = json.loads(line.strip())

        if msg["type"] == "decision":
            decision = msg["decision"]
            obs = encode_state(msg["state"])
            mask = get_action_mask(decision, msg["context"])

            # Convert action to integer index
            action_raw = msg["action"]
            if decision in ("win", "self_win", "pong", "kong"):
                action_idx = int(action_raw)  # True->1, False->0
            elif decision == "discard":
                action_idx = int(action_raw)  # tile ID 0-33
            elif decision == "chow":
                action_idx = int(action_raw)  # 0=pass, 1=LEFT, 2=MID, 3=RIGHT
            elif decision == "self_kong":
                action_idx = int(action_raw)  # 0=pass, 1-34=tile+1
            else:
                continue

            data[decision].append((obs, action_idx, mask))
            n_decisions += 1

        elif msg["type"] == "game_over":
            game_rewards.append(msg["reward"])
            g = msg.get("game_idx", len(game_rewards) - 1)
            if (g + 1) % 5000 == 0:
                elapsed = time.time() - t0
                print(f"  collected {g+1}/{n_games} games, "
                      f"{n_decisions} decisions, {elapsed:.0f}s")

        elif msg["type"] == "batch_done":
            break

    proc.stdin.close()
    proc.wait(timeout=10)

    elapsed = time.time() - t0
    print(f"\nData collection complete: {n_games} games, "
          f"{n_decisions} total decisions, {elapsed:.0f}s")
    for dec, samples in sorted(data.items()):
        print(f"  {dec:12s}: {len(samples):>8,} samples")
    print(f"  FF win rate: {np.mean([r > 0 for r in game_rewards]):.1%} "
          f"(avg reward: {np.mean(game_rewards):+.2f})")

    return dict(data)


# ── Supervised Training ──────────────────────────────────────────────────────

def train_supervised(net: nn.Module, data: dict, args,
                     save_dir: Path = None) -> None:
    """Train network to predict FF actions via cross-entropy."""
    device = torch.device(args.device)
    net = net.to(device)

    optimizer = torch.optim.Adam(net.parameters(), lr=args.lr, weight_decay=1e-5)
    scheduler = torch.optim.lr_scheduler.CosineAnnealingLR(
        optimizer, T_max=args.epochs, eta_min=args.lr / 10)

    # Prepare datasets per decision type
    datasets = {}
    for decision, samples in data.items():
        obs_arr = np.array([s[0] for s in samples], dtype=np.float32)
        act_arr = np.array([s[1] for s in samples], dtype=np.int64)
        mask_arr = np.array([s[2] for s in samples], dtype=np.float32)

        obs_t = torch.tensor(obs_arr)
        act_t = torch.tensor(act_arr)
        mask_t = torch.tensor(mask_arr, dtype=torch.bool)

        datasets[decision] = TensorDataset(obs_t, act_t, mask_t)

    print(f"\nTraining for {args.epochs} epochs, lr={args.lr}, "
          f"batch_size={args.batch_size}")
    print("-" * 60)

    for epoch in range(args.epochs):
        net.train()
        epoch_losses = defaultdict(list)
        epoch_correct = defaultdict(int)
        epoch_total = defaultdict(int)

        for decision, dataset in datasets.items():
            loader = DataLoader(dataset, batch_size=args.batch_size,
                                shuffle=True, drop_last=False)

            for obs_b, act_b, mask_b in loader:
                obs_b = obs_b.to(device)
                act_b = act_b.to(device)
                mask_b = mask_b.to(device)

                logits, _ = net(obs_b, decision, mask_b)
                loss = F.cross_entropy(logits, act_b)

                optimizer.zero_grad()
                loss.backward()
                nn.utils.clip_grad_norm_(net.parameters(), 1.0)
                optimizer.step()

                epoch_losses[decision].append(loss.item() * len(obs_b))
                preds = logits.argmax(dim=-1)
                epoch_correct[decision] += (preds == act_b).sum().item()
                epoch_total[decision] += len(obs_b)

        scheduler.step()

        # Log epoch summary
        total_loss = 0.0
        total_correct = 0
        total_n = 0
        parts = []
        for dec in sorted(epoch_losses.keys()):
            n = epoch_total[dec]
            avg_loss = sum(epoch_losses[dec]) / n
            acc = epoch_correct[dec] / n * 100
            parts.append(f"{dec}:{avg_loss:.3f}/{acc:.0f}%")
            total_loss += sum(epoch_losses[dec])
            total_correct += epoch_correct[dec]
            total_n += n

        overall_acc = total_correct / total_n * 100
        overall_loss = total_loss / total_n
        print(f"  epoch {epoch+1:>2}/{args.epochs}  "
              f"loss={overall_loss:.4f}  acc={overall_acc:.1f}%  "
              f"lr={scheduler.get_last_lr()[0]:.1e}  "
              f"[{', '.join(parts)}]")
        sys.stdout.flush()

        # Save checkpoint every 5 epochs
        if save_dir is not None and (epoch + 1) % 5 == 0:
            ckpt_path = save_dir / f"imitation_epoch{epoch+1}.pt"
            torch.save({
                "net_state":   net.state_dict(),
                "obs_dim":     OBS_DIM,
                "hidden_size": args.hidden_size,
                "n_layers":    args.n_layers,
                "epoch":       epoch + 1,
                "n_games":     args.n_games,
            }, str(ckpt_path))
            print(f"    [checkpoint saved → {ckpt_path}]")
            sys.stdout.flush()


# ── CLI ──────────────────────────────────────────────────────────────────────

def parse_args():
    p = argparse.ArgumentParser(description="Imitation learning from FirstFelix")
    p.add_argument("--jar",        type=str, required=True)
    p.add_argument("--java",       type=str, default="java")
    p.add_argument("--n-games",    type=int, default=50_000)
    p.add_argument("--seed",       type=int, default=0)
    p.add_argument("--epochs",     type=int, default=20)
    p.add_argument("--lr",         type=float, default=1e-3)
    p.add_argument("--batch-size", type=int, default=512)
    p.add_argument("--hidden-size",type=int, default=512)
    p.add_argument("--n-layers",   type=int, default=2)
    p.add_argument("--save-dir",   type=str, default="rl/checkpoints/imitation_v1")
    p.add_argument("--checkpoint", type=str, default=None,
                   help="Optional: warm-start from existing checkpoint")
    p.add_argument("--device",     type=str, default="cpu")
    return p.parse_args()


def main():
    args = parse_args()
    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    # Step 1: Collect data
    print("=" * 60)
    print("Step 1: Collecting FF decision data")
    print("=" * 60)
    data = collect_ff_data(args.jar, args.n_games, args.seed, args.java)

    # Step 2: Create/load model
    net = MahjongNet(obs_dim=OBS_DIM, hidden_size=args.hidden_size,
                     n_layers=args.n_layers)
    if args.checkpoint:
        ckpt = torch.load(args.checkpoint, map_location=args.device,
                          weights_only=False)
        net.load_state_dict(ckpt["net_state"])
        print(f"Loaded checkpoint: {args.checkpoint}")

    # Step 3: Train
    print("\n" + "=" * 60)
    print("Step 2: Supervised training")
    print("=" * 60)
    train_supervised(net, data, args, save_dir=save_dir)

    # Step 4: Save
    out_path = save_dir / "imitation_final.pt"
    torch.save({
        "net_state": net.state_dict(),
        "obs_dim": OBS_DIM,
        "hidden_size": args.hidden_size,
        "n_layers": args.n_layers,
        "n_games": args.n_games,
        "epochs": args.epochs,
    }, str(out_path))
    print(f"\nSaved model → {out_path}")


if __name__ == "__main__":
    main()
