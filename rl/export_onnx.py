"""
export_onnx.py — Export a MahjongConvNet checkpoint to ONNX for use inside
the Scala rollout server (ONNX Runtime Java).

The graph takes a batch of flat v3 observations (B, 759) and returns the
logits of every decision head plus the value estimate; the caller picks the
head it needs and applies its own action masking (mask AFTER inference —
masking only removes rows from an argmax/softmax, it does not change the
network's computation).

Usage
─────
    rl/venv/bin/python rl/export_onnx.py \
        --checkpoint rl/checkpoints/exit_v3_r3_soft/exit_final.pt \
        --out rl/checkpoints/exit_v3_r3_soft/exit_final.onnx
"""

import argparse
import sys
from pathlib import Path

import numpy as np
import torch
import torch.nn as nn

sys.path.insert(0, str(Path(__file__).parent))

from model import load_net

HEADS = ["discard", "win", "self_win", "pong", "kong", "chow", "self_kong"]


class AllHeads(nn.Module):
    """Wraps MahjongConvNet: one forward pass computing every head.

    v4 nets additionally emit the danger heads as PROBABILITIES (sigmoid
    applied in-graph): opp_tenpai (B,3) and opp_waits (B,3,34), opponent
    order seat-relative (myId+1, +2, +3) as in DangerLabels."""

    def __init__(self, net):
        super().__init__()
        self.net = net

    def forward(self, obs):
        n = self.net
        x = n.stem(n._to_planes(obs))
        x = n.blocks(x)
        pooled = torch.cat([x.mean(dim=2), x.amax(dim=2)], dim=1)
        feat = n.pool_fc(pooled)
        value = n.value_head(feat).squeeze(-1)
        discard = n.discard_conv(x).squeeze(1)
        self_kong = torch.cat(
            [n.self_kong_pass(feat), n.self_kong_conv(x).squeeze(1)], dim=1)
        base = (discard, n.win_head(feat), n.self_win_head(feat),
                n.pong_head(feat), n.kong_head(feat), n.chow_head(feat),
                self_kong, value)
        if n.is_v4:
            return base + (torch.sigmoid(n.danger_tenpai_head(feat)),
                           torch.sigmoid(n.danger_wait_conv(x)))
        return base


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--checkpoint", required=True)
    p.add_argument("--out", required=True)
    args = p.parse_args()

    net, meta = load_net(args.checkpoint, device="cpu")
    assert meta["arch"] == "conv", "ONNX export supports the conv net only"
    net.eval()
    wrapper = AllHeads(net).eval()

    out_names = HEADS + ["value"]
    if net.is_v4:
        out_names += ["opp_tenpai", "opp_waits"]
    dummy = torch.zeros(1, net.obs_dim, dtype=torch.float32)
    torch.onnx.export(
        wrapper, dummy, args.out,
        input_names=["obs"],
        output_names=out_names,
        dynamic_axes={"obs": {0: "batch"},
                      **{h: {0: "batch"} for h in out_names}},
        opset_version=17,
    )

    # parity check vs PyTorch on random inputs
    import onnxruntime as ort
    sess = ort.InferenceSession(args.out, providers=["CPUExecutionProvider"])
    rng = np.random.default_rng(0)
    obs = rng.random((64, net.obs_dim), dtype=np.float32)
    ort_out = sess.run(None, {"obs": obs})
    with torch.no_grad():
        pt_out = wrapper(torch.tensor(obs))
    max_err = max(float(np.abs(o - p.numpy()).max())
                  for o, p in zip(ort_out, pt_out))
    print(f"exported {args.out}  max |onnx - torch| = {max_err:.2e}")
    assert max_err < 1e-4, "ONNX output mismatch"

    import time
    for bs in (1, 16, 64):
        x = obs[:bs]
        t0 = time.time()
        n_iter = 200
        for _ in range(n_iter):
            sess.run(None, {"obs": x})
        dt = (time.time() - t0) / n_iter * 1000
        print(f"  batch {bs:>3}: {dt:.3f} ms/call")


if __name__ == "__main__":
    main()
