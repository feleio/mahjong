"""
export_belief_onnx.py — Export a trained opponent hand-belief model (BeliefNet)
to ONNX for use inside the Scala rollout server (ONNX Runtime Java).

The graph takes a batch of two inputs:

    feat  (B, 138)  public-trail features, EXACTLY as belief_train.load builds
                    them: concat(disc/4, pong, kong, chow, [hand_size/13,
                    remaining/136]).
    avail (B, 34)   per-tile copies this opponent could still hold
                    (clip(4 - disc - 3*pong - 4*kong - chow, 0, 4)).

and returns:

    probs (B, 34)   softmax over tile types of the (avail-masked) logits — the
                    per-tile-type hold distribution the search uses to weight
                    opponent-hand determinization.

Masking is baked into the graph (logits.masked_fill(avail<=0, -30)) so the
Scala side only has to supply the two feature tensors and read one output.

Usage
─────
    rl/venv/bin/python rl/export_belief_onnx.py \
        --checkpoint rl/checkpoints/belief/ff.pt \
        --out rl/checkpoints/belief/ff.onnx
"""

import argparse
import sys
from pathlib import Path

import numpy as np
import torch
import torch.nn as nn
import torch.nn.functional as F

sys.path.insert(0, str(Path(__file__).parent))

from belief_train import BeliefNet


class BeliefProbs(nn.Module):
    """Wraps BeliefNet: masked logits → softmax probability distribution."""

    def __init__(self, net):
        super().__init__()
        self.net = net

    def forward(self, feat, avail):
        logits = self.net(feat, avail)          # already masked_fill(avail<=0, -30)
        return F.softmax(logits, dim=1)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--checkpoint", default="rl/checkpoints/belief/ff.pt")
    p.add_argument("--out", default="rl/checkpoints/belief/ff.onnx")
    args = p.parse_args()

    ckpt = torch.load(args.checkpoint, map_location="cpu")
    net = BeliefNet()
    net.load_state_dict(ckpt["net_state"])
    net.eval()
    wrapper = BeliefProbs(net).eval()

    dummy_feat = torch.zeros(1, 138, dtype=torch.float32)
    dummy_avail = torch.full((1, 34), 4.0, dtype=torch.float32)
    torch.onnx.export(
        wrapper, (dummy_feat, dummy_avail), args.out,
        input_names=["feat", "avail"],
        output_names=["probs"],
        dynamic_axes={"feat": {0: "batch"}, "avail": {0: "batch"},
                      "probs": {0: "batch"}},
        opset_version=17,
    )

    # parity check vs PyTorch on random inputs
    import onnxruntime as ort
    sess = ort.InferenceSession(args.out, providers=["CPUExecutionProvider"])
    rng = np.random.default_rng(0)
    feat = rng.random((64, 138), dtype=np.float32)
    avail = rng.integers(0, 5, (64, 34)).astype(np.float32)
    ort_out = sess.run(None, {"feat": feat, "avail": avail})[0]
    with torch.no_grad():
        pt_out = wrapper(torch.tensor(feat), torch.tensor(avail)).numpy()
    max_err = float(np.abs(ort_out - pt_out).max())
    print(f"exported {args.out}  max |onnx - torch| = {max_err:.2e}")
    assert max_err < 1e-4, "ONNX output mismatch"

    import time
    for bs in (1, 3, 16):
        x_f, x_a = feat[:bs], avail[:bs]
        t0 = time.time()
        n_iter = 500
        for _ in range(n_iter):
            sess.run(None, {"feat": x_f, "avail": x_a})
        dt = (time.time() - t0) / n_iter * 1000
        print(f"  batch {bs:>3}: {dt:.3f} ms/call")


if __name__ == "__main__":
    main()
