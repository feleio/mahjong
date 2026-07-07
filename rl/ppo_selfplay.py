"""
ppo_selfplay.py — KL-anchored PPO fine-tune of a strong checkpoint via self-play.

Stage A of the AlphaZero-style program: warm-start from the best ExIt
checkpoint and optimise real game money directly. Most tables are true
self-play (all 4 seats share the evolving net, zero-sum rewards); a fraction
are vs 3×FirstFelix to keep the eval objective in the data mix.

Key differences from the older self_play.py:
  * obs v3 + MahjongConvNet (loaded from any MahjongAgent checkpoint)
  * N parallel JVMs with cross-env batched NN forwards
  * KL penalty toward the frozen anchor checkpoint (prevents policy collapse)
  * gamma=1 Monte-Carlo returns (single terminal reward), reward scaling
  * value-head warmup (the distilled checkpoint's value head is stale)
  * checkpoints saved in MahjongAgent format → eval_parallel.py works directly

Usage
─────
    rl/venv/bin/python rl/ppo_selfplay.py \
        --checkpoint rl/checkpoints/exit_v3_r3_soft/exit_final.pt \
        --n-selfplay-envs 8 --n-ff-envs 4 \
        --total-games 60000 --device cuda --save-dir rl/checkpoints/ppo_a
"""

import argparse
import collections
import sys
import time
from pathlib import Path

import numpy as np
import torch
import torch.nn as nn

sys.path.insert(0, str(Path(__file__).parent))

from env import MahjongEnv, DECISION_SPACES, encode_state, get_action_mask
from model import MahjongAgent, load_net
from self_play import TrueSelfPlayEnv, _action_to_json  # protocol reuse

REWARD_SCALE = 25.0


# ── Uniform adapter over both table types ─────────────────────────────────────

class SelfPlayTable:
    """All 4 seats driven by the net. Pending obs exposed for batching."""
    kind = "selfplay"

    def __init__(self, jar, seed_iter):
        self.env = TrueSelfPlayEnv(jar)
        self.seed_iter = seed_iter
        self.transitions = {i: [] for i in range(4)}
        self.pending = None      # (seat, decision, obs, mask)
        self._msg = None
        self.reset()

    def reset(self):
        self.transitions = {i: [] for i in range(4)}
        self._msg = self.env.reset(seed=next(self.seed_iter))
        self._prep()

    def _prep(self):
        msg = self._msg
        if msg["type"] == "game_over":
            self.pending = None
            return
        state, context = msg["state"], msg.get("context", {})
        obs = encode_state(state, version=3, context=context)
        mask = get_action_mask(msg["decision"], context)
        self.pending = (int(msg["seat_id"]), msg["decision"], obs, mask)

    def finished(self):
        return self._msg["type"] == "game_over"

    def result(self):
        rewards = {int(k): float(v) for k, v in self._msg["rewards"].items()}
        return self.transitions, rewards

    def apply(self, action, log_prob, value):
        seat, decision, obs, mask = self.pending
        self.transitions[seat].append(
            (obs, decision, action, log_prob, value, mask))
        self._msg = self.env.step(decision, action)
        self._prep()

    def close(self):
        self.env.close()


class FFTable:
    """Seat 0 driven by the net vs 3 FirstFelix."""
    kind = "ff"

    def __init__(self, jar, seed_iter):
        self.env = MahjongEnv(jar, opponent="firstfelix", obs_version=3)
        self.seed_iter = seed_iter
        self.transitions = {i: [] for i in range(4)}
        self.pending = None
        self._done = False
        self._reward = 0.0
        self.reset()

    def reset(self):
        self.transitions = {i: [] for i in range(4)}
        self._done, self._reward = False, 0.0
        obs, info = self.env.reset(seed=next(self.seed_iter))
        self._info = info
        self._prep(obs)

    def _prep(self, obs):
        self.pending = (0, self._info["decision"], obs,
                        self._info["action_mask"])

    def finished(self):
        return self._done

    def result(self):
        return self.transitions, {0: self._reward, 1: 0.0, 2: 0.0, 3: 0.0}

    def apply(self, action, log_prob, value):
        seat, decision, obs, mask = self.pending
        self.transitions[0].append(
            (obs, decision, action, log_prob, value, mask))
        obs2, r, done, info = self.env.step(action)
        if done:
            self._done, self._reward = True, float(r)
            self.pending = None
        else:
            self._info = info
            self._prep(obs2)

    def close(self):
        self.env.close()


# ── Batched acting across tables ──────────────────────────────────────────────

@torch.no_grad()
def act_batch(net, tables, device):
    """One synchronous round: act on every table's pending decision."""
    todo = [t for t in tables if not t.finished() and t.pending is not None]
    groups = collections.defaultdict(list)
    for t in todo:
        groups[t.pending[1]].append(t)
    for decision, ts in groups.items():
        obs_t = torch.tensor(np.stack([t.pending[2] for t in ts]),
                             dtype=torch.float32, device=device)
        mask_t = torch.tensor(np.stack([t.pending[3] for t in ts]),
                              dtype=torch.bool, device=device)
        actions, log_probs, values = net.act(obs_t, decision, mask_t)
        for t, a, lp, v in zip(ts, actions.tolist(),
                               log_probs.tolist(), values.tolist()):
            t.apply(int(a), float(lp), float(v))


# ── PPO update with KL anchor ─────────────────────────────────────────────────

def ppo_update(net, anchor, optimizer, buffer, args, device,
               policy_frozen=False):
    """buffer: list of (obs, decision, action, log_prob, value, mask, return_)"""
    by_dec = collections.defaultdict(list)
    for row in buffer:
        by_dec[row[1]].append(row)

    stats = collections.defaultdict(list)
    for epoch in range(args.n_epochs):
        for dec, rows in by_dec.items():
            obs = np.stack([r[0] for r in rows])
            act = np.array([r[2] for r in rows], dtype=np.int64)
            lp_old = np.array([r[3] for r in rows], dtype=np.float32)
            val_old = np.array([r[4] for r in rows], dtype=np.float32)
            mask = np.stack([r[5] for r in rows])
            ret = np.array([r[6] for r in rows], dtype=np.float32)
            adv = ret - val_old
            adv = (adv - adv.mean()) / (adv.std() + 1e-8)

            idx = np.random.permutation(len(rows))
            for s in range(0, len(idx), args.batch_size):
                b = idx[s: s + args.batch_size]
                obs_t = torch.tensor(obs[b], dtype=torch.float32, device=device)
                act_t = torch.tensor(act[b], dtype=torch.long, device=device)
                lp_t = torch.tensor(lp_old[b], dtype=torch.float32, device=device)
                ret_t = torch.tensor(ret[b], dtype=torch.float32, device=device)
                adv_t = torch.tensor(adv[b], dtype=torch.float32, device=device)
                mask_t = torch.tensor(mask[b], dtype=torch.bool, device=device)

                logits, value = net.forward(obs_t, dec, mask_t)
                dist = torch.distributions.Categorical(logits=logits)
                log_prob = dist.log_prob(act_t)
                entropy = dist.entropy()

                v_loss = nn.functional.mse_loss(value, ret_t)

                if policy_frozen:
                    loss = args.value_coef * v_loss
                else:
                    ratio = torch.exp(log_prob - lp_t)
                    surr1 = ratio * adv_t
                    surr2 = torch.clamp(ratio, 1 - args.clip_eps,
                                        1 + args.clip_eps) * adv_t
                    p_loss = -torch.min(surr1, surr2).mean()

                    # KL(new ‖ anchor) on the same masked support.
                    # Use a finite mask value: -inf logits on both sides make
                    # (-inf) - (-inf) = NaN, and NaNs leak through torch.where
                    # into gradients.
                    with torch.no_grad():
                        a_logits, _ = anchor.forward(obs_t, dec, None)
                        a_logits = a_logits.masked_fill(~mask_t, -30.0)
                    a_logp = torch.log_softmax(a_logits, dim=-1)
                    n_logp = torch.log_softmax(
                        logits.masked_fill(~mask_t, -30.0), dim=-1)
                    n_p = n_logp.exp()
                    kl_anchor = (n_p * (n_logp - a_logp)).sum(-1).mean()

                    loss = (p_loss + args.value_coef * v_loss
                            - args.entropy_coef * entropy.mean()
                            + args.kl_coef * kl_anchor)
                    stats["p_loss"].append(p_loss.item())
                    stats["kl_anchor"].append(kl_anchor.item())
                    stats["entropy"].append(entropy.mean().item())
                    stats["approx_kl"].append(
                        (lp_t - log_prob).mean().item())

                if not torch.isfinite(loss):
                    optimizer.zero_grad()
                    stats["skipped_batches"].append(1.0)
                    continue
                optimizer.zero_grad()
                loss.backward()
                if policy_frozen:
                    # warmup trains the value head alone; value gradients
                    # through the shared trunk would shift the policy
                    for name, prm in net.named_parameters():
                        if "value_head" not in name:
                            prm.grad = None
                nn.utils.clip_grad_norm_(net.parameters(), args.max_grad_norm)
                optimizer.step()
                stats["v_loss"].append(v_loss.item())

        # early stop on behaviour drift within the update
        if not policy_frozen and stats["approx_kl"] and \
                abs(np.mean(stats["approx_kl"][-8:])) > args.target_kl:
            break
    return {k: float(np.mean(v)) for k, v in stats.items() if v}


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    p = argparse.ArgumentParser()
    p.add_argument("--jar", default="target/scala-2.12/mahjong-assembly-0.1.0.jar")
    p.add_argument("--checkpoint", required=True)
    p.add_argument("--n-selfplay-envs", type=int, default=8)
    p.add_argument("--n-ff-envs", type=int, default=4)
    p.add_argument("--total-games", type=int, default=60_000)
    p.add_argument("--games-per-update", type=int, default=96)
    p.add_argument("--lr", type=float, default=2e-5)
    p.add_argument("--clip-eps", type=float, default=0.15)
    p.add_argument("--value-coef", type=float, default=0.5)
    p.add_argument("--entropy-coef", type=float, default=0.003)
    p.add_argument("--kl-coef", type=float, default=0.3)
    p.add_argument("--target-kl", type=float, default=0.02)
    p.add_argument("--max-grad-norm", type=float, default=0.5)
    p.add_argument("--n-epochs", type=int, default=2)
    p.add_argument("--batch-size", type=int, default=1024)
    p.add_argument("--value-warmup-games", type=int, default=2000,
                   help="train value head only for the first N games")
    p.add_argument("--seed-offset", type=int, default=5_000_000)
    p.add_argument("--save-dir", default="rl/checkpoints/ppo_a")
    p.add_argument("--save-every", type=int, default=5000)
    p.add_argument("--device", default="cuda")
    args = p.parse_args()

    device = torch.device(args.device)
    save_dir = Path(args.save_dir)
    save_dir.mkdir(parents=True, exist_ok=True)

    net, meta = load_net(args.checkpoint, device=str(device))
    anchor, _ = load_net(args.checkpoint, device=str(device))
    anchor.eval()
    for prm in anchor.parameters():
        prm.requires_grad_(False)
    optimizer = torch.optim.Adam(net.parameters(), lr=args.lr, eps=1e-5)

    def save(path):
        agent = MahjongAgent.__new__(MahjongAgent)
        agent.device, agent.arch, agent.net = device, meta["arch"], net
        agent.save(str(path))

    def seed_gen(start, stride, offset):
        s = start
        while True:
            yield offset + s
            s += stride

    n_env = args.n_selfplay_envs + args.n_ff_envs
    tables = []
    for i in range(args.n_selfplay_envs):
        tables.append(SelfPlayTable(args.jar, seed_gen(i, n_env, args.seed_offset)))
    for i in range(args.n_ff_envs):
        tables.append(FFTable(args.jar,
                              seed_gen(args.n_selfplay_envs + i, n_env,
                                       args.seed_offset)))
    print(f"{args.n_selfplay_envs} self-play tables + {args.n_ff_envs} FF tables"
          f" | anchor={args.checkpoint} | kl_coef={args.kl_coef} lr={args.lr}")
    sys.stdout.flush()

    games = 0
    buffer = []
    ff_money = collections.deque(maxlen=2000)
    sp_seat_money = collections.deque(maxlen=2000)
    t0 = time.time()
    games_since_update = 0
    last_save = 0

    while games < args.total_games:
        act_batch(net, tables, device)
        for t in tables:
            if not t.finished():
                continue
            transitions, rewards = t.result()
            for seat, trs in transitions.items():
                if not trs:
                    continue
                ret = rewards.get(seat, 0.0) / REWARD_SCALE
                for (obs, dec, a, lp, v, mask) in trs:
                    buffer.append((obs, dec, a, lp, v, mask, ret))
            if t.kind == "ff":
                ff_money.append(rewards[0])
            else:
                sp_seat_money.append(rewards[0])
            games += 1
            games_since_update += 1
            t.reset()

        if games_since_update >= args.games_per_update:
            frozen = games < args.value_warmup_games
            st = ppo_update(net, anchor, optimizer, buffer, args, device,
                            policy_frozen=frozen)
            buffer.clear()
            games_since_update = 0
            gps = games / (time.time() - t0)
            ffm = np.mean(ff_money) if ff_money else float("nan")
            print(f"games={games:>7,} ff_money={ffm:+.2f} "
                  f"(n={len(ff_money)}) "
                  f"v_loss={st.get('v_loss', 0):.4f} "
                  f"p_loss={st.get('p_loss', 0):.4f} "
                  f"kl_anchor={st.get('kl_anchor', 0):.4f} "
                  f"ent={st.get('entropy', 0):.3f} "
                  f"{'[value-warmup]' if frozen else ''} gps={gps:.1f}")
            sys.stdout.flush()

        if games - last_save >= args.save_every:
            path = save_dir / f"ppo_games_{games}.pt"
            save(path)
            torch.save({"optimizer_state": optimizer.state_dict(),
                        "games": games}, save_dir / "optim_state.pt")
            print(f"  saved -> {path}")
            sys.stdout.flush()
            last_save = games

    save(save_dir / "ppo_final.pt")
    print(f"DONE {games} games -> {save_dir / 'ppo_final.pt'}")
    for t in tables:
        t.close()


if __name__ == "__main__":
    main()
