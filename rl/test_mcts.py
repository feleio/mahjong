"""
test_mcts.py — Smoke-test for the imperfect-information MCTS pipeline.

Tests:
  1. MCTSRolloutClient starts without error
  2. evaluate_discard() returns valid (mean, std) for multiple candidate tiles
  3. Different tiles get different Q-values (MCTS is actually discriminating)
  4. ImperfectInfoMCTS.get_action_and_policy() returns valid action + policy
  5. Full mini game: collect one MCTS-augmented game end-to-end

Usage
─────
    python rl/test_mcts.py --jar target/scala-2.12/mahjong-assembly-0.1.0.jar

Pass --n-rollouts 3 for a fast smoke test.
"""

import argparse
import sys
import time
from pathlib import Path

import numpy as np
import torch

sys.path.insert(0, str(Path(__file__).parent))

from env import OBS_DIM, encode_state, get_action_mask
from model import MahjongNet
from mcts import MCTSRolloutClient, ImperfectInfoMCTS, PairedMCTSPolicy
from mcts_train import MCTSGameCollector


# ── Helpers ───────────────────────────────────────────────────────────────────

PASS = "\033[32mPASS\033[0m"
FAIL = "\033[31mFAIL\033[0m"


def check(condition: bool, msg: str) -> bool:
    status = PASS if condition else FAIL
    print(f"  [{status}] {msg}")
    return condition


# ── Minimal synthetic game state ──────────────────────────────────────────────

def make_synthetic_state(hand_tiles=(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)):
    """
    Build a minimal synthetic state dict matching the Scala server format.
    hand_tiles: tile IDs that make up the current hand (13 tiles at start,
                we'll add tile 13 to simulate post-draw = 14 tiles).
    """
    hand_counts = [0] * 34
    for t in hand_tiles:
        hand_counts[t] += 1
    # Add one more tile as if just drawn
    hand_counts[13] += 1  # 14 tiles total

    return {
        "hand":     hand_counts,
        "my_groups": {"pongs": [], "kongs": [], "chows": []},
        "opp_groups": [
            {"pongs": [], "kongs": [], "chows": []},
            {"pongs": [], "kongs": [], "chows": []},
            {"pongs": [], "kongs": [], "chows": []},
        ],
        "discarded":  [0] * 34,
        "remaining":  60,
        "my_id":      0,
        "cur_player_id": 0,
    }


# ── Tests ──────────────────────────────────────────────────────────────────────

def test_rollout_client_starts(jar: str, n_rollouts: int) -> MCTSRolloutClient:
    print("\n[Test 1] MCTSRolloutClient startup")
    try:
        client = MCTSRolloutClient(jar_path=jar, n_rollouts=n_rollouts, rollout_opp="chicken")
        check(True, f"RolloutServer started (pid={client._proc.pid})")
        return client
    except Exception as e:
        check(False, f"Failed to start: {e}")
        sys.exit(1)


def test_evaluate_discard(client: MCTSRolloutClient) -> None:
    print("\n[Test 2] evaluate_discard — single tile")
    state = make_synthetic_state()
    t0 = time.time()
    mean, std = client.evaluate_discard(state, discard_tile=13)
    elapsed = time.time() - t0
    check(isinstance(mean, float), f"mean_reward is float: {mean:.3f}")
    check(isinstance(std, float),  f"std_reward  is float: {std:.3f}")
    check(elapsed < 60.0,          f"responded in {elapsed:.2f}s")


def test_multiple_tiles_differ(client: MCTSRolloutClient) -> None:
    print("\n[Test 3] evaluate_discard — multiple tiles have different Q-values")
    state  = make_synthetic_state()
    # Evaluate tiles 0-5 (all in hand)
    qs = {}
    for tile in range(6):
        mean, _ = client.evaluate_discard(state, discard_tile=tile)
        qs[tile] = mean
        print(f"          tile {tile}: Q={mean:+.3f}")

    values = list(qs.values())
    all_same = all(abs(v - values[0]) < 0.01 for v in values)
    check(not all_same, "Q-values are not all identical (MCTS discriminates)")


def test_mcts_policy(jar: str, client: MCTSRolloutClient, device: str) -> None:
    print("\n[Test 4] ImperfectInfoMCTS.get_action_and_policy")
    net  = MahjongNet(hidden_size=256)
    mcts = ImperfectInfoMCTS(net=net, rollout_client=client, temperature=1.0, device=device)

    state = make_synthetic_state()
    obs   = encode_state(state)
    # Build a discard action mask: tiles in hand are valid
    mask  = np.array(state["hand"], dtype=bool)  # 34-element
    context = {"valid_tiles": [i for i, c in enumerate(state["hand"]) if c > 0]}

    action, policy, value = mcts.get_action_and_policy(
        obs, state, "discard", context, mask
    )

    check(0 <= action <= 33,                    f"action is valid tile id: {action}")
    check(policy.shape == (34,),                f"policy shape: {policy.shape}")
    check(abs(policy.sum() - 1.0) < 1e-4,       f"policy sums to 1: {policy.sum():.6f}")
    check(policy[action] > 0.0,                  f"selected action has non-zero prob")
    check(isinstance(value, float),              f"value is float: {value:.4f}")

    print(f"          Selected tile: {action}  |  policy max={policy.max():.3f}  |  value={value:.3f}")

    # Also test a binary decision (should skip MCTS)
    binary_obs  = obs
    binary_mask = np.array([True, True])
    a2, pol2, _ = mcts.get_action_and_policy(binary_obs, state, "win", {}, binary_mask)
    check(a2 in (0, 1), f"win action is 0 or 1: {a2}")
    check(pol2.shape == (2,), f"win policy shape: {pol2.shape}")


def test_full_game(jar: str, client: MCTSRolloutClient, device: str) -> None:
    print("\n[Test 5] Full MCTS-augmented game vs Chicken")
    net  = MahjongNet(hidden_size=256)
    mcts = ImperfectInfoMCTS(net=net, rollout_client=client, temperature=1.0, device=device)

    collector = MCTSGameCollector(
        jar_path  = jar,
        mcts      = mcts,
        opponent  = "chicken",
    )

    t0 = time.time()
    try:
        steps, reward = collector.collect_game(seed=42)
        elapsed = time.time() - t0
        n_discard = sum(1 for s in steps if s.decision == "discard")
        check(len(steps) > 0,   f"game produced {len(steps)} decision steps")
        check(n_discard > 0,    f"  of which {n_discard} were discard decisions (MCTS used)")
        check(isinstance(reward, float), f"final reward: {reward:.2f}")
        check(elapsed < 300.0,  f"completed in {elapsed:.1f}s")

        # Verify MCTS policy shape for discard steps
        for s in steps:
            if s.decision == "discard":
                check(s.mcts_policy.shape == (34,),
                      f"discard policy shape: {s.mcts_policy.shape}")
                check(abs(s.mcts_policy.sum() - 1.0) < 1e-4,
                      f"discard policy sums to 1: {s.mcts_policy.sum():.6f}")
                break
    except Exception as e:
        check(False, f"Game failed: {e}")
        import traceback; traceback.print_exc()
    finally:
        collector.close()


def test_evaluate_batch(jar: str) -> None:
    print("\n[Test 6] evaluate_batch — paired worlds, FF opponents + FF self-policy")
    client = MCTSRolloutClient(jar_path=jar, rollout_opp="firstfelix")
    try:
        state      = make_synthetic_state()
        candidates = [0, 1, 5, 13, 9, 12]
        n_worlds   = 16

        t0 = time.time()
        rewards = client.evaluate_batch(state, candidates, n_worlds)
        elapsed = time.time() - t0

        check(rewards.shape == (n_worlds, len(candidates)),
              f"rewards shape {rewards.shape} == ({n_worlds}, {len(candidates)})")
        check(np.isfinite(rewards).all(), "all rewards finite")
        per_decision = elapsed
        check(elapsed < 120.0, f"batch of {n_worlds}x{len(candidates)} rollouts in {elapsed:.2f}s "
                               f"({elapsed / (n_worlds * len(candidates)) * 1000:.0f}ms/rollout)")

        # Paired variance reduction: SD of paired diffs should be well below
        # SD of raw rewards (they share world outcomes like draws).
        diffs = rewards[:, 1] - rewards[:, 0]
        print(f"          raw reward SD={rewards.std():.2f}  paired diff SD={diffs.std():.2f}")
        print(f"          mean rewards per candidate: "
              + ", ".join(f"{t}:{m:+.2f}" for t, m in zip(candidates, rewards.mean(axis=0))))
    finally:
        client.close()


def test_paired_policy(jar: str, device: str) -> None:
    print("\n[Test 7] PairedMCTSPolicy.get_action")
    client = MCTSRolloutClient(jar_path=jar, rollout_opp="firstfelix")
    try:
        net    = MahjongNet(hidden_size=256)
        policy = PairedMCTSPolicy(net=net, rollout_client=client,
                                  n_worlds=16, top_k=4, device=device)

        state   = make_synthetic_state()
        obs     = encode_state(state)
        mask    = np.array([c > 0 for c in state["hand"]], dtype=bool)
        context = {"valid_tiles": [i for i, c in enumerate(state["hand"]) if c > 0]}

        t0 = time.time()
        action, info = policy.get_action(obs, state, "discard", context, mask)
        elapsed = time.time() - t0

        check(0 <= action <= 33 and mask[action], f"action is a valid hand tile: {action}")
        check("nn_action" in info and "switched" in info, f"info populated: {info}")
        check(elapsed < 120.0, f"discard decision in {elapsed:.2f}s")
        print(f"          nn_action={info['nn_action']}  chosen={action}  "
              f"switched={info['switched']}  gain={info.get('mean_gain', 0):.2f}")

        # Binary decision should bypass MCTS entirely and be fast
        t0 = time.time()
        a2, info2 = policy.get_action(obs, state, "win", {}, np.array([True, True]))
        check(a2 in (0, 1) and not info2["switched"],
              f"binary decision bypasses MCTS: action={a2} in {time.time()-t0:.3f}s")
    finally:
        client.close()


def test_search_depth1_matches_flat(jar: str, nn_model: str) -> None:
    print("\n[Test 8] search (depth-1) ≈ flat evaluate_batch")
    if not nn_model or not Path(nn_model).exists():
        check(False, f"skipped: no --nn-model (search needs -Drl.nnmodel); got {nn_model!r}")
        return

    from mcts import SearchPolicy  # noqa: F401  (import-smoke)

    # Same opponent + same (nn) self-policy in both, so at max_depth=1 the tree
    # only branches at the root and every deeper discard is the NN tail — exactly
    # what evaluate_batch does with self_policy="nn". Root Q(a) should therefore
    # match the flat mean_rewards[a] in expectation.
    client = MCTSRolloutClient(jar_path=jar, rollout_opp="firstfelix", nn_model=nn_model,
                               rollout_threads=3)
    try:
        state      = make_synthetic_state()
        candidates = [0, 1, 5, 13, 9, 12]

        # Flat: many worlds, our seat plays nn.
        n_worlds = 400
        flat = client.evaluate_batch(state, candidates, n_worlds, self_policy="nn")
        flat_mean = flat.mean(axis=0)  # aligned to candidates

        # Depth-1 tree, forced ~uniform root exploration (huge c_puct) so every
        # candidate is visited enough for its Q=w/n to be an accurate estimate.
        # This is the true equivalence the plan targets: root Q(a) == flat
        # mean_rewards[a] in expectation, independent of the (possibly skewed)
        # net prior. With a normal c_puct PUCT would instead concentrate on the
        # prior-preferred move — correct behaviour, but not a Q-accuracy test.
        sims = 1800
        resp = client.search(state, candidates, sims, max_depth=1, n_parallel=3,
                             rollout_tail="inf", rollout_opp="firstfelix",
                             temperature=1.0, c_puct=1e6)
        tiles  = [int(t) for t in resp["tiles"]]
        visits = np.asarray(resp["visits"], dtype=np.float64)
        qs     = np.asarray(resp["q"],      dtype=np.float64)
        policy = np.asarray(resp["policy"], dtype=np.float64)

        check(tiles == candidates,                       f"tiles echo candidates: {tiles}")
        check(np.isfinite(qs).all() and np.isfinite(visits).all(), "q/visits finite")
        check(int(visits.sum()) == sims,                 f"visits sum to sims: {int(visits.sum())}=={sims}")
        check(abs(policy.sum() - 1.0) < 1e-6,            f"policy sums to 1: {policy.sum():.6f}")
        check((visits > 0).all(),                        "every candidate explored under forced exploration")

        print(f"          flat mean : " + ", ".join(f"{t}:{m:+.2f}" for t, m in zip(candidates, flat_mean)))
        print(f"          tree q    : " + ", ".join(f"{t}:{q:+.2f}" for t, q in zip(tiles, qs)))
        print(f"          visits    : " + ", ".join(f"{t}:{int(v)}" for t, v in zip(tiles, visits)))

        # Per-tile Q should track flat means (same rollout, same policies).
        tree_best = tiles[int(np.argmax(qs))]
        flat_best = candidates[int(np.argmax(flat_mean))]
        check(tree_best == flat_best,
              f"tree argmax-Q ({tree_best}) == flat argmax-mean ({flat_best})")
        corr = np.corrcoef(qs, flat_mean)[0, 1]
        check(corr > 0.9, f"per-tile Q vs flat-mean correlation: {corr:.3f}")

        # Sanity: with the default c_puct + Q-normalization, PUCT concentrates on
        # a high-value move and the visit policy is peaked (not a hard equality,
        # just that normalization keeps exploration from starving — reported).
        resp2 = client.search(state, candidates, 600, max_depth=1, n_parallel=3,
                              rollout_tail="inf", rollout_opp="firstfelix", c_puct=1.5)
        v2 = np.asarray(resp2["visits"], dtype=np.float64)
        print(f"          default c_puct visits: "
              + ", ".join(f"{t}:{int(v)}" for t, v in zip(resp2["tiles"], v2)))
    finally:
        client.close()


def test_value_leaf_batch(jar: str, nn_model: str, value_model: str) -> None:
    print("\n[Test 9] evaluate_batch value-leaf hybrid (CRN + 1-ply value bootstrap)")
    if not nn_model or not Path(nn_model).exists() or \
       not value_model or not Path(value_model).exists():
        check(False, f"skipped: needs --nn-model and --value-model; got "
                     f"{nn_model!r}, {value_model!r}")
        return

    client = MCTSRolloutClient(jar_path=jar, rollout_opp="firstfelix",
                               nn_model=nn_model, value_model=value_model,
                               rollout_threads=3)
    try:
        state      = make_synthetic_state()
        candidates = [0, 1, 5, 13, 9, 12]
        n_worlds   = 32

        t0 = time.time()
        rv = client.evaluate_batch(state, candidates, n_worlds,
                                   self_policy="nn", value_leaf_plies=1)
        t_value = time.time() - t0
        t0 = time.time()
        rf = client.evaluate_batch(state, candidates, n_worlds, self_policy="nn")
        t_full = time.time() - t0

        check(rv.shape == (n_worlds, len(candidates)),
              f"rewards shape {rv.shape} == ({n_worlds}, {len(candidates)})")
        check(np.isfinite(rv).all(), "all rewards finite")
        # The 1-ply infoset depends on the sampled world (opponent responses,
        # our draw), so the value estimates must vary across worlds — this is
        # the "0-ply degenerates to greedy-on-the-value-net" guard.
        check(rv.std(axis=0).max() > 1e-6,
              f"leaf values vary across worlds (max per-tile SD={rv.std(axis=0).max():.3f})")
        means = rv.mean(axis=0)
        check(not all(abs(m - means[0]) < 1e-9 for m in means),
              "per-candidate means differ (hybrid discriminates)")

        # CRN payoff: paired diffs vs candidate 0 should be tighter than the
        # full-rollout ones (value net is deterministic per infoset). Reported,
        # asserted loosely (<=) since both are already small on synthetic hands.
        dv = (rv - rv[:, :1]).std()
        df = (rf - rf[:, :1]).std()
        check(dv <= df + 1e-9, f"paired diff SD: value-leaf {dv:.2f} <= full {df:.2f}")
        check(t_value < t_full * 1.5,
              f"value-leaf batch not slower than full rollouts ({t_value:.2f}s vs {t_full:.2f}s)")
        print(f"          means: " + ", ".join(f"{t}:{m:+.2f}" for t, m in zip(candidates, means)))
    finally:
        client.close()


# ── CLI ───────────────────────────────────────────────────────────────────────

def parse_args() -> argparse.Namespace:
    p = argparse.ArgumentParser(description="MCTS smoke tests")
    p.add_argument("--jar",        required=True,  type=str)
    p.add_argument("--java",       default="java", type=str)
    p.add_argument("--n-rollouts", default=5,      type=int)
    p.add_argument("--device",     default="cpu",  type=str)
    p.add_argument("--nn-model",   default="rl/checkpoints/student/student48.onnx", type=str,
                   help="student ONNX for the search test (-Drl.nnmodel)")
    p.add_argument("--value-model", default="rl/checkpoints/student/student48_valuenet.onnx",
                   type=str, help="value ONNX for the value-leaf test (-Drl.valuemodel)")
    return p.parse_args()


def main() -> None:
    args = parse_args()

    print("=" * 60)
    print("MCTS Pipeline Smoke Test")
    print(f"  jar={args.jar}  n_rollouts={args.n_rollouts}  device={args.device}")
    print("=" * 60)

    client = test_rollout_client_starts(args.jar, args.n_rollouts)
    try:
        test_evaluate_discard(client)
        test_multiple_tiles_differ(client)
        test_mcts_policy(args.jar, client, args.device)
        test_full_game(args.jar, client, args.device)
    finally:
        client.close()

    test_evaluate_batch(args.jar)
    test_paired_policy(args.jar, args.device)
    test_search_depth1_matches_flat(args.jar, args.nn_model)
    test_value_leaf_batch(args.jar, args.nn_model, args.value_model)
    print("\nDone.")


if __name__ == "__main__":
    main()
