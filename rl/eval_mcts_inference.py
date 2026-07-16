"""Compare baseline (no MCTS) vs MCTS at inference time vs FirstFelix."""
import sys, time
sys.path.insert(0, 'rl')
import numpy as np
import torch
from model import MahjongNet, MahjongAgent
from env import MahjongEnv
from mcts import MCTSRolloutClient, ImperfectInfoMCTS

JAR  = 'target/scala-2.12/mahjong-assembly-0.1.0.jar'
CKPT = 'rl/checkpoints/imitation_v1/imitation_final.pt'
N    = 500

# ── Baseline: original model, deterministic, no MCTS ─────────────────────────
print("Evaluating baseline (no MCTS)...")
agent = MahjongAgent.load(CKPT, device='cpu')
env   = MahjongEnv(JAR, opponent='firstfelix')
base_rewards = []
t0 = time.time()
for i in range(N):
    obs, info = env.reset(seed=i)
    while True:
        action = agent.select_action(obs, info['decision'], info['action_mask'], deterministic=True)
        obs, r, done, info = env.step(action)
        if done:
            base_rewards.append(r)
            break
env.close()
elapsed = time.time() - t0
print(f"  mean={np.mean(base_rewards):.2f}  win%={np.mean([r>0 for r in base_rewards]):.1%}  n={N}  t={elapsed:.0f}s")
sys.stdout.flush()

# ── MCTS inference: FF rollouts, greedy (τ=0.1) ───────────────────────────────
print("Evaluating MCTS inference (FF rollouts, n=20, tau=0.1)...")
from env import OBS_DIM
net  = MahjongNet(obs_dim=OBS_DIM)
ckpt = torch.load(CKPT, map_location='cpu', weights_only=False)
net.load_state_dict(ckpt['net_state'])

client = MCTSRolloutClient(JAR, n_rollouts=20, rollout_opp='firstfelix')
mcts   = ImperfectInfoMCTS(net=net, rollout_client=client, temperature=0.1, device='cpu')
env2   = MahjongEnv(JAR, opponent='firstfelix')
mcts_rewards = []
t0 = time.time()
for i in range(N):
    obs, info = env2.reset(seed=i)
    while True:
        action, _, _ = mcts.get_action_and_policy(
            obs, info['state'], info['decision'], info['context'], info['action_mask']
        )
        obs, r, done, info = env2.step(action)
        if done:
            mcts_rewards.append(r)
            break
env2.close()
client.close()
elapsed = time.time() - t0
print(f"  mean={np.mean(mcts_rewards):.2f}  win%={np.mean([r>0 for r in mcts_rewards]):.1%}  n={N}  t={elapsed:.0f}s")
sys.stdout.flush()

print("\nSummary:")
print(f"  Baseline :  {np.mean(base_rewards):+.2f}/game  {np.mean([r>0 for r in base_rewards]):.1%} wins")
print(f"  MCTS inf :  {np.mean(mcts_rewards):+.2f}/game  {np.mean([r>0 for r in mcts_rewards]):.1%} wins")
print(f"  Delta    :  {np.mean(mcts_rewards)-np.mean(base_rewards):+.2f}/game")
