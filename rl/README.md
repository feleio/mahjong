# Mahjong RL Framework

Reinforcement learning framework for training a deep learning agent to play
**Hong Kong-style 4-player Mahjong** using the existing Scala simulator.

---

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                       Python RL Framework                    │
│                                                              │
│  train.py / self_play.py                                     │
│       │                                                      │
│       ▼                                                      │
│  PPOTrainer  ──────────►  MahjongNet (PyTorch)               │
│       │                     ├── Shared encoder (512→256)     │
│       │                     ├── Value head                   │
│       ▼                     ├── Discard head (→34)           │
│  MahjongEnv (gym)           ├── Binary head (→2)             │
│       │                     ├── Chow head (→4)               │
│       │ stdin/stdout JSON   └── Self-kong head (→35)         │
│       ▼                                                      │
└──────────────────────────────────────────────────────────────┘
                │
                ▼  (subprocess)
┌─────────────────────────────────────────────────────────────┐
│                     Scala Simulator                          │
│                                                              │
│  RLGymServer                                                 │
│       │                                                      │
│       ├── RLPlayer (seat 0)  ◄──── talks to Python          │
│       ├── Chicken  (seat 1)                                  │
│       ├── Chicken  (seat 2)                                  │
│       └── Chicken  (seat 3)                                  │
└─────────────────────────────────────────────────────────────┘
```

### Communication protocol

Every message is a **newline-delimited JSON** line.

| Direction | Message | Purpose |
|-----------|---------|---------|
| Python → Scala | `{"cmd": "reset", "seed": 42}` | Start new game |
| Scala → Python | `{"type": "observation", "decision": "discard", "state": {...}, "context": {...}}` | Ask for action |
| Python → Scala | `{"action": 5}` | Submit action |
| Scala → Python | `{"type": "game_over", "reward": 16.0, ...}` | Game finished |

---

## Quick Start

### 1. Build the fat-JAR

```bash
# From project root
sbt assembly
# → target/scala-2.12/mahjong-assembly-0.1.0.jar
```

### 2. Install Python dependencies

```bash
pip install -r rl/requirements.txt
```

### 3. Train (vs Chicken bots)

```bash
python rl/train.py \
    --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \
    --total-steps 2_000_000 \
    --device cpu
```

### 4. AlphaZero-style self-play

All four seats use the same shared network, so the agent bootstraps
its own curriculum — no fixed opponents needed.

```bash
python rl/self_play.py \
    --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \
    --total-games 100_000 \
    --device cpu
```

### 5. Evaluate

```bash
python rl/evaluate.py \
    --checkpoint rl/checkpoints/selfplay_final.pt \
    --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \
    --n-games 500
```

### 6. Strategy analysis (learn from the AI)

```bash
python rl/strategy_analysis.py \
    --checkpoint rl/checkpoints/selfplay_final.pt \
    --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \
    --n-games 500
```

Sample output:

```
============================================================
  STRATEGY TIPS FOR HUMAN PLAYERS
============================================================

  [1] DISCARD HONOURS EARLY: The agent discards honour tiles
      (winds & dragons) 42% of the time.  These tiles can
      only form Pong/Kong groups and don't form sequences —
      discard them early unless you have a pair.

  [2] ALWAYS WIN WHEN YOU CAN: The agent accepts virtually
      every win opportunity.  Never pass on a valid win …
```

---

## Training on AWS (GPU)

### EC2 (p3.2xlarge — 1× V100)

```bash
python rl/aws_train.py --mode ec2 \
    --key-name my-keypair \
    --security-group sg-0abc123 \
    --region us-east-1 \
    --instance-type p3.2xlarge \
    --total-games 500_000
```

The script will:
1. Launch a GPU EC2 instance
2. Upload the project via rsync
3. Install Java, sbt, and PyTorch (CUDA)
4. Build the fat-JAR with `sbt assembly`
5. Run `self_play.py` and stream logs back to your terminal

### SageMaker (managed, automatic S3 output)

```bash
python rl/aws_train.py --mode sagemaker \
    --role-arn arn:aws:iam::123456789:role/SageMakerRole \
    --s3-bucket my-mahjong-bucket \
    --instance-type ml.p3.2xlarge \
    --total-games 500_000
```

Checkpoints are saved to `s3://my-mahjong-bucket/mahjong-rl/`.

---

## State Space (485 features)

| Segment | Size | Description |
|---------|------|-------------|
| hand | 34 | Count of each tile type in hand (÷4) |
| my_pong | 34 | 1 if I have a pong of tile i |
| my_kong | 34 | 1 if I have a kong of tile i |
| my_chow | 34 | 1 if tile i is in one of my chow groups |
| opp{0,1,2}_pong | 34×3 | Opponents' ponged tiles |
| opp{0,1,2}_kong | 34×3 | Opponents' konged tiles |
| opp{0,1,2}_chow | 34×3 | Tiles in opponents' chow groups |
| discarded | 34 | Total discards per tile type (÷4) |
| remaining | 1 | Tiles left in wall (÷136) |
| my_seat | 4 | One-hot of agent's seat |
| cur_player | 4 | One-hot of current player |

## Action Spaces

| Decision | Space | Meaning |
|----------|-------|---------|
| discard | Discrete(34) | Tile ID to discard |
| win / self_win | Discrete(2) | 0=pass, 1=win |
| pong / kong | Discrete(2) | 0=pass, 1=accept |
| chow | Discrete(4) | 0=pass, 1=LEFT, 2=MIDDLE, 3=RIGHT |
| self_kong | Discrete(35) | 0=pass, 1–34=tile_id+1 |

## Reward

| Outcome | Reward |
|---------|--------|
| Self-drawn win (score S) | +scoreMap[S] × 1.5 |
| Win on opponent's discard | +scoreMap[S] |
| Responsible discard (you lose) | −Σ scoreMap[winner_score] |
| Draw / other | 0 |

Default score map: 3pts→8, 4pts→16, 5pts→24, 6pts→32, 7pts→48, 8pts→64

---

## File Reference

| File | Description |
|------|-------------|
| `rl/env.py` | Gym-compatible Python environment |
| `rl/model.py` | Multi-head actor-critic network |
| `rl/ppo.py` | PPO algorithm & rollout buffer |
| `rl/train.py` | Train vs Chicken bots |
| `rl/self_play.py` | AlphaZero-style 4-seat self-play |
| `rl/evaluate.py` | Evaluate a trained checkpoint |
| `rl/strategy_analysis.py` | Extract strategy tips for humans |
| `rl/aws_train.py` | Launch training on AWS EC2 / SageMaker |
| `rl/requirements.txt` | Python dependencies |
| `src/.../rl/RLPlayer.scala` | Scala RL player (stdin/stdout JSON) |
| `src/.../rl/RLGymServer.scala` | Scala game server main entry point |
