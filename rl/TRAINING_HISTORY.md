# Mahjong RL Training History

All training runs in chronological order. EMA = exponential moving average (α=0.05) of per-episode reward, not exact evaluation figures.

---

## Run 1 — Early Proof-of-Concept
**Log:** `rl/train_local.log`
**From scratch** | Opponent: Chicken | lr: 3e-4 | Device: CPU

| Metric | Value |
|--------|-------|
| Total steps | 500,770 |
| Total episodes | 19,294 |
| Final EMA reward | -1.0 |
| Final entropy | 1.33 |
| Throughput | ~1,382 sps |

- Old logging format (reward, win_rate fields, not money/avg_win_pay)
- Tenpai reward shaping NOT yet implemented
- Final checkpoint overwritten

---

## Run 2 — Chicken Training with Tenpai Shaping  ✓ BEST CHICKEN MODEL
**Log:** `rl/train_tenpai.log`
**From scratch** | Opponent: Chicken | lr: 3e-4 | Device: CUDA

| Metric | Value |
|--------|-------|
| Total steps | 2,001,440 |
| Total episodes | 74,447 |
| Final EMA money | +4.5 |
| Final EMA avg_win_pay | +31.6 |
| Final entropy | 0.18 |
| Throughput | ~1,517 sps |

- First run with tenpai reward shaping (+$2 for reaching tenpai after discard)
- **Key checkpoint: `rl/checkpoints/step_1918866.pt`** — used as base for all subsequent fine-tuning
- Final checkpoint also saved as `rl/checkpoints/final.pt` (later overwritten)

**Evaluation (500 games, deterministic=False):**
| vs | $/game | Win rate | Draw rate | Notes |
|----|--------|----------|-----------|-------|
| Chicken | **+$0.48** | 5.0% | 84.8% | Slightly above random; 85% draws are natural with minScore=3 |

---

## Run 3 — Fine-tune vs FirstFelix (Pure)  ✗ CATASTROPHIC FORGETTING
**Log:** `rl/train_firstfelix.log`
**Resumed from:** `rl/checkpoints/final.pt` (= end of Run 2) at step 2,001,440
**Opponent: FirstFelix only** | lr: 3e-4 | Device: CUDA

| Metric | Value |
|--------|-------|
| Steps this run | ~2,000,000 (2M → 4M) |
| Total steps | 4,000,004 |
| Total episodes | 150,911 |
| Final EMA money | +0.6 |
| Final EMA avg_win_pay | +26.2 |
| Final entropy | 0.10 (very collapsed) |
| Throughput | ~3,182 sps |

- Started at money=-7.2 (vs FF), gradually improved to +0.6 EMA by end
- Low entropy (0.10) indicates policy over-converged to FF-specific behaviour
- **Catastrophic forgetting:** Chicken performance collapsed

**Evaluation (500 games):**
| vs | $/game | Win rate | Notes |
|----|--------|----------|-------|
| Chicken | -$0.29 | ~3% | Forgot Chicken strategy |
| FirstFelix | -$6.60 | ~3% | Still losing to FF despite FF-only training |

---

## Run 4 — Mixed Training (50/50), Default LR  ✗ STILL FORGETTING
**Log:** `rl/train_mixed.log`
**Resumed from:** `rl/checkpoints/step_1918866.pt` at step 1,918,866
**Opponent: mixed (50% Chicken / 50% FF per game)** | lr: 3e-4 | Device: CUDA

| Metric | Value |
|--------|-------|
| Steps this run | ~2,100,000 (1.9M → 4M) |
| Total steps | 4,000,852 |
| Total episodes | 149,898 |
| Final EMA money | -1.4 |
| Final EMA avg_win_pay | +16.9 |
| Final entropy | 0.16 |
| Throughput | ~2,979 sps |

- Hypothesis: 50/50 mix would prevent forgetting. It didn't at lr=3e-4.
- FF's large negative gradients (~$-28/loss vs Chicken's ~$-7/loss) still dominated
- Final checkpoint: `rl/checkpoints/final.pt` (overwritten)

**Evaluation (500 games):**
| vs | $/game | Win rate | Draw rate | Notes |
|----|--------|----------|-----------|-------|
| Chicken | -$0.36 | 3.4% | 83.4% | Worse than original Chicken baseline |
| FirstFelix | -$6.96 | 3.0% | 71.8% | No improvement over baseline |

---

## Run 5 — Mixed Training (50/50), Lower LR  ✗ UNSTABLE LATE
**Log:** `rl/train_mixed_low_lr.log`
**Resumed from:** `rl/checkpoints/step_1918866.pt` at step 1,918,866
**Opponent: mixed (50% Chicken / 50% FF per game)** | lr: 3e-5 (10× lower) | Device: CUDA

| Metric | Value |
|--------|-------|
| Steps this run | ~2,100,000 (1.9M → 4M) |
| Total steps | 4,001,034 |
| Total episodes | 149,242 |
| Final EMA money | -2.7 |
| Final EMA avg_win_pay | +21.0 |
| Final entropy | 0.35 (spiked at end) |
| Throughput | ~3,001 sps |
| Save dir | `rl/checkpoints/mixed_low_lr/` |

- Lower LR maintained healthy entropy (~0.20) through most of training
- EMA money improved to -0.3 around step 2.5M (best point)
- Policy went unstable in final ~500k steps: entropy spiked 0.19 → 0.35, money dropped -0.3 → -2.7
- Final checkpoint broken (0 wins vs Chicken)

**Evaluation (500 games):**
| Checkpoint | vs | $/game | Win rate | Notes |
|------------|-----|--------|----------|-------|
| `step_2525049.pt` (~2.5M) | Chicken | **-$0.10** | 3.4% | Best intermediate, nearly breakeven |
| `step_2525049.pt` (~2.5M) | FirstFelix | -$7.02 | 2.6% | No improvement over baseline |
| `final.pt` (4M) | Chicken | -$1.26 | **0.0%** | Fully collapsed — do not use |

---

## Run 6 — From Scratch vs FirstFelix, 8 Parallel Envs  ✗ STUCK
**Log:** `rl/train_ff_scratch.log`
**From scratch** | Opponent: FirstFelix | lr: 3e-4 | entropy-coef: 0.02 | n-envs: 8 | Device: CUDA

| Metric | Value |
|--------|-------|
| Total steps | ~3,100,000 (killed early) |
| Final EMA money | -7.0 (flat throughout) |
| Final entropy | ~1.20 |
| Throughput | ~1,300 sps |
| Save dir | `rl/checkpoints/ff_scratch/` |

- First run using `--n-envs 8` (VecMahjongEnv with 8 parallel JVM processes)
- EMA money was -7.0 from step 20k all the way to 3.1M — completely flat, no learning
- **Root cause:** training from scratch directly vs a strong opponent provides no useful gradient signal — the model loses every game without knowing why
- Killed early; curriculum learning or self-play needed instead

---

## Self-Play Experiments (S1–S4)

### S1 — True 4-Seat Self-Play, Cold Start  ✗ ZERO LEARNING SIGNAL
**Log:** `rl/train_selfplay_scratch.log`
**From scratch** | True self-play (all 4 seats = shared network) | entropy-coef: 0.02 | Device: CUDA

| Metric | Value |
|--------|-------|
| Total games | ~491,000 / 500,000 (process died) |
| Final win rate | 0–0.59% (sporadic, no trend) |
| Final entropy | 1.77 (near-random throughout) |
| Throughput | ~15–17 gps |

- **Infrastructure change:** Scala server modified to support `-Drl.selfplay=true` — all 4 seats become RLPlayers, each observation tagged with `seat_id`, game_over sends `rewards: {0:.., 1:.., 2:.., 3:..}` for all seats
- **Root cause of failure:** 4 random players never reach tenpai → <0.5% win rate → near-zero gradient signal → policy stays random forever
- AlphaZero avoids this with MCTS; without tree search, pure self-play from scratch cannot bootstrap in a high-draw game

### S2 — True Self-Play, Warm Start (Run 2 checkpoint)  ✗ DRAW FARMING
**Log:** `rl/train_selfplay_warmstart.log`
**Resumed from:** `rl/checkpoints/step_1918866.pt` | entropy-coef: 0.02 | lr: 3e-4

| Metric | Value |
|--------|-------|
| Total games | ~139,000 (killed early) |
| Starting entropy | 0.40 |
| Final entropy | 1.55 (rose throughout) |
| Win rate | 0–1.17% (no trend) |

- Warm start gave immediate wins (0.59% from game 1k vs 0% cold-start)
- But entropy rose from 0.40 → 1.55 over 139k games — model unlearning its strategy
- **Root cause:** symmetric zero-sum self-play converges to Nash equilibrium = mutual draw-farming. All seats explore simultaneously, nobody has a stable strategy to exploit.

**Evaluation (500 games, `selfplay_e008/selfplay_final.pt`, entropy-coef=0.008):**
| vs | $/game | Win rate | Notes |
|----|--------|----------|-------|
| Chicken | -$0.80 | 0.6% | Regressed from Run 2 baseline (+$0.48) |
| FirstFelix | -$7.31 | 0.6% | No improvement |

### S3 — Hybrid Self-Play (70% self-play + 30% Chicken)  ✗ STILL REGRESSED
**Log:** `rl/train_selfplay_hybrid.log`
**Resumed from:** `rl/checkpoints/step_1918866.pt` | hybrid-ratio: 0.3 | entropy-coef: 0.01 | lr: 1e-4

| Metric | Value |
|--------|-------|
| Total games | 500,000 (completed) |
| Starting entropy | 0.31 |
| Final entropy | 1.55 (same drift) |
| Score bonus | 5pt +20%, 7pt +50%, 8pt +100% added to Scala server |

- 30% Chicken games were not enough to prevent entropy drift
- Score bonus for high-value wins added to reward shaping (Scala change) but effect unclear
- **Evaluation:** -$1.46 vs Chicken (0% wins), -$7.71 vs FF — worst result yet
- **Conclusion:** self-play without MCTS reliably destroys warm-start strategies regardless of mix ratio

### S4 — Self-Play Conclusion
Self-play experiments (S1–S3) all regressed the Run 2 baseline. The failure mode is consistent: entropy rises, policy forgets its strategy, draw rate increases, reward signal disappears. Without MCTS to guide search, pure self-play cannot stably improve in Hong Kong Mahjong due to the 85% draw rate providing insufficient win signal.

---

## Run 7 — Mixed Training, lr=1e-5  ✓ BEST FF PERFORMANCE
**Log:** `rl/train_mixed_very_low_lr.log`
**Resumed from:** `rl/checkpoints/mixed_low_lr/step_2525049.pt` at step 2,525,049
**Opponent: mixed (50/50)** | lr: 1e-5 | entropy-coef: 0.005 | n-envs: 8 | Device: CUDA

| Metric | Value |
|--------|-------|
| Steps this run | ~2,000,000 (2.5M → 4.5M) |
| Total steps | 4,500,821 |
| Total episodes | 173,073 |
| Starting entropy | 0.17 |
| Final entropy | 0.31 |
| Throughput | ~1,300–1,500 sps |
| Save dir | `rl/checkpoints/mixed_very_low_lr/` |

- Entropy stable for longer than all previous runs (0.17 → 0.31 over 2M steps vs 0.19 → 0.35 in Run 5)
- Model peaked early (~150k steps after resume) then gradually degraded — same collapse pattern
- **Pattern confirmed:** model always peaks 50–150k steps after resuming, then degrades

**Evaluation (500 games):**
| Checkpoint | vs Chicken | Win% | vs FF | Win% | Avg win score | Notes |
|------------|-----------|------|-------|------|---------------|-------|
| `step_2679831.pt` (~2.68M) | -$0.37 | 4.4% | **-$6.55** | **3.4%** | 3.82pt | **Best FF $/game ever** |
| `final.pt` (4.5M) | -$0.86 | 3.6% | -$6.30 | 2.4% | 3.06pt | Degraded from peak |

---

## Run 8 — Mixed Training, lr=5e-6  ✓ BEST ALL-ROUND CHECKPOINT
**Log:** `rl/train_mixed_5e6.log`
**Resumed from:** `rl/checkpoints/mixed_very_low_lr/step_2679831.pt` at step 2,679,831
**Opponent: mixed (50/50)** | lr: 5e-6 | entropy-coef: 0.005 | n-envs: 8 | Device: CUDA

| Metric | Value |
|--------|-------|
| Steps this run | ~1,320,000 (2.68M → 4.0M) |
| Total steps | 4,000,970 |
| Total episodes | 152,752 |
| Starting entropy | 0.165 |
| Peak entropy | 0.356 (step ~3.87M) |
| Final entropy | 0.332 (recovering) |
| Throughput | ~1,300–1,500 sps |
| Save dir | `rl/checkpoints/mixed_5e6/` |

- Entropy peaked at 0.356 then started recovering — first time entropy self-corrected
- Same peak-then-degrade pattern; best checkpoint early in the run

**Evaluation (500 games):**
| Checkpoint | vs Chicken | Win% | vs FF | Win% | Avg win score | Notes |
|------------|-----------|------|-------|------|---------------|-------|
| `step_2731542.pt` (~2.73M) | **-$0.06** | **4.4%** | -$7.02 | **4.4%** | **3.91pt** | ⭐ Best all-round |
| `final.pt` (4.0M) | -$0.76 | 3.4% | -$6.17 | 2.0% | 3.00pt | Degraded |

---

## All-Time Best Checkpoints

| Checkpoint | vs Chicken $/game | Win% | vs FF $/game | Win% | Use for |
|------------|------------------|------|-------------|------|---------|
| `rl/checkpoints/step_1918866.pt` | **+$0.48** | 5.0% | -$8.62 | — | Best RL vs Chicken |
| `rl/checkpoints/mixed_5e6/step_2731542.pt` | -$0.06 | 4.4% | -$7.02 | 4.4% | Best all-round RL |
| `rl/checkpoints/imitation_v1/imitation_final.pt` | +$4.51 | 16.2% | -$1.67 | 13.1% | Imitation v1 |
| `rl/checkpoints/imitation_v2/imitation_final.pt` | +$4.69 | 19.7% | -$0.78 | 16.8% | Best MLP (obs v2) |
| `rl/checkpoints/exit_v3_r2_soft/exit_final.pt` | **+$9.18** | **23.5%** | **+$2.44** | **21.8%** | ⭐⭐⭐ **Best raw network** (CNN, obs v3, ExIt r2) |
| `exit_v3_r2_soft` **+ PairedMCTSPolicy** (K=64) | — | — | **+$3.57** | **25.3%** | ⭐⭐⭐ **Best player overall** |

---

## Summary Table

| Run | Method | Opponent | vs Chicken win% | vs FF win% | Status |
|-----|--------|----------|-----------------|------------|--------|
| 1 | RL scratch | Chicken | — | — | Old format |
| 2 | RL scratch | Chicken | 5.0% / +$0.48 | — | ✓ Best RL vs Chicken |
| 3 | RL fine-tune | FirstFelix | ~3% / -$0.29 | ~3% / -$6.60 | ✗ Forgetting |
| 4 | RL fine-tune | Mixed | 3.4% / -$0.36 | 3.0% / -$6.96 | ✗ Forgetting |
| 5 | RL fine-tune | Mixed | 3.4% / -$0.10 | 2.6% / -$7.02 | ✗ Unstable |
| 6 | RL scratch | FirstFelix | — | 0% / -$7.0 | ✗ No signal |
| S1–S3 | Self-play | Various | 0–0.6% | 0.6–1.0% | ✗ All regressed |
| 7 | RL fine-tune | Mixed | 4.4% / -$0.37 | 3.4% / -$6.55 | ✓ Best RL FF $/game |
| 8 | RL fine-tune | Mixed | 4.4% / -$0.06 | 4.4% / -$7.02 | ✓ Best all-round RL |
| IL-v1 | Imitation (50k, 2L) | — | 16.2% / +$4.51 | 13.1% / -$1.67 | ⭐ Breakthrough |
| IL-v2 | Imitation (100k, 3L) | — | **19.7% / +$4.69** | **16.8% / -$0.78** | ⭐⭐ **Best overall** |

---

## Key Observations

1. **85% draw rate is normal** — Hong Kong Mahjong with minScore=3 means many potential hands can't win; draws are frequent by design.
2. **5% win rate ≈ random** — With 85% draws, only 15% of games have a winner. Agent winning 25% of those = ~3.75% of total games; 5% is slightly above random.
3. **Tenpai reward shaping is active** — +$2 intermediate reward in `RLPlayer.scala` (TENPAI_REWARD=2.0), passed through to Python via `tenpai_reward` field.
4. **Catastrophic forgetting** — FirstFelix's large loss magnitude (~$-36/game when losing) overwhelms Chicken gradients even at 50/50 mix. LR reduction helps but doesn't fully solve it.
5. **Peak-then-degrade pattern** — every resumed training run peaks 50–150k steps in, then degrades. The model is near a local maximum and small updates push it over the edge.
6. **Self-play without MCTS fails in high-draw games** — all three self-play variants (cold-start, warm-start, hybrid) regressed the baseline. The 85% draw rate means too little win signal for self-play to bootstrap.
7. **Very low LR (1e-5 to 5e-6) extends stability** — entropy drift is slower but not eliminated. Halving LR doubles the time before collapse but doesn't prevent it.
8. **Score bonus for high-value wins** — added 5pt/7pt/8pt multipliers to Scala reward in self-play mode. Effect visible: avg win score rose from 3.0pt to 3.9pt in Run 8 early checkpoints.
9. **n-envs 8 works** — VecMahjongEnv with 8 parallel JVMs achieves ~1,300–1,500 sps, roughly 4–5× single-env throughput.

---

## Per-Player Discard Observation (2026-03-22)

Expanded observation dimension from **485 → 587** by replacing the single shared discard counts (34 floats) with per-player discard counts (4 × 34 = 136 floats). This lets the model see *who* discarded *what*, enabling defensive play against specific opponents — the same information FirstFelix uses via `previousPlayerMainSuit(discards)`.

**Changes:**
- `rl/env.py`: `OBS_DIM` 485 → 587, `encode_state` writes per-player discards normalised by 4.0
- `src/…/RLPlayer.scala`: `stateToMap` adds `"discarded_by_player": [[…], …, […]]` (4 × 34 list)
- `rl/model.py`: docstring updated; architecture unchanged

---

## Imitation Learning from FirstFelix (2026-03-22)  ⭐ MAJOR BREAKTHROUGH

**New files:** `rl/imitation_train.py`, `src/…/FFDataServer.scala`

Supervised learning directly from FF's decisions. `RecordingFF` extends FirstFelix, intercepts every decision (`discard`, `win`, `pong`, `kong`, `chow`, `self_kong`, `self_win`), and emits the observation + action pair to Python. Python trains the neural network to predict FF's decisions via cross-entropy loss.

### Imitation v1 — 50k games, 2-layer 512-hidden
**Checkpoint:** `rl/checkpoints/imitation_v1/imitation_final.pt`

| Metric | Value |
|--------|-------|
| Training games | 50,000 |
| Total decisions | ~1.25M |
| Architecture | 2-layer MLP, hidden=512 (OBS_DIM=587) |
| Epochs | 20 |
| Final overall accuracy | **98.4%** |
| Accuracy by type | discard: 100%, chow: 94%, pong: 90%, self_kong: 99%, win: 97% |

**Evaluation (2000 games, deterministic=True):**
| vs | $/game | Win rate | Avg win score | Notes |
|----|--------|----------|---------------|-------|
| FirstFelix | -$1.67 | **13.1%** | 5.49 | **2× FF's ~6.4% individual rate** |
| Chicken | +$4.51 | 16.2% | 5.23 | Strong offensive play |

**Key insight:** Imitation learning bypasses the peak-then-degrade failure of RL fine-tuning. By directly cloning FF's 98.4%-accurate decision-making, the model immediately surpasses all previous RL checkpoints by a large margin.

---

## Post-Imitation Experiments (2026-03-22–23)

All attempts to improve beyond imitation_v1 via RL or MCTS failed:

### RL Fine-tune from imitation_v1  ✗ DEGRADED
- lr=1e-5, opponent=firstfelix, warm-start from imitation_final.pt
- Money went from -0.5 to -4.7 over 600k steps
- Same peak-then-degrade pattern; PPO pushes model away from already-optimal imitation decisions

### AlphaZero MCTS Training (az_v1)  ✗ DEGRADED
- `rl/mcts_train.py`, n_rollouts=10, rollout_opp=firstfelix, τ=1.0
- Started at ema_reward=-4.38, win%=3.1% → degraded to ema_reward=-6.42, win%=1.9% after 1088 games
- Root cause: 10 rollouts per tile too noisy; MCTS Q-estimates worse than model's 98.4% accurate predictions

### MCTS at Inference Time  ✗ DEGRADED
- 20 FF rollouts per candidate discard tile, τ=0.1, 500 games
- Baseline 14.4% win → MCTS inference 9.6% win (−3.84/game)
- Root cause: determinization extremely noisy with 90% hidden tiles; 20 rollouts insufficient to beat well-calibrated imitation decisions

---

## Imitation v2 — 100k games, 3-layer 1024-hidden  ⭐ NEW BEST (2026-03-24)

**Checkpoint:** `rl/checkpoints/imitation_v2/imitation_final.pt`

Scaled up imitation learning: 2× data, deeper/wider model, more epochs.

| Metric | Value |
|--------|-------|
| Training games | 100,000 |
| Total decisions | ~2.5M |
| Architecture | **3-layer MLP, hidden=1024→512→256** (1.28M params vs 453K) |
| Epochs | 30 |
| Final overall accuracy | **99.5%** (+1.1pp vs v1) |
| Accuracy by type | discard: 100%, chow: **99%**, pong: **97%**, self_kong: 100%, win: 98% |

Key accuracy gains vs v1: chow 94%→99%, pong 90%→97%. Checkpoints saved every 5 epochs.

**Evaluation (2000 games, deterministic=True):**
| vs | $/game | Win rate | Avg win score | vs v1 delta |
|----|--------|----------|---------------|------------|
| FirstFelix | -$0.78 | **16.8%** | 35.93 | +3.7pp |
| Chicken | +$4.69 | **19.7%** | 29.81 | +3.5pp |

**imitation_v2 is now 2.6× FF's individual win rate (~6.4%).**

---

## MCTS Implementation (2026-03-21)

Implemented imperfect-information MCTS (determinization-based / PIMC) for the discard decision.

**New files:**
- `src/main/scala-2.12/.../rl/MCTSRolloutServer.scala` — Standalone Scala server that accepts game-state snapshots, determinizes hidden info (opponent hands + draw pile), runs N complete rollouts with heuristic players, returns mean reward per candidate discard tile.
- `rl/mcts.py` — Python client (`MCTSRolloutClient`) + `ImperfectInfoMCTS` class. Uses MCTS for discard decisions; NN for binary decisions (win/pong/kong/chow).
- `rl/mcts_train.py` — AlphaZero-style training: MCTS generates policy targets π_mcts, NN trained with cross-entropy(π_mcts, π_net) for discard + MSE(v_net, R) for all steps.
- `rl/test_mcts.py` — Smoke test (5 tests, all passing).

**Test results (n_rollouts=20):**
- Rollout latency: ~0.38s per tile evaluation
- Q-value range: tile 1 (B2)=-6.0 vs tile 3 (D4)=-0.2 (MCTS discriminates correctly)
- Full game with MCTS: 25 discard decisions evaluated in ~2.1s

**Training parameters for MCTS runs:**
- `--n-rollouts 8` (fast) to `--n-rollouts 20` (quality)
- `--rollout-opp chicken` for speed; `--rollout-opp firstfelix` for stronger estimates
- `--temperature 1.0` for training; `0.1` for evaluation (more greedy)
- Start from `rl/checkpoints/mixed_5e6/step_2731542.pt` (best all-round checkpoint)
- Use `--opponent firstfelix` for actual training games

**Example command:**
```bash
python rl/mcts_train.py \
  --jar target/scala-2.12/mahjong-assembly-0.1.0.jar \
  --checkpoint rl/checkpoints/mixed_5e6/step_2731542.pt \
  --device cuda \
  --n-rollouts 8 \
  --rollout-opp chicken \
  --opponent firstfelix \
  --total-games 200_000 \
  --games-per-update 64 \
  --save-dir rl/checkpoints/mcts_v1
```

---

## Strategy Analysis (2026-03-21)

Analysis of `step_2731542.pt` (best all-round checkpoint), 1000 deterministic games vs Chicken.
Full output: `rl/strategy_analysis_best.log`

**What the AI independently discovered:**

1. **Never chow** — 100% pass rate across 1,631 chow opportunities. Chowing reveals hand structure to opponents, scores no bonus, and locks you into specific tile dependencies.
2. **Pong everything** — 100% accept rate for pong (n=1,828) and kong (n=137). Pong reduces hand size and unlocks All-Pong bonus.
3. **Always take wins** — 100% accept rate for win and self-win. A guaranteed 3pt win beats a speculative 7pt hand.
4. **Two-phase discard strategy:**
   - *Early game (>60 tiles):* commit to ONE suit, shed everything else aggressively (Red Dragon 8.3%, Chr 3/4 ~7.9%, Bam 2/7 ~7.8% of all discards)
   - *Late game (<30 tiles):* dump isolated terminals and honour singles (Dot 9 5.8%, Bam 1 5.8%, Bam/Dot 8 ~5.5%)
5. **Red Dragon: discard early if singleton, keep if paired** — 8.3% early discard rate drops to 2.6% late.
6. **Green Dragon & N Wind: almost never discarded** — treated as high-value pong anchors.

**The AI converged to All-Pong / Single-Suit hybrid strategy** — pick one suit by turn 3, pong everything, never chow, shed the other suits, take any win. This is a well-known strong strategy in Hong Kong Mahjong; the model discovered it from experience alone.

**Tips for human players:**
| Old habit | Change to |
|-----------|-----------|
| Chowing to speed up hand | Never chow — leaks info, no bonus |
| Holding tiles across 2–3 suits | Commit to one suit by turn 3–4 |
| Passing pongs to keep sequences | Pong everything |
| Waiting for a better hand to win | Take every win, even 3pt minimum |
| Holding 1s/9s hoping for sequences | Dump terminals late game |
| Always discarding dragons | Keep dragons once you have a pair |

---

## Paired-Worlds MCTS — First Player to Beat FirstFelix (2026-07-03/04)  ⭐⭐ BREAKTHROUGH

Goal: play *much better* than FirstFelix (the strongest heuristic). imitation_v2
was FF-parity (-$0.78/game vs a 3×FF table). Three statistical fixes to the MCTS
rollout estimator turned search into a genuine improvement operator.

### Why previous MCTS failed, and the fixes
1. **No common random numbers** — each candidate discard was evaluated on
   *different* random determinizations, so Q-differences at 20 rollouts were
   noise. Fix: `evaluate_batch` in `MCTSRolloutServer.scala` evaluates ALL
   candidates on the SAME K sampled worlds (paired evaluation).
2. **Chicken self-policy in rollouts** — rollouts assumed we play badly after
   the discard. Fix: our seat plays FirstFelix in rollouts.
3. **No statistical guard** — MCTS overrode a 99.5%-accurate policy on noise.
   Fix: `PairedMCTSPolicy` (rl/mcts.py) prunes to top-k=6 candidates by the
   imitation policy, computes PAIRED per-world Q-differences vs the imitation
   choice, and only overrides when mean gain > 1.5·SE and > $0.25.

### Gate evaluation (500 paired-seed games vs 3×FirstFelix, `eval_paired_mcts.py`)
| Policy | $/game | Win% | Draw% |
|--------|--------|------|-------|
| imitation_v2 (no search) | -$0.78 | 16.8% | 64.6% |
| imitation_v2 + PairedMCTS (K=32) | **+$1.59** | **24.6%** | 57.8% |

Paired delta +$2.37 ± 1.27. MCTS overrides 10.4% of discard decisions.
Confirmed at scale during data generation:
- 30,000 games @ K=32: **+$1.31/game, 22.8% win**
- 20,000 games @ K=64: **+$2.17/game, 24.2% win** (more worlds → better switches)

**First player ever to make positive money vs a FirstFelix table** (~3.8× FF's
per-seat win rate). Latency ~2.5s/game (top-6 candidates × 32-64 worlds × ~6ms
FF rollouts) — fully playable.

### Expert Iteration distillation: 3 attempts, all failed to absorb the gains
Tried to bake the search advantage into the raw network (30k+20k games of
MCTS-improved decisions, warm-start from imitation_v2):

| Round | Recipe | Switched-label acc | Eval vs FF | Verdict |
|-------|--------|--------------------|------------|---------|
| v1 hard | one-hot labels, switch-weight 3 (`exit_distill.py`) | 13.9% | -$0.81 / 17.3% | flat |
| v2 soft | Boltzmann(ΔQ/τ) targets, SE-scaled τ (`exit_distill_soft.py`) | 23.7% agree | -$1.49 / 15.2% | **worse** (entropy injection) |
| v3 confident | hard labels only where ΔQ > max(2·SE, $1) (`exit_distill_confident.py`) | 16.9% | -$1.04 / 17.0% | flat |

Even confidently-better overrides (mean gain $7.18) are only ~17% learnable.
**Conclusion:** the search advantage is not extractable from the 587-dim obs by
this MLP — MCTS computes explicit EV over determinized futures (deep
tile-counting), which has no shortcut in feature space. Same reason AlphaZero's
raw policy net is far weaker than net+search.

### How to use the best player
```python
from model import MahjongAgent
from mcts import MCTSRolloutClient, PairedMCTSPolicy

agent  = MahjongAgent.load("rl/checkpoints/imitation_v2/imitation_final.pt", device="cpu")
client = MCTSRolloutClient("target/scala-2.12/mahjong-assembly-0.1.0.jar",
                           rollout_opp="firstfelix")
policy = PairedMCTSPolicy(net=agent.net, rollout_client=client,
                          n_worlds=64, top_k=6, z_threshold=1.5, min_gain=0.25)
action, info = policy.get_action(obs, state, decision, context, action_mask)
```

New files: `rl/eval_paired_mcts.py` (paired gate eval), `rl/eval_parallel.py`
(fast parallel checkpoint eval), `rl/exit_datagen.py` (ExIt data gen),
`rl/exit_distill*.py` (3 distillation recipes), data in `rl/data/exit_v1|v2/`.

### What the MCTS overrides actually do (future analysis)
~54k recorded overrides with ΔQ values in `rl/data/exit_v2/` shards
(`discard_cands`/`discard_mean_d`/`discard_se_d`) — mining these for
human-interpretable insights (e.g. defensive folds vs committed opponents,
EV-based suit abandonment timing) is a promising follow-up.

---

## Obs v3 + CNN + Expert Iteration — Raw Network Beats FirstFelix (2026-07-04)  ⭐⭐⭐ GOAL ACHIEVED

Full "go big" arc: Scala shanten features + tile-axis CNN + iterated ExIt.
The raw network (no search at play time) now decisively beats the FirstFelix table.

### Ingredient 1 — Shanten/ukeire features (Scala)
`Shanten.scala`: standard-hand shanten DP (win=-1, tenpai=0, max 8) with fixed-group
support + unseen-weighted ukeire; 18 unit tests (`ShantenTest`); ~1.6ms per full
14-tile discard feature block. `RLFeatures.featureMap` merged into the state maps
of both `RLPlayer` (live gym) and `FFDataServer` (imitation data).

### Ingredient 2 — Obs v3 (759 dims, `encode_state(state, version=3, context=…)`)
v2 (587) + 5 new planes + 2 scalars:
- `shanten_after[34]`, `ukeire_after[34]` — hand quality per candidate discard
- `improve_tiles[34]` — which draws improve a 13-tile hand (unseen-weighted)
- `unseen[34]` — tile counting for defence
- `context_tile[34]` — **the offered/drawn tile for win/pong/kong/chow decisions.
  v2 never included this: the MLP always decided melds blind.**
- scalars: current shanten, is-discard-state

### Ingredient 3 — MahjongConvNet (`--arch conv`, 675k params)
Residual 1D CNN over the 34-tile axis (21 tile-planes + 11 broadcast scalars →
32 ch → 6 res blocks @128). Per-tile 1×1-conv discard head (spatially aligned),
pooled heads otherwise. **Split binary heads** (win/self_win/pong/kong) — with a
shared head, kong's partially-unlearnable labels corrupt always-accept win
behaviour (this and the two fixes above were found via 3 failed iterations
v3a → v3b → v3c; final architecture = v3d).

### Imitation v3d (base clone)
100k FF games (parallel FFDataServer collection, rare-decision oversampling ≤25×),
16 epochs. 99.8% accuracy — every head ≥99% incl. kong 100%.
**`rl/checkpoints/imitation_v3d/imitation_epoch10.pt`: -$0.20/game, 13.2% win,
48.1 avg win pay** (vs imitation_v2: -$0.78 / 16.8% / 35.9) — near-FF-parity,
truer FF style (fewer, bigger wins).

### Expert Iteration rounds (20k games each, K=64 paired worlds, z=1.5)
Distillation recipe: **soft Boltzmann(ΔQ/τ) targets with SE-scaled τ**
(`exit_distill_soft.py`) — failed on the MLP, works on the CNN; confident-hard
(`exit_distill_confident.py`) also transfers but less (+0.94 vs +1.88 after r1).

| Round | Generator (net+MCTS) money/win% | Distilled raw net | Raw net eval vs 3×FF |
|-------|--------------------------------|-------------------|----------------------|
| base  | — | imitation_v3d_e10 | -$0.20 / 13.2% |
| 1 | +$3.29 / 23.0% | `exit_v3_soft/exit_final.pt` | **+$1.88 / 19.9%** (paired Δ +1.14±0.36 conf-hard; soft best) |
| 2 | +$3.57 / 25.3% (switch rate 12.2%→11.2%) | `exit_v3_r2_soft/exit_final.pt` | **+$2.44–2.62 / ~21%** (pooled Δ vs r1 +0.67±0.44, 4k paired games) |
| 3 | (overnight run — see below) | | |

**vs Chicken:** exit_v3_r2_soft = +$9.18/game, 23.5% win (v2 was +$4.69/19.7%).

### Why distillation works now but failed on the MLP (2026-07-03)
The MCTS advantage is EV over hand progressions — shanten/ukeire math. The MLP
had to derive it from raw counts (impossible in 3 dense layers); the CNN reads it
off the feature planes and only has to learn *how to weigh* it. Same data recipe,
same search — the observation/architecture was the entire bottleneck.

### Best players (2026-07-04)
| Player | vs 3×FF | Notes |
|--------|---------|-------|
| `rl/checkpoints/exit_v3_r2_soft/exit_final.pt` (raw net) | **+$2.44/game, 21.8% win** | ⭐⭐⭐ best raw network |
| same net + PairedMCTSPolicy (K=64) | **+$3.57/game, 25.3% win** | ⭐⭐⭐ best player overall, ~5s/game |

### ExIt round 3 + convergence (2026-07-05 early)
- Generator (r2 net + MCTS): +$3.65/game, 25.7% win, switch rate 10.8% (r1: 12.2% → r2: 11.2%)
- Distilled `rl/checkpoints/exit_v3_r3_soft/exit_final.pt`: **+$2.25/game, 20.9% win**
  vs 3×FF (4000 games); paired Δ vs r2 = +0.25 ± 0.40 — within noise.
- vs Chicken: **+$9.40/game, 25.8% win**.
- **Loop converged:** per-round raw-net gains +2.08 → +0.67 → +0.25(ns). The net
  has absorbed what K=64 rollout search can teach at this scale. Further gains
  likely need deeper search (more worlds / NN-guided rollouts) or RL on top.

### Overfitting check (2026-07-04 night)
ExIt trains only vs FirstFelix opponents, but gains transfer fully to Chicken —
both columns improve monotonically through the rounds (v2 → v3d → r1 → r2:
Chicken +$4.69 → +$6.60 → +$8.47 → +$9.18; FF -$0.78 → -$0.20 → +$1.88 → +$2.44).
No FF-specific exploitation; the model learned general play quality.

### FINAL BEST (as of 2026-07-05)
| Player | vs 3×FF | vs 3×Chicken |
|--------|---------|--------------|
| **Raw net:** `rl/checkpoints/exit_v3_r3_soft/exit_final.pt` | **+$2.25 / 20.9%** | **+$9.40 / 25.8%** |
| **Net + search:** same + `PairedMCTSPolicy(n_worlds=64)` | **+$3.65 / 25.7%** | — |

---

## AlphaZero-Style Program: Self-Play RL + NN-Guided Search (2026-07-05)

Goal shift: from "beat FirstFelix" to general strength. Three stages; user
approved the full program.

### Stage A — KL-anchored PPO self-play fine-tune (`rl/ppo_selfplay.py`)
60k games from exit_v3_r3_soft: 12 true-self-play tables (all 4 seats share
the evolving net, zero-sum rewards — score-bonus shaping now behind
`-Drl.scorebonus`, default off) + 6 FF tables, batched forwards, KL(new‖anchor)
penalty (0.3), value-head-only warmup (2k games), reward /25, γ=1.
Fixed a NaN: -inf masked logits on both sides of the anchor KL make
(-inf)−(-inf)=NaN and torch.where leaks NaN grads — re-mask with -30.0.

**Result: `ppo_a/ppo_final.pt` ≈ r3 everywhere.** League head-to-head
(new `rl/league_eval.py`, 1000 paired 1v3 games both directions):
+0.86±1.13 n.s. vs FF: -0.16±0.54 n.s. vs Chicken: +0.35±0.51 n.s.
No regression, no breakthrough — teacher ceiling is the binding constraint.

### Stage B — NN-guided rollouts (the real AlphaZero move)
1. **Fast student policy** (`rl/distill_student.py`): 48ch×2-block conv
   (260KB) distilled from r3 on 2.7M states; 60.7% discard / 94-100% meld
   agreement. ONNX 0.084ms/call (teacher: 4ms). `rl/export_onnx.py` exports
   all heads + parity check.
2. **Scala**: `NNRollout.scala` — `V3Obs` (exact port of encode_state v3;
   parity test `rl/test_v3obs.py`: 0.00e+00 error on 495 real decisions),
   `OnnxPolicyService` (batched ORT via LinkedBlockingQueue + single
   inference thread), `NNPlayer` (greedy masked argmax). MCTSRolloutServer:
   `self_policy`/`rollout_opp` ∈ {firstfelix, chicken, nn}, parallel worlds
   (`-Drl.rolloutThreads`), rollout GameState now seeded with the REAL
   discard history (`discards_by_player`, relative seat order) — heuristic
   rollouts ignored it, NN policies need it. Build needs JDK 11
   (JAVA_HOME=/usr/lib/jvm/java-11-openjdk-amd64 sbt assembly); onnxruntime
   1.17.1 added to build.sbt.
3. **Rollout timing** (K=64, 6 cands, steady state): FF/FF 1thr 359ms →
   8thr 87ms (threading alone = 4× win). NN-self/FF-opp 8thr ≈ 1.3s,
   32thr ≈ 0.95s. NN/NN ≈ 8s (too slow for bulk datagen).
4. **A/B gate** (`rl/eval_mcts_ab.py`, 400 paired games, base=r3, K=64):
   search w/ NN-self rollouts +$6.15/game vs FF-self +$4.75 —
   **paired delta +1.40 ± 1.76** (n.s. but positive at every checkpoint;
   best generator measured to date).

### Stage B ExIt round 1 (self-play) — RUNNING overnight
`rl/exit_datagen2.py`: self-play tables (seat0 = r3+search, seats1-3 = raw
r3), rollouts self=nn(student48) opp=firstfelix, K=64 top6 z1.5.
6 workers × 12k games total → rl/data/exit_sp1. Distill with
exit_distill_soft.py when enough shards exist; gate with league_eval vs r3
AND eval_parallel vs FF/Chicken.

### Insight extraction (2026-07-04, separate thread)
136,811 search overrides mined ($4.77 mean). Simple-rule capture vs recorded
per-candidate Q-gains: min-shanten/max-ukeire greedy −$0.68/decision (worse
than instinct); always-take-tenpai −$0.68/fire; only "fix dead tenpai wait"
is positive (+$4.68/fire, 0.4% of decisions). Strategy guide artifact
published. Scripts in session scratchpad.

### Self-play ExIt round 1 RESULT — first improvement over r3 (2026-07-06)
Full 12k self-play games (6 workers × 2k, teacher = r3 + NN-guided search,
self-rollout=student48, opp-rollout=firstfelix, K=64). Teacher held
+$5.4/game vs the raw-r3 table across all 12k games, override rate steady
15.7% (vs FF-based loop's declining 12→11→10.8%). ~410k Q-labeled decisions.

Distilled `exit_sp1b_soft/exit_final.pt` (soft targets, 6 epochs, warm-start
r3, switched-agree 28.9%). **Gates vs r3 — all three positive:**
| Gate | candidate B | r3 | paired delta |
|------|-------------|-----|--------------|
| League head-to-head (2000×2) | — | — | **+$1.13 ± 0.75** |
| vs 3×FF (3000) | +$2.13 / 21.0% / $41.1 pay | +$1.41 / 19.7% / $37.8 | +$0.72 ± 0.62 |
| vs 3×Chicken (3000) | +$10.10 / 24.8% / $44.9 | +$9.51 / 25.7% / $40.7 | +$0.59 ± 0.57 |

Three independent conditions, inverse-variance pooled ≈ **+$0.76 ± 0.37 (~2σ)**.
Same bigger-wins signature (avg pay up in every column). **PROMOTED as new
base** → `rl/checkpoints/best_raw_net.pt`. Stage A PPO was flat; this
NN-guided self-play round is the program's first real gain over r3.

NEW BEST (2026-07-06):
| Player | vs 3×FF | vs 3×Chicken |
|--------|---------|--------------|
| `exit_sp1b_soft/exit_final.pt` (= best_raw_net.pt) | +$2.13 / 21.0% | +$10.10 / 24.8% |

Next: fresh student distilled from sp1b (student48_sp1b) → export ONNX →
self-play ExIt round 2 (teacher = sp1b + NN search). Iterate until flat.

### Self-play ExIt round 2 — CONVERGED (do not promote; 2026-07-07)
Full 12k self-play games from sp1b base (teacher held +$4.6/game vs raw-sp1b,
override rate 14.0%, down from round-1's 15.7%). Distilled
`exit_sp2c_soft/exit_final.pt` (switched-agree 14.6%, half of sp1b's 28.9% —
first sign the teacher's overrides are getting unlearnable on the stronger base).

**Gates vs sp1b — INCOHERENT (specialization, not general strength):**
| Reference | candidate C vs sp1b |
|-----------|---------------------|
| head-to-head league (2000×2) | **+$1.28 ± 0.81** (C wins) |
| vs 3×FF anchor (3000) | +$0.24 ± 0.59 (flat) |
| vs 3×Chicken anchor (3000) | −$0.64 ± 0.53 (worse) |
| vs r3 anchor, transitive (2000×2) | C beats r3 +$0.80 vs sp1b's +$1.13 → **worse** |

C matchup-beats its own training partner but is flat/worse on ALL THREE fixed
anchors (FF, Chicken, r3). Round 1 (sp1b) was coherent across all conditions
(+1.13/+0.72/+0.59); round 2 is not. Classic self-play cycling/style-overfit
(issue #13 lever 6). **sp1b remains champion (best_raw_net.pt unchanged).**

**Self-play ExIt distillation loop CONVERGED.** One genuine universal gain
(r3 → sp1b, ~2σ) then specialization. Next: pivot to deeper search levers
(issue #13) that raise the teacher's GENERAL strength — belief-state
determinization + IS-MCTS — rather than distillation chasing self-play
matchups. A cheaper round-3 variant worth trying first: mixed-opponent
datagen (seats 1-3 sampled from {sp1b, r3, FF, Chicken, past ckpts}) to keep
the improvement operator honest against a population instead of one partner.

### FINAL BEST (as of 2026-07-07) — unchanged from round 1
| Player | vs 3×FF | vs 3×Chicken |
|--------|---------|--------------|
| **`best_raw_net.pt` = `exit_sp1b_soft/exit_final.pt`** | **+$2.13 / 21.0%** | **+$10.10 / 24.8%** |

### Mixed-opponent ExIt round — BREAKS THE PLATEAU (2026-07-09)  ⭐
Hypothesis (from round-2 specialization): the loop plateaued because datagen
seats 1-3 were all copies of the champion, so distillation learned to beat one
partner, not to improve in general. Fix (exit_datagen3.py): seat 0 = champion
(sp1b) + NN-guided search, but each game's opponents drawn from a POPULATION —
55% mixed self-play (seats 1-3 each sampled from {sp1b, r3, r2, imitation}),
30% FirstFelix tables, 15% Chicken tables.

12k games (6 workers) then, after an OOM pause, resumed leaner (3 workers,
fresh seeds) — ~8,700 combined games, 87 shards (exit_mix1 + exit_mix1_r2).
Distilled `exit_mix_soft/exit_final.pt` (warm-start sp1b, soft-Q, 6 epochs).

**Gates vs sp1b — ALL THREE POSITIVE (universal gain, unlike round 2):**
| Gate | delta (D − sp1b) |
|------|------------------|
| head-to-head league (2000×2) | **+$2.01 ± 0.81** (~2.5σ) |
| vs 3×FF (3000) | +$0.68 ± 0.65 (win 20.9→23.2%) |
| vs 3×Chicken (3000) | +$0.52 ± 0.52 (win 24.7→26.5%) |

Contrast round 2 (same-partner): league +1.28 but FF +0.24 / Chicken −0.64.
Opponent diversity converted a specializing loop back into a generalizing one.
**PROMOTED as new champion** → `rl/checkpoints/best_raw_net.pt`.

NEW BEST (2026-07-09):
| Player | vs 3×FF | vs 3×Chicken |
|--------|---------|--------------|
| **`exit_mix_soft/exit_final.pt` (= best_raw_net.pt)** | **+$3.38 / 23.2%** | **+$10.62 / 26.5%** |

Next: another mixed-opponent round with the new champion added to the pool
(keep diversifying), or pivot to deeper search (issue #13 belief-state
determinization). Add an Elo ladder now that head-to-head is the live metric.

**CAVEAT (transitivity check, same day):** D vs r3 head-to-head = −$0.29 ± 0.90
(EVEN), despite sp1b>r3 (+1.13) and D>sp1b (+2.01). Intransitive — head-to-head
is matchup-dependent here (D's pool/search centered on sp1b, giving an
anti-sp1b edge). D's promotion stands on the FIXED anchors (FF +0.68, Chicken
+0.52, both positive) but the gain is MODEST, not the +2.01 league number.
Two head-to-head misleads now (round-2 false positive, this r3 anomaly) →
build an ELO LADDER (round-robin over {r3,sp1b,D,imitation}+FF+Chicken) BEFORE
the next round; stop trusting single pairwise duels for the promote decision.

### Elo ladder (#15) — post-r3 nets are a TIE and an intransitive cycle (2026-07-09)
Built rl/elo_ladder.py: paired round-robin, money-scale least-squares rating,
bootstrap CIs, explicit intransitivity residual. Over {r3, sp1b, D, imit},
2000 games/pair:
- Ratings: D +0.52 / r3 +0.08 / sp1b -0.20 / imit -0.39 — ALL CIs overlap (no
  net separated at 95%).
- **Intransitivity $0.65/game** (≈ the whole rating spread): cycle
  sp1b>r3 (+0.53), D>sp1b (+1.68), r3>D (+0.56).
- Anchor calibration (common seeds): vs FF r3 best / D worst; vs Chicken D best
  / sp1b worst — all within ~$0.5.
**Conclusion:** the r3→sp1b→D "climb" was pairwise-gate noise; on fixed
benchmarks and by ladder rating the three are peers. Distillation-of-search has
saturated. The ladder (rl/checkpoints/elo.md) is now the promotion gate. Next
real lever = stronger teacher (belief-state determinization + IS-MCTS, #13),
not more distill rounds.
