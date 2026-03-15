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

## Summary Table

| Run | Opponent | LR | Steps | vs Chicken $/game | vs FF $/game | Status |
|-----|----------|----|-------|-------------------|-------------|--------|
| 1 | Chicken | 3e-4 | 500k | N/A | N/A | Old format, no tenpai |
| 2 | Chicken | 3e-4 | 2M | **+$0.48** | -$8.62 | ✓ Best baseline |
| 3 | FirstFelix | 3e-4 | 2M→4M | -$0.29 | -$6.60 | ✗ Catastrophic forgetting |
| 4 | Mixed | 3e-4 | 1.9M→4M | -$0.36 | -$6.96 | ✗ Still forgetting |
| 5 | Mixed | 3e-5 | 1.9M→4M | -$0.10 (step 2.5M) | -$7.02 | ✗ Unstable late |

**Current best checkpoint: `rl/checkpoints/step_1918866.pt`** (Run 2, Chicken-only, +$0.48/game)

---

## Key Observations

1. **85% draw rate is normal** — Hong Kong Mahjong with minScore=3 means many potential hands can't win; draws are frequent by design.
2. **5% win rate ≈ random** — With 85% draws, only 15% of games have a winner. Agent winning 25% of those = ~3.75% of total games; 5% is slightly above random.
3. **Tenpai reward shaping is active** — +$2 intermediate reward in `RLPlayer.scala` (TENPAI_REWARD=2.0), passed through to Python via `tenpai_reward` field.
4. **Catastrophic forgetting** — FirstFelix's large loss magnitude (~$-36/game when losing) overwhelms Chicken gradients even at 50/50 mix. LR reduction helps but doesn't fully solve it.
5. **GPU utilisation** — All runs used `--n-envs 1` (single Scala server). VecMahjongEnv supports `--n-envs N`; future runs should use `--n-envs 4` for ~3-4× throughput.
