# How the champion beats FirstFelix

*(2026-07-11, `rl/compare_vs_ff.py`, 120 games, champion = `best_raw_net.pt` = `exit_sp1b_soft`)*

## Method

The champion (seat 0, greedy) plays 120 games vs 3×FirstFelix. At **every**
discard decision we also ask what FirstFelix would play from the exact same
seat (`ff_decide`: a fresh FF re-decides its target from this hand + discard
history, then picks its discard). When the two disagree, both tiles are
evaluated on the **same 32 determinized worlds** with NN-guided rollouts
(paired CRN), giving an **unbiased** per-decision dollar difference — unlike
the `mine_overrides` numbers, nothing here is argmax-selected, so the means
are trustworthy, not winner's-cursed.

## Headline numbers

| metric | value |
|---|---|
| money | **+$1.57/game** (120 games; long-run reference +$2.13) |
| win / deal-in | 20.8% / 11.7% |
| discard decisions | 2,443 |
| **agreement with FF** | **40.9%** |
| priced disagreements | 1,443 |
| edge per disagreement | **+$1.32 ± 0.21** (6.4σ) |
| summed one-step edge | +$15.83/game (see caveats — this does *not* accumulate to net game money) |

The champion is not "FirstFelix plus polish": they pick **different tiles on
59% of discards**, and when they differ the champion's tile is worth +$1.32
on identical worlds.

## The surprise: the edge is judgment, not speed

Disagreements bucketed by what the champion's tile did *relative to FF's
tile* (shanten/ukeire computed from the obs feature planes):

| bucket | meaning | share | mean $ |
|---|---|---|---|
| tempo  | champion's tile leaves strictly **better** shanten | 19.3% | **+$0.33** |
| accept | same shanten, materially more ukeire | 20.9% | +$0.73 |
| shape  | same shanten, similar ukeire — wait quality / safety | 43.7% | +$1.27 |
| slower | champion's tile leaves **worse** shanten — deliberate slowdown | 16.0% | **+$3.40 ± 0.96** |

Read that bottom row again: the champion's single most valuable behaviour
against FirstFelix is **choosing to be slower**. FF races its turn-one target
unconditionally; the champion knows when the race is wrong — when the pair is
worth more than the tempo, when the payable hand matters more than the fast
one. Raw speed (`tempo`) is its *smallest* per-decision edge: FF's rigid
flush/all-pong targeting is actually competitive on pace.

## The edge triples in the endgame

| phase | n | mean $ per disagreement |
|---|---|---|
| early (wall > 55) | 663 | +$0.87 |
| mid | 461 | +$1.07 |
| late (wall ≤ 30) | 319 | **+$2.60** |

Late-game states are where FF's frozen target and total absence of
re-evaluation cost the most — and where the champion's counting (live outs,
payability) pays the most.

## FirstFelix's failure modes, in its own code

FirstFelix (`player/FirstFelix.scala`) commits on its first discard to a
target — flush in its longest suit (>5 tiles), or all-pong with ≥3 pairs —
choosing the flush suit *while avoiding the previous player's apparent main
suit*. It then discards off-target tiles unconditionally and never
reconsiders.

The costliest expression of this found in the data: with a hand like
`2B 2B 8B 8B + one lone tile`, FF's suit-avoidance rule can pick a target
suit it barely holds — and then it **discards from its own pairs** to chase
it. Three consecutive decisions in one endgame priced at **+$117 to +$120**
each: the champion throws the lone tile and keeps both pairs; FF breaks a
pair. The other recurring pattern is wait selection between near-equal
shapes (`1D 2D`: FF throws the terminal, champion keeps the better wait —
+$40 class).

Class-shift table (FF's choice → champion's choice) confirms two systematic
swaps:

- **safety swap** (20.6% of disagreements): FF throws a *middle* tile, the
  champion throws a terminal/honor instead (+$0.75–1.35).
- **pair/shape respect** (17.4%): FF throws an edge/terminal *from a working
  shape*, the champion throws a middle tile and keeps the shape
  (+$1.94–3.26).

## What a human should take from this

1. **Don't fixate.** FF loses mostly because it never revisits its turn-one
   plan. Re-evaluate the target every few draws.
2. **Speed is overrated.** Against a fast-but-rigid opponent, the champion
   wins on decision *quality* — the willingness to take a slower, better
   hand is worth 10× its pure-speed edge per decision.
3. **Never break a pair to chase a suit** you don't substantially hold.
4. **The endgame is where skill concentrates** — the per-decision stakes
   triple after the wall passes ~30 tiles.

## Caveats

- `ff_decide` uses a **fresh** FF that re-decides its target at the queried
  state; a real in-game FF carries the target it chose on turn one. The
  comparison prices FF's *decision function*, including its target rule —
  which is exactly what produced the +$118 pair-breaking rows.
- The +$15.83/game "summed one-step edge" is a density of local decision
  quality, not net game value: after one better discard the two trajectories
  diverge, so one-step gains don't accumulate (measured net: +$1.57–2.13).
- 120-game money has SE ≈ $3.6/game; the per-decision paired estimates are
  the precise numbers here, the game money is the noisy one.
- Raw rows: `rl/analysis/champion_vs_ff_raw.json`.
