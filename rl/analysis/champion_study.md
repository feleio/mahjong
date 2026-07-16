# How the Champion Plays — mahjong lessons from a self-taught machine

*(2026-07-11 · sources: `strategy_analysis.py` 200 games vs 3×FirstFelix,
`mine_overrides.py` on 27 exit_mix1 shards · rendered version: the
"How the Champion Plays" artifact, English + Cantonese)*

We trained a network that beats every heuristic at Hong Kong mahjong, then
made it explain itself: 200 instrumented games, and 7,771 places where an
even stronger search corrected it — each correction priced in dollars.

## The player

The champion (`best_raw_net.pt` = `exit_sp1b_soft`) is a residual CNN trained
by **expert iteration**: play, let a paired Monte-Carlo search evaluate every
candidate discard across hundreds of determinized deals, retrain toward what
the search preferred, repeat. RL fine-tuning, deeper tree search, and other
fashionable ideas were all tried and all measurably failed — every real gain
came from this loop.

The search teacher still overrides the champion on **14% of discards**, and
paired evaluation prices each override. Those overrides are the syllabus.

| stat (this study's 200 games) | value |
|---|---|
| money | +$5.50/game |
| win rate | 27.5% |
| deal-in rate | 21.5% |
| priced corrections mined | 7,771 |

## Lesson 1 — Take the win. Every time.

55 win opportunities in 200 games (ron + tsumo); the champion accepted
**all 55**. A small win now also denies three opponents the rest of the hand.
Refusing a made minimum hand to fish for a bigger one is the most common
human leak.

## Lesson 2 — Hold honors early, shed them late

Honors are among the champion's *rarest* early discards and *most common*
late ones; terminals are the opposite:

| tile | early share | late share |
|---|---|---|
| Red Dragon | 1.0% | 4.3% |
| East Wind | 2.9% | 4.4% |
| Bamboo 1 | 5.1% | 2.8% |

HK-specific logic: with a 3-faan minimum, a paired honor is the cheapest
route to a payable hand, so a lone honor early has *option value* — hold it
while the hand takes shape. Late, an unpaired honor is dead weight and (with
copies visible) usually safe to release. Terminals carry no such option.

## Lesson 3 — Never wait on a tile that's already gone

The champion's own most expensive habit, corrected repeatedly by the search:
technically-tenpai hands whose winning tile has **zero live copies**.

Example (+$41 claimed, 7 tiles in wall): hand `1B 2B 2B 3B 7B 8B 8B 8B` —
net discards 7B keeping a dead wait; search discards 2B, same shanten,
3 live outs. **35% of all corrections** involved dead or nearly-dead tiles.

Rule: count your wait in the pond and melds before settling on it. A tenpai
waiting on the fourth copy of a tile you can see three of is a pose, not a
hand.

## Lesson 4 — A bad tenpai is worth less than a good one-away

The largest corrections ($60–90 claimed) are the search *breaking tenpai on
purpose*. Example (+$87, wall 29): concealed `3D 3D S S Wh` — net clings to
a 1-out wait (discards Wh); search discards 3D, stepping back to one-away
with 12 tiles of acceptance.

Rule: value a hand by *live outs × payability*, not shanten count; re-check
late — a wait that was fine ten discards ago may have died since.

## Lesson 5 — Claims are options, not obligations

The champion accepts 100% of wins but passes 24% of pongs, 36% of kongs,
33% of chows. Ask what you will discard *after* the claim before calling.

## The syllabus, priced (search-over-champion corrections)

| correction type | share | mean $ | teaches |
|---|---|---|---|
| shape — same speed, subtle re-waiting | 44.2% | $4.87 | which equal-looking discard leaves the better wait |
| tempo — a strictly faster discard existed | 20.9% | $5.21 | the fastest route is often not the obvious one |
| **safety — pays speed for defense/value** | 17.7% | **$5.80** | folding and redirecting is where the money is |
| acceptance — same speed, more live outs | 17.2% | $4.74 | count outs, not patterns |

Also: in 12.4% of corrections the net wanted to throw a *middle* tile and
the search threw an *honor* instead — safety at zero shape cost.

## Caveats

- Per-example $ figures from the override mining are argmax-selected →
  upward-biased (winner's curse). Trust orderings and group means.
- Apparent suit preferences in the discard tables are sampling noise
  (~130 obs/tile) and deliberately not taught.
- The model's weakest skill — reading opponents — is the current training
  target (issue #21, danger heads). When it lands: lesson 6, when to fold.
