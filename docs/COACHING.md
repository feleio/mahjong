# Coaching Suite — teaching humans to play like the champion

**Status:** design spec (2026-08-06). Tracking issue: see the coach-suite issues on GitHub.
**Goal:** the dogfood stack (`server/` + `web/`) should not just let humans *play against*
the champion (net D, `best_raw_net.onnx`) — it should actively *teach* them to play like it.
This is the human-facing half of the #30 flywheel: better-taught humans play more games,
producing more/better records for eval.

## 1. Where we are

| Capability | Legacy stack (`webgame/`) | Dogfood stack (`server/` + `web/`) |
|---|---|---|
| Play vs champion | yes | yes (PR #32) |
| Game recording | yes (own DB, :5433) | yes — replay-complete records (PR #31) |
| Live coach hints (policy % per action) | **yes** (`WebGameServer.coach()`) | no |
| Value / win-prospect meter | yes (value in hint payload) | no |
| Danger overlay (opp tenpai, deal-in heat) | **yes** (#23, v4 heads) | no |
| Post-game review | no | no |
| Per-player progress stats | no | outcomes only (`human_eval.py`) |

The coach was built on the legacy stack (#23) and never ported when the dogfood stack
became the recorded, deployed one. Meanwhile the records (`game_records` + `game_events`)
deterministically replay every game — wall + seats + event stream reconstruct every hidden
hand at every decision point (`GameRecordingSpec`) — but nothing reads them back
(`GameRecordRepo.listGames/getGame/eventsFor` are test-only today).

## 2. What "teaching" means here

Three learning loops, in increasing depth:

1. **In-the-moment imitation** — while deciding, see what the champion would do and how
   strongly it prefers it. Fast feedback, builds intuition, zero friction.
2. **Threat awareness** — the single biggest human-vs-net skill gap in HK mahjong is
   defense: knowing when an opponent is tenpai and which discards deal in. The v4 danger
   heads (tenpai AUC 0.859) exist precisely for this (#21–#23).
3. **Post-game reflection** — replay a finished game, see every decision where you diverged
   from the champion, sorted by how much the champion disliked your move. Track
   agreement-rate over time as the "am I improving?" metric.

## 3. Features

### F1 — Live coach hints (server + web)

Attach a `coach` payload to every prompt sent to a human seat, and render it in `web/`.

- **Server:** `GameHooks.promptSink` already receives the engine's `CurState` on every
  decision and discards it. Instead: build obs (`V3Obs`/`V4Obs` by `svc.inputDim`), query
  the champion `OnnxPolicyService`, softmax the relevant head **restricted to legal
  actions**, and attach `CoachHint {probs: Map[actionKey, prob], value: Float}` to
  `Models.Prompt`. Mirror the head→key mapping from `WebGameServer.coach()`
  (binary pass=0/accept=1; self-kong pass=0, tile t→t+1; chow pass=0, pos→id+1;
  discard t→t). Hints must **never throw** — any failure degrades to no hint.
- **Web:** a 🎓 Coach toggle (persisted in `localStorage`, default **off** so first-time
  players aren't overwhelmed):
  - discard prompts: per-tile probability badge + highlight ring on the champion's top pick;
  - claim prompts (win/pong/kong/chow): a % badge on each button in `PromptPanel`;
  - the toggle lives in the game header.
- **Config:** coach model(s) from `MAHJONG_COACH_MODELS` (`name=path,...`), defaulting to
  the champion + danger model. Missing files are skipped with a log line, never fatal.

### F2 — Danger overlay

Load `rl/checkpoints/exit_v4/exit_final.onnx` alongside the champion (auto-detected as v4
by input dim = 1443). On the human's discard prompts:

- **opponent tenpai badges**: ⚠ p(tenpai) per opponent seat (amber ≥30%, red ≥60%);
- **per-tile deal-in heat**: `danger[t] = max_i p(tenpai_i) · p(wait_i[t])` rendered as a
  red bar/edge per hand tile, visually distinct from the discard-probability channel
  (jade fill = policy, cinnabar edge = danger — per the #23 guidance). Danger is shown as
  *relative* heat (waits hit@1 is only ~13%; absolute numbers would over-claim precision).

### F3 — Win-prospect meter

`PolicyOut.value` comes back free on every coach query (~1ms). Show the human seat's value
estimate as a small meter/sparkline in the header, updated on each of their prompts. Label
it "champion's assessment of your seat", not "win probability" (it's a money-value head).
Rides on the F1 payload — no extra server round-trip.

### F4 — Post-game review ("where the champion disagreed")

- **Server, replay:** a pure `GameReplayer` that takes `(wall, seats, events)` from
  `GameRecordRepo` and re-drives the engine to reconstruct the `CurState` at every decision
  point of a chosen seat — same determinism the recording spec proves. At each human
  decision: encode obs, query champion, record `{seq, decisionType, chosen, champion
  top action, probOfChosen, probOfTop, value}`.
- **REST:**
  - `GET /api/games?player=NAME&limit=N` — finished games (from `game_records`, humans only);
  - `GET /api/games/:id/review?seat=K` — the replayed decision list + summary
    (agreement rate, mean prob-gap, biggest disagreements first).
- **Web:** a `/review` page: list your recent games → per-game view showing each
  disagreement with the hand at that moment, what you played, what the champion would
  have played and at what probability. Sort by prob-gap (champion's top prob − prob of
  your move) so the top of the list is the most instructive mistake.
- Reviews are computed on demand and cached in memory (games are immutable once finished).

### F5 — Progress stats

- Server: `GET /api/players/:name/stats` — per-player, across all finished games:
  games, money/game, deal-in rate (from outcomes), and **champion-agreement rate** over
  time (from F4 reviews of that player's games).
- Web: shown at the top of `/review`. This is the "am I getting better?" number.
- `rl/human_eval.py` gains `--agreement` later (separate issue) once the replay endpoint
  exists; not in scope for the first PR.

## 4. Non-goals

- No changes to the legacy `webgame/` stack (frozen reference implementation).
- No model-quality work: the champion is final (#29); this is presentation only.
- No gating/eval claims: coach output is display-only, same stance as #23.
- No public deploy (still blocked on Next 14→16 per docs/DEPLOY.md).

## 5. Implementation plan

One PR on `claude/coach-suite`, commits in this order (each issue-tagged):

1. **CoachService** (server): multi-model ONNX loading (champion + v4 danger), obs
   selection by input dim, `hintFor(seat, curState, decisionType, context)` returning
   `CoachHint` — ported from `WebGameServer.coach()`. Unit-tested against a scripted state.
2. **Prompt plumbing**: `Models.Prompt` gains optional `coach` field; `GameRunner`
   promptSink computes hints for human seats only (champion seats never need them);
   `annotateForSeat` already routes prompts per-seat so no privacy leak.
3. **Web coach UI**: types, toggle, discard badges + ring, claim-button badges,
   danger heat, tenpai badges, value meter. Bespoke dark CSS (no Tailwind) matching
   `globals.css` tokens.
4. **GameReplayer + review API**: replay from records, review endpoints, in-memory cache.
5. **Review UI**: `/review` list + game detail page, progress stats header.
6. **Docs**: this file + README/DEPLOY notes (`MAHJONG_COACH_MODELS`, new endpoints).

Testing: sbt unit tests for CoachService + GameReplayer (replay determinism reuses
`GameRecordingSpec` fixtures); manual end-to-end via browser (create room vs champion,
verify hints/danger/value, finish a game, review it).

## 6. Risks / open questions

- **Replay fidelity**: the replayer must consume events in exactly the recorded order,
  including timeout-defaulted actions. Mitigation: assert replayed event stream ==
  recorded stream; refuse to review (410) on mismatch rather than show wrong analysis.
- **v4 obs from replay**: V4Obs needs discard order, which the replayer has natively.
- **Latency**: two ONNX queries per human decision (~2ms) on the game thread is fine;
  review replay of a full game is ~100 decisions ≈ a few hundred ms — do it on a blocking
  pool, cache the result.
- **UI overload**: default coach OFF; danger channel only drawn when a danger model is
  loaded; everything degrades to the current UI when models are missing.
