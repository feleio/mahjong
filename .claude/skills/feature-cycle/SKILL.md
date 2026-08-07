---
name: feature-cycle
description: >
  Run a complete feature-delivery cycle for a product area of this repo:
  review current status, define new features and gaps, write a design spec,
  make an implementation plan, file GitHub issues, implement everything in a
  PR, test end-to-end with the browser MCP, and file follow-up issues for
  anything found along the way. Use this whenever the user asks to "review X
  and define/implement new features", "do the full cycle on X", "spec, file
  issues and implement", "run the feature cycle", or sets a goal that chains
  review → spec → issues → PR → browser testing — even if they don't name
  this skill explicitly.
---

# Feature-delivery cycle

Take one product area from "what do we have?" to "merged-ready PR with issues
filed for everything that remains." The value of the procedure is traceability:
every shipped change traces back to an issue, every issue to a spec section,
and every loose end found on the way ends up as a new issue instead of being
forgotten.

Track the phases with the task list (TaskCreate) so nothing is dropped; the
cycle spans hours and context compaction.

## Phase 1 — Review current status

- Read auto-memory (MEMORY.md) and open GitHub issues/PRs first:
  `gh issue list --state all --limit 50`, `gh pr list --state all --limit 30`.
  This repo's history lives in its issues — closed issues explain which levers
  were tried and rejected, so don't re-propose refuted ideas.
- Launch an Explore agent for a status map of the area (concrete file paths +
  line refs). Ask it specifically for: what already exists, what exists in a
  *different* stack/module and could be ported, how to run things locally, and
  where the data lives. Prior art in this repo is common — e.g. the legacy
  `webgame/` stack often has features the current `server/`+`web/` stack lacks.
- Verify ground facts yourself before designing on top of them (model files
  exist, containers run, ports match).

## Phase 2 — Spec and plan

- Write the design spec to `docs/<AREA>.md` (committed with the PR, not a
  scratch file). Structure: status table (what exists where) → goal → features
  F1..Fn with concrete server/web/config design per feature → non-goals →
  implementation plan (ordered commits) → risks/open questions.
- Ground every feature in evidence from Phase 1 (name files/line refs). Call
  out non-goals explicitly — especially things past issues already refuted.
- The plan should be one branch/PR unless features are independent enough that
  separate PRs reduce risk.

## Phase 3 — File GitHub issues

- Always `gh` CLI. One umbrella issue naming the goal, the spec's location,
  the children and the non-goals; then one issue per feature with enough
  implementation sketch that it stands alone (head/key mappings, endpoints,
  file paths). Cross-reference: children say "Part of #<umbrella>";
  dependencies say "depends on F1".

## Phase 4 — Implement

- Branch `claude/<area>-<slug>` off master.
- Read every file you will modify before editing; mirror existing idioms
  (bespoke dark CSS in `web/`, circe semiauto encoders, `assume`-guarded
  specs that skip when Postgres or ONNX models are absent).
- Order work so the server compiles and its tests pass before starting the
  frontend: `sbt server/Test/compile` → `sbt server/test` → `cd web && npm
  run build`.
- Write real tests, not smoke tests: fidelity/round-trip properties across
  many seeds, tamper/negative cases, cache-hit assertions.
- Update docs (README endpoint table, DEPLOY env vars, Dockerfile model
  copies) in the same commit — deploy drift is a recurring failure mode here.
- Commit referencing the issue numbers.

## Phase 5 — Test live with the browser MCP

- Redeploy: check what `PUBLIC_HOST` the running web image was baked with
  before rebuilding (`docker exec mahjong-web-1 sh -c 'grep -rho
  "http://[^\"]*:8080" .next | sort -u'`), then
  `docker compose --profile app up -d --build` and wait for
  `curl localhost:8080/api/health` to report ok.
- Drive the real UI with the claude-in-chrome tools: play through the actual
  user journey (create room, seat a champion bot, play decisions, finish,
  visit the new pages). Verify each shipped feature visually, not just via
  API calls. Screenshot key states.
- Anything broken, surprising, or out of scope that you notice: fix it if it
  is in scope for the PR, otherwise file a GitHub issue immediately (Phase 6)
  — do not keep a mental list.

## Phase 6 — PR + follow-ups

- Open the PR with `gh pr create`: summary per feature, issue references
  ("Closes #n" for completed children), test evidence (suite counts, browser
  findings), and the standard footer:

  🤖 Generated with [Claude Code](https://claude.com/claude-code)

- File follow-up issues for everything discovered but not fixed (UX gaps,
  deploy caveats, flaky behavior), each linking back to the umbrella issue.
- Comment on the umbrella issue with a status summary: what shipped, what's
  filed as follow-up, what's blocked on the user.
- Finish with a report to the user: features shipped, PR link, issues filed,
  anything needing their decision.

## Failure modes to avoid

- Designing features already refuted by closed issues (check first).
- Shipping server changes whose Docker image lacks a new model/asset.
- Testing only with curl when the deliverable is UI behavior.
- Losing findings from testing because they weren't filed as issues on the
  spot.
