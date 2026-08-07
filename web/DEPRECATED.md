# Retired — do not add features here

`web/` was the first player-facing UI. The product is now
[`webgame/frontend/`](../webgame/frontend) — Next.js 16 / React 19 / Tailwind,
with the full mahjong table, the coach overlay and the review pages, talking to
the same `server/` backend.

This directory stays only until the deploy is switched over (issue #47), so the
dogfood stack keeps running during the migration. Anything you are tempted to
build here belongs in `webgame/frontend/` instead.

`webgame/backend/` (the Node + Socket.IO server that used to sit under the new
frontend) is retired for the same reason: `server/` is the single backend, and
it is the only one that records replay-complete games.
