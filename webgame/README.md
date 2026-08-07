# webgame

`frontend/` is the player-facing app — Next.js 16 / React 19 / Tailwind, talking
directly to the Scala `server/` over REST + WebSocket.

The Node + Socket.IO backend that used to live here was removed in issue #47:
`server/` is now the single backend, and it is the only one that records
replay-complete games, which the coach review and every eval depend on.

## Run it

```bash
docker compose up -d postgres          # from the repo root
sbt "server/run"                       # :8080
cd webgame/frontend && pnpm install && pnpm dev
```

Or the whole stack in Docker: `docker compose --profile app up -d --build`
(see docs/DEPLOY.md).
