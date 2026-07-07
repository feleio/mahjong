# Mahjong Webgame

Multiplayer Hong Kong mahjong on the web, built on the Scala mahjong engine in
this repository. Create a room, share the 6-character code, fill empty seats
with bots, and play in the browser (desktop and mobile).

## Architecture

```
frontend (Next.js :3000)
   │  Socket.IO (see PROTOCOL.md)
backend (Node/TS :4000) ── Prisma ──> Postgres (:5433, docker)
   │  JSON lines over stdio
Scala engine (io.fele.app.mahjong.web.WebGameServer, fat jar)
```

- **Scala `WebGameServer`** (`src/main/scala-2.12/io/fele/app/mahjong/web/`):
  persistent process hosting many concurrent games. Human seats are
  `RemotePlayer`s whose decisions block on JSON messages; bot seats use the
  engine's `Chicken` player. Full-state snapshots accompany every event.
- **Backend**: rooms (host/join/bots/start), per-seat redacted game views,
  decision timeouts (45s discard / 20s claims) with sensible defaults,
  ~600ms event pacing so bot turns are watchable, persistence of users,
  rooms, games, and cumulative balances.
- **Frontend**: lobby + responsive game table, tiles drawn in CSS.

## Running

```bash
# 0) prerequisites: JDK 17, sbt, Node 20+, pnpm, docker

# 1) build the engine jar (repo root)
sbt assembly    # → target/scala-2.12/mahjong-assembly-0.1.0.jar

# 2) postgres
cd webgame && docker compose up -d

# 3) backend
cd backend
pnpm install
pnpm db:push        # create tables
pnpm dev            # :4000 (spawns the engine jar)

# 4) frontend
cd ../frontend
pnpm install
pnpm dev            # :3000
```

Open http://localhost:3000, enter a name, create a room, share the code (or
open a second browser/incognito window to join), add bots for empty seats and
start.

## Tests

- Engine bridge smoke test: `node webgame/backend/test/e2e.mjs` (requires the
  backend to be running) — plays two full games with two socket clients.

## Rules implemented (by the engine)

Hong Kong style: chow/pong/kong claims, self-kong, multiple simultaneous
winners on a discard, minimum 3 faan, score map {3:8 … 10:128}; discard win is
paid by the discarder, self-draw is paid by all three (half tariff each,
winner receives 1.5×). Draw (wall exhausted) carries no payments. Dealer stays
on a draw or dealer win, otherwise rotates.
