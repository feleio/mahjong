# Mahjong

A multiplayer mahjong web app driven by the original Scala game engine.

## Layout

| Path        | What it is |
|-------------|------------|
| `src/`      | Original Scala 2.12 mahjong engine (now exposed as the `core` sbt module). |
| `server/`   | New Scala / http4s / doobie backend that wraps the engine, exposes REST + WebSocket APIs and persists rooms in Postgres. |
| `web/`      | Next.js 14 (TypeScript) frontend with lobby, room (host configures seats / start), and live game UI. |
| `rl/`       | Existing reinforcement learning Python tooling (untouched). |

## Quick start (development)

### 1. Postgres

```bash
docker compose up -d postgres
```

### 2. Backend (Scala, http4s)

```bash
sbt "server/run"
# defaults: HTTP + WS on :8080, Postgres at jdbc:postgresql://localhost:5432/mahjong
```

Configuration is driven by `server/src/main/resources/application.conf` and
overridable via env vars `MAHJONG_SERVER_HOST`, `MAHJONG_SERVER_PORT`,
`MAHJONG_DB_URL`, `MAHJONG_DB_USER`, `MAHJONG_DB_PASSWORD`.

### 3. Frontend (Next.js)

```bash
cd web
cp .env.example .env.local       # adjust API/WS hosts if needed
npm install
npm run dev
# open http://localhost:3000
```

## How it plays

* The lobby (`/`) lets you set a display name, create a room, or join an existing one.
* The room creator (host) sees a seat configuration panel:
  * Seat 1 is always the host (Human).
  * Seats 2–4 can be set to **Human** (an open slot any guest can take from
    the lobby) or to one of the bundled AI bots: **Chicken**, **Random**,
    **Felix**, or **3-point Chicken** — each is a `Player` from the engine.
* Once **all four seats are filled**, the host's **Start game** button lights up.
* During play, every connected client receives live snapshots over a
  WebSocket (`/ws/rooms/:id`). The server hides other players' hands. When
  the engine asks the human at your seat for a decision (win/kong/pong/chow/
  self-kong/discard), a prompt panel appears with the matching options.

The server runs the existing `FlowImpl` (synchronous engine) on a worker
thread per game; human seats use a `WebSocketPlayer` whose `decide*` methods
block on a queue fed by the websocket.

## REST API summary

```
GET    /api/rooms                     list rooms
POST   /api/rooms                     { name, hostName }            create
GET    /api/rooms/:id                 fetch a room
POST   /api/rooms/:id/join            { name, seatIndex? }          join an open seat
PATCH  /api/rooms/:id/seat            { hostPlayerId, seatIndex, kind }
POST   /api/rooms/:id/start           { hostPlayerId }
GET    /ws/rooms/:id?seat=<n>&player=<playerId>     gameplay websocket
```

## Notes

* The original engine code is unchanged — the new backend depends on the
  `core` module via sbt and reuses `FlowImpl`, all `Player` subclasses,
  `RandomTileDrawer`, `Hand`, `ScoreCalculator`, and `GameLogger`.
* In-flight games live entirely in memory; only room configuration and seat
  assignments are persisted to Postgres so users can rejoin after a refresh.
* The original `sbt run` of the engine alone (the `core` project) still
  works — `sbt "core/run"` from the multi-project root.

## Running the original engine simulation

```sbt
sbt "core/runMain io.fele.app.mahjong.Main"
```
