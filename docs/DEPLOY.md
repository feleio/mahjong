# Dogfood deploy (issue #30 step 2)

Private/LAN deploy of the full stack — Postgres + Scala game server (with the
champion bot and game recording) + Next.js front-end.

## One machine, LAN players

```bash
# PUBLIC_HOST = how players' browsers reach this machine (default: localhost)
PUBLIC_HOST=192.168.1.42 docker compose --profile app up -d --build
```

Then players open `http://192.168.1.42:3000`.

- `web` (port 3000) — the Next.js UI, built from `webgame/frontend`. `NEXT_PUBLIC_*` bases are baked at image
  build time, so changing `PUBLIC_HOST` requires `--build`.
- `server` (port 8080) — HTTP + WebSocket API. Bakes the champion ONNX
  (`rl/checkpoints/best_raw_net.onnx`) into the image; every game is recorded
  to Postgres (`game_records` / `game_events`).
- `postgres` (host port 5434 by default, override with `PG_PORT`) — same
  instance the dev workflow uses; game records accumulate in the `mahjong-pg`
  volume. Back this volume up — it is the human-data flywheel. (5434 rather
  than the usual 5433 to avoid colliding with other Postgres containers on
  a dev box; the in-network port the server uses is unaffected.)

Without `--profile app`, `docker compose up` still starts Postgres only
(unchanged dev workflow).

## Checks

```bash
curl http://localhost:8080/api/health
# 200 {"status":"ok","recording":true,"gamesRecorded":N,"recordingError":null,"champion":"ok"}
# 503 {"status":"degraded","recording":false,...} when game recording is down —
#     the health check does a live DB round-trip, so a dead Postgres shows up
#     here immediately instead of only in the boot logs.
docker compose logs server | grep -E "Champion|recording"
#   Game recording enabled (N stale in-progress games marked aborted)
#   Champion bot enabled (model: /app/model/best_raw_net.onnx)
```

Exported game data lives in Postgres:

```sql
SELECT count(*), status FROM game_records GROUP BY status;
SELECT count(*) FROM game_events;
```

## Configuration

| Env var | Default | What it does |
| --- | --- | --- |
| `PUBLIC_HOST` | `localhost` | Host players' browsers use. Baked into the web image, so changing it needs `--build`. |
| `WEB_PORT` | `3000` | Host port for the UI. |
| `PG_PORT` | `5434` | Host port for Postgres, bound to `127.0.0.1` only. |
| `MAHJONG_ALLOWED_ORIGINS` | set by compose | Comma-separated browser origins allowed to call the API and open game sockets. Compose sets it to this stack's own web origin; `*` allows any (the bare-`sbt run` default). |
| `MAHJONG_DB_PASSWORD` | `mahjong` | Postgres password, used by both the database and the server. **Set this for anything but a private machine.** |
| `MAHJONG_MAX_ROOMS` | `200` | Rooms held in memory before creation is refused with 429. |
| `MAHJONG_MAX_RUNNING_GAMES` | `32` | Concurrent games (each holds an engine thread). |
| `MAHJONG_ROOM_TTL_HOURS` | `24` | Idle rooms are evicted after this; rooms mid-game never are. |
| `MAHJONG_CREATE_ROOMS_PER_MINUTE` | `10` | Room creations per caller IP. |

## Security model

Access is by capability, with no accounts:

- A room's **join code or id is the capability to reach it**. There is no room
  listing, and the server never publishes the host id or any seat's player id
  (issue #51) — yours is handed to you once, on create/join, and lives in your
  browser's localStorage.
- Holding a seat's player id is what lets you play that seat; holding the host
  id is what lets you run the room.
- `MAHJONG_ALLOWED_ORIGINS` bounds which sites may call the API or open a game
  socket, so another page you visit cannot act as you (issue #52).

Two people who type the same display name still share one review history and
one stats page — names are not accounts. That is a known gap, tracked
separately, and it is worth knowing before inviting strangers.

## Before putting this on the public internet

Still required, and deliberately not in the compose file:

1. **TLS via a reverse proxy.** Everything here is plain HTTP/WS. A public
   deploy needs `https://` + `wss://` in front (and then `PUBLIC_HOST`,
   `NEXT_PUBLIC_*` and `MAHJONG_ALLOWED_ORIGINS` all have to use the https
   origin).
2. **A real `MAHJONG_DB_PASSWORD`**, not the default.
3. **Client IPs through the proxy.** The room-creation rate limit keys on the
   connection's remote address, so behind a proxy every caller shares one
   bucket unless the deploy forwards the real address.

On a LAN or VPN, the defaults above are safe to run as-is.
