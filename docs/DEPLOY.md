# Dogfood deploy (issue #30 step 2)

Private/LAN deploy of the full stack — Postgres + Scala game server (with the
champion bot and game recording) + Next.js front-end.

## One machine, LAN players

```bash
# PUBLIC_HOST = how players' browsers reach this machine (default: localhost)
PUBLIC_HOST=192.168.1.42 docker compose --profile app up -d --build
```

Then players open `http://192.168.1.42:3000`.

- `web` (port 3000) — Next.js UI. `NEXT_PUBLIC_*` bases are baked at image
  build time, so changing `PUBLIC_HOST` requires `--build`.
- `server` (port 8080) — HTTP + WebSocket API. Bakes the champion ONNX
  (`rl/checkpoints/best_raw_net.onnx`) into the image; every game is recorded
  to Postgres (`game_records` / `game_events`).
- `postgres` (host port 5433) — same instance the dev workflow uses; game
  records accumulate in the `mahjong-pg` volume. Back this volume up — it is
  the human-data flywheel.

Without `--profile app`, `docker compose up` still starts Postgres only
(unchanged dev workflow).

## Checks

```bash
curl http://localhost:8080/api/health          # {"status":"ok"}
docker compose logs server | grep -E "Champion|recording"
#   Game recording enabled (N stale in-progress games marked aborted)
#   Champion bot enabled (model: /app/model/best_raw_net.onnx)
```

Exported game data lives in Postgres:

```sql
SELECT count(*), status FROM game_records GROUP BY status;
SELECT count(*) FROM game_events;
```

## Not for public internet

Public deploy is blocked on the Next 14→16 migration (residual advisories on
all of 14.x) — see issue #30. CORS is `*` and rooms have no auth; keep this
behind a LAN/VPN.
