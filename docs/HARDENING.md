# Public-deploy hardening: room auth, CORS, abuse limits

Issue #30's flywheel infrastructure is complete and humans are playing (9 games
recorded on the dogfood stack as of 2026-08-07), but the stack is confined to a
trusted LAN by two blockers named in #30's status: **CORS is `*` and rooms have
no auth**. This cycle removes those blockers so the public-deploy go/no-go
becomes a pure hosting decision instead of an engineering one.

## Status: what exists today (verified 2026-08-07, master @ 255c273)

| Concern | Where | State |
|---|---|---|
| CORS | `server/.../Routes.scala:27-39` | Hand-rolled `Access-Control-Allow-Origin: *`, no config knob |
| WS origin check | `server/.../Main.scala:79-85` | None — `WsRoutes` is mounted *outside* `withCors`; no `Origin` validation on upgrade (cross-site WebSocket hijacking surface) |
| Room secrets | `Routes.scala:114-122`, `Models.scala:106` | `GET /api/rooms` and `GET /api/rooms/:id` serve **every room's `hostId` and every seat's `playerId`** — the only credentials the server has |
| Seat auth | `WsRoutes.scala:34-39` | `?seat=&player=` query params matched against the (publicly served) `playerId` |
| Host auth | `RoomManager.scala:93,163,274` | Equality against the (publicly served) `hostId` |
| Game-list leak | `Routes.scala:42-51` | `GameListItem.seats: List[Seat]` includes historical `playerId`s |
| WS lobby frame | `WsRoutes.scala:44-47` | Serializes the full `Room` incl. `hostId`/`playerId`s to any spectator |
| Input validation | `Routes.scala:15,17` | None — `name`/`hostName` unbounded (frontend `maxLength=24` is cosmetic) |
| Abuse limits | `RoomManager.scala:52,159` | None — unbounded room creation, one OS thread per running game (`GameRunner.scala:53,76-78`), rooms never evicted |
| Room persistence | `RoomRepo.scala:60-66` | Round-trip drops `code`/`balances`/`gamesPlayed` — after a server restart, join-by-code URLs break and money totals reset |
| Postgres exposure | `docker-compose.yml:9` | Published on `0.0.0.0:5434` with password `mahjong` hardcoded (`docker-compose.yml:7`, `application.conf:28`) |
| Container users | `Dockerfile.server`, `webgame/frontend/Dockerfile` | Both run as root |
| Frontend identity | `webgame/frontend/src/lib/adapt.ts:36,47,50`, `Lobby.tsx:55-56` | UI identifies "you"/host by comparing the leaked `playerId`/`hostId` |
| Stale docs | `README.md:26,33-41`, `.gitignore` web/ block, `.claude/skills/feature-cycle/SKILL.md:90` | Reference the deleted `web/` stack, wrong Postgres port, npm instead of pnpm |

The consequence of the secrets row: `curl /api/rooms` → take any `hostId` →
you are the host of every room; take any seat's `playerId` → you can play that
person's hand over the websocket. Any auth built without fixing this is theatre.

## Goal

A capability-based access model with no accounts:

- **Knowing a room's code or UUID is the capability to join/spectate it.**
  Room ids are UUIDs and codes are 6 chars from a 29-symbol alphabet; both are
  unguessable *once the server stops listing them* (the frontend never used
  `GET /api/rooms` — verified by grep).
- **A seat's `playerId` is the capability to play it; the creator's `hostId`
  is the capability to run the room.** Both are UUIDs handed only to their
  owner at create/join time and never served to anyone else again.
- Browsers from non-allowed origins can neither read the API nor open sockets.
- One misbehaving client cannot exhaust server resources.

## Features

### F1 — Close the credential leak (public wire views)

Server (`Routes.scala`, `Models.scala`, `WsRoutes.scala`):

- New wire types: `SeatView(index, kind, name, occupied: Boolean)` and
  `RoomView(id, name, seats: List[SeatView], status, createdAt, code,
  balances, gamesPlayed)` — no `hostId`, no `playerId`. `occupied` =
  `playerId.isDefined` (the lobby renders open vs taken seats).
- Every endpoint that returns `Room` returns `RoomView` instead:
  `GET /api/rooms/:id`, and the room embedded in the responses of create,
  join, seat-change, start, ready, start-next. The caller's *own* credentials
  keep flowing through the existing dedicated fields
  (`CreateRoomResp.hostPlayerId`, `JoinResp.playerId`).
- The WS `lobby` frame (`WsRoutes.scala:44-47`) serializes `RoomView`.
- `GameListItem.seats` becomes id-free (index, kind, name).
- **Delete `GET /api/rooms`** (and `RoomManager.list`'s route). Nothing in
  `webgame/frontend/src` calls `listRooms` — verified.

Frontend (`webgame/frontend/src`):

- `types.ts`: `SeatInfo.userId`/`RoomState.hostUserId`/`youUserId` →
  seat-index-based: `RoomState.hostSeat: 0` (host always sits at seat 0 —
  `RoomManager.scala:55`, seat 0 unchangeable per `RoomManager.scala:97-98`),
  `youSeat` already exists from stored creds.
- `Lobby.tsx:55-56`: `isYou` = `seat.seat === room.youSeat`; `isSeatHost` =
  `seat.seat === room.hostSeat`.
- `adapt.ts:31-55`: stop reading `s.playerId`/`room.hostId`; `occupied` drives
  the empty-seat display; drop the unused `listRooms` wrapper in `server.ts`.

### F2 — Real CORS + WS origin allowlist

- New config `server.allowedOrigins` (default `"*"` for local dev), env
  `MAHJONG_ALLOWED_ORIGINS`, comma-separated exact origins
  (e.g. `http://192.168.1.42:3000`).
- Replace the hand-rolled `corsHeaders`/`withCors` (`Routes.scala:27-39`) with
  http4s's `org.http4s.server.middleware.CORS` configured from that list
  (`withAllowOriginHost` on the parsed set, or `withAllowOriginAll` when `*`).
- WS upgrade origin check in `WsRoutes` (http4s CORS middleware does not guard
  websockets): when the allowlist is not `*` and the request carries an
  `Origin` header not in the list → `403 Forbidden` before `wsb.build`.
  Requests without `Origin` (curl, native clients) pass — the threat model is
  cross-site browser hijacking, not API access control.
- `docker-compose.yml`: server gets
  `MAHJONG_ALLOWED_ORIGINS: http://${PUBLIC_HOST:-localhost}:${WEB_PORT:-3000}`
  — the deployed stack is locked to its own web origin by default.

### F3 — Input validation + abuse limits

- Server-side validation shared by create/join: display name and room name
  trimmed, 1–24 chars (mirror of the frontend's `maxLength=24`), else 400.
- `server.maxRooms` (default 200): `create` rejects with 429 when the
  in-memory room map is at the cap.
- `server.maxRunningGames` (default 32): `startGame`/`startNextGame` reject
  when that many runners are live (each holds a dedicated OS thread —
  `GameRunner.scala:53`).
- Per-IP rate limit on `POST /api/rooms`: 10/minute sliding window keyed on
  the connection's remote address (`Request.remote`). Documented caveat: put
  the server behind a proxy and this keys on the proxy — acceptable until the
  TLS-proxy follow-up lands.
- Room eviction: a background fs2 stream (every 10 min) drops in-memory rooms
  with no live runner that have been `Finished`/`Waiting` for > 24 h
  (config `server.roomTtlHours`). DB rows are kept — game records and review
  are unaffected.

### F4 — Room persistence round-trip fix

`rooms` table gains `code`, `balances`, `games_played` columns (same
`ALTER TABLE ... ADD COLUMN IF NOT EXISTS` migration idiom as
`GameRecordRepo.scala:123-125`); `RoomRepo.upsert`/`rowToRoom` persist and
restore them. Fixes two real bugs that matter once the code is the capability:
after a server restart, `/room/CODE` URLs stop resolving (restored `code` is
`""`) and room money totals silently reset.

### F5 — Deploy hardening + stale-stack cleanup

- `docker-compose.yml`: Postgres binds `127.0.0.1:${PG_PORT:-5434}:5432`;
  password becomes `${MAHJONG_DB_PASSWORD:-mahjong}` for both `postgres` and
  `server` services.
- Both Dockerfiles get a non-root `USER`.
- Root `.gitignore`: cover `.env` and `webgame/frontend/.env*` (the current
  Next.js block only covers the deleted `web/`).
- `docs/DEPLOY.md`: env-var table (`MAHJONG_ALLOWED_ORIGINS`,
  `MAHJONG_DB_PASSWORD`, loopback `PG_PORT`), rewrite the "Not for public
  internet" section (L49-54) to the new model: LAN-safe out of the box; public
  exposure additionally needs the TLS reverse proxy (follow-up issue).
- Stale-stack cleanup: `README.md` frontend section (`web/`+npm →
  `webgame/frontend`+pnpm), Postgres port 5432→5434, REST table completed
  (`/ready`, `/start-next`, `/health`); remove unused `socket.io-client` from
  `webgame/frontend/package.json`; fix `.claude/skills/feature-cycle/SKILL.md:90`
  (`cd web && npm run build` → `cd webgame/frontend && pnpm build`); drop the
  checked-in `tsconfig.tsbuildinfo`.

## Non-goals (explicit)

- **Accounts / durable player identity.** `game_records` stays keyed on the
  display-name string (`GameRecordRepo.scala:255`, `rl/human_eval.py:169`).
  Two people named "Alice" still merge. This is a coordinated schema + Python
  change — filed as a follow-up, not smuggled in here.
- **TLS / reverse proxy in compose.** Public exposure still requires one
  (wss:// especially); filed as a follow-up. The go/no-go on actually facing
  the internet remains the owner's call — this cycle only removes the
  engineering blockers.
- **WS credential moved out of the query string** (first-message auth).
  Follow-up; risk today is limited to proxy access logs.
- **Password-protected/private rooms.** The capability model (unguessable
  code/UUID, no listing) is the access control.
- **Retried levers.** Nothing here touches training/search; those tracks are
  closed (#13, #24, #25, #29).

## Implementation plan (one branch, ordered commits)

1. F1 server: `SeatView`/`RoomView`, endpoint + WS-lobby switch, delete
   `GET /api/rooms`, sanitize `GameListItem`; new `WireHygieneSpec` asserting
   the serialized JSON of every public payload contains no `hostId`/`playerId`
   key anywhere.
2. F2: `allowedOrigins` config, http4s CORS middleware, WS origin gate; specs
   for allowed/disallowed/no-origin on both REST and WS routes.
3. F3: validation + caps + rate limit + eviction; specs (reject paths, cap
   boundary, eviction of stale vs live rooms).
4. F4: rooms schema migration + round-trip property spec (Postgres-gated via
   the existing `assume` idiom).
5. F1 frontend: `types.ts`/`adapt.ts`/`Lobby.tsx`/`server.ts` seat-index
   identity; `pnpm lint && pnpm build`.
6. F5: compose/Dockerfiles/.gitignore/DEPLOY.md/README/SKILL.md cleanup.

Gate order per repo convention: `sbt server/Test/compile` → `sbt server/test`
(check the Postgres-skip count — suites self-skip when Postgres is absent) →
`cd webgame/frontend && pnpm lint && pnpm build`.

## Risks / open questions

- **Reconnect compatibility:** stored `RoomCreds` (localStorage) keep working
  — `playerId` is unchanged, only its *publication* stops. Live rooms created
  before the deploy also keep working; `RoomView` is a projection, not a
  schema change.
- **`GET /api/players/:name/stats` and `/api/games?player=`** stay
  name-keyed and public. Anyone can read anyone's aggregate stats/history by
  guessing a display name (ids no longer leak, hands in finished games are
  visible by design). Acceptable for the dogfood cohort; folded into the
  accounts follow-up.
- **http4s CORS middleware behavior differences** (e.g. `Vary` handling,
  preflight status) could break the frontend's fetches — covered by Phase 5
  live browser testing before merge.
- **Rate-limit state is in-memory** — resets on restart, not shared across
  replicas. Fine: the server is a single process by design (in-memory rooms).
