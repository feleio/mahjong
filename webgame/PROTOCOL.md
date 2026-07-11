# Mahjong Webgame — Realtime Protocol (Socket.IO)

Single Socket.IO connection per client, default namespace. Server: `http://localhost:4000`.

## Auth (handshake)

```js
io(SERVER_URL, { auth: { token: "<uuid from localStorage>", name: "<display name>" } })
```

Server upserts a `User` by `token`. Changing `name` updates the user. All
subsequent events are in the context of that user.

## Tile encoding

Tiles are integers 0–33 (engine encoding):

- 0–8: Dots (筒) 1–9
- 9–17: Bamboo (索) 1–9
- 18–26: Characters (萬) 1–9
- 27–30: Winds E S W N (東南西北)
- 31–33: Dragons Red Green White (中發白)

Chow position ids: 0 = LEFT (claimed tile is lowest of the run), 1 = MIDDLE, 2 = RIGHT.

## Client → Server (all use ack callback `(res) => {}`; res is `{ ok: true, ... }` or `{ ok: false, error: string }`)

| Event | Payload | Ack on success | Notes |
|---|---|---|---|
| `room:create` | `{}` | `{ ok, room: RoomState }` | Creator becomes host, seat 0 |
| `room:join` | `{ code: string }` | `{ ok, room: RoomState }` | Takes lowest free seat. Rejoining an active game re-attaches to the previous seat. |
| `room:leave` | `{}` | `{ ok }` | Host leaving passes host to next human; empty room closes |
| `room:addBot` | `{}` | `{ ok, room }` | Host only, lobby only; fills lowest free seat with a bot |
| `room:removeBot` | `{ seat: number }` | `{ ok, room }` | Host only, lobby only |
| `room:start` | `{}` | `{ ok }` | Host only; remaining free seats are auto-filled with bots |
| `room:state` | `{}` | `{ ok, room: RoomState \| null, game: GameView \| null, decision: Decision \| null }` | Full resync (call on connect/reconnect) |
| `game:action` | `{ requestId: number, action: boolean \| number \| null }` | `{ ok }` | Answer to a `game:decision`. Ignored if the request already timed out. |

## Server → Client

| Event | Payload | Audience |
|---|---|---|
| `room:update` | `RoomState` | everyone in room |
| `game:started` | `{ gameId, dealerSeat, view: GameView }` | per-socket (view is seat-specific) |
| `game:event` | `{ event: GameEvent, view: GameView }` | per-socket (redacted view) |
| `game:decision` | `Decision` | only the seat that must act |
| `game:waiting` | `{ seat: number, decision: string }` | everyone else in room (show "waiting for X…") |
| `game:over` | `{ winnersInfo: WinnersInfo \| null, balances: number[4], view: GameView }` | per-socket; final view reveals all hands |
| `room:closed` | `{ reason: string }` | everyone in room |

## Types

```ts
type Seat = 0 | 1 | 2 | 3;

interface SeatInfo {
  seat: Seat;
  name: string;          // display name or "Bot 2"
  isBot: boolean;
  userId: string | null; // null for bots / empty seats
  connected: boolean;    // false when a human's socket is down
  balance: number;       // cumulative across games in this room
  empty: boolean;        // true if nobody (lobby only)
}

interface RoomState {
  code: string;              // 6-char join code
  status: "lobby" | "playing";
  hostUserId: string;
  seats: SeatInfo[];         // always length 4
  youSeat: Seat | null;      // your seat in this room
  youUserId: string;
  gamesPlayed: number;
}

interface MeldGroup { kind: "pong" | "kong" | "chow"; tiles: number[] } // tiles are full set, sorted

interface GameView {
  gameId: string;
  yourSeat: Seat;
  dealerSeat: Seat;
  curSeat: Seat;             // whose turn (engine's current player)
  remaining: number;         // tiles left in wall
  hand: number[];            // YOUR tiles only, sorted
  handCounts: number[];      // concealed tile count per seat
  melds: MeldGroup[][];      // per seat, exposed groups
  discards: { seat: Seat; tile: number }[]; // chronological river
  pendingDiscard: { seat: Seat; tile: number } | null; // just-discarded tile, claimable, not yet in river
  lastDrawnTile: number | null;  // set only if YOU just drew it (highlight in hand)
  lastEvent: GameEvent | null;
}

type GameEvent =
  | { kind: "draw"; seat: Seat; tile: number | null } // tile null unless it's you
  | { kind: "discard"; seat: Seat; tile: number }
  | { kind: "pong" | "kong"; seat: Seat; fromSeat: Seat; tile: number }
  | { kind: "chow"; seat: Seat; fromSeat: Seat; tile: number; position: 0 | 1 | 2 };

interface Decision {
  requestId: number;
  decision: "discard" | "self_win" | "win" | "self_kong" | "kong" | "pong" | "chow";
  context: {
    tile?: number;         // the tile involved (win/kong/pong/chow/self_win)
    score?: number;        // win score (win/self_win)
    fromSeat?: Seat;       // who discarded (win/kong/pong/chow)
    validTiles?: number[]; // discard: your hand; self_kong: kongable tiles
    positions?: (0|1|2)[]; // chow options
  };
  deadlineTs: number;      // epoch ms; server auto-acts after this
  view: GameView;
  coach?: CoachHint;       // AI-coach hint; present only when the engine runs
                           // with -Dweb.coachmodel (backend env COACH_MODEL)
}

// The strongest model's probability over this decision's actions.
// Keys: discard → tile value ("0".."33", over validTiles);
// win/self_win/pong/kong → "pass" / "accept";
// chow → "pass" / position id ("0"|"1"|"2");
// self_kong → "pass" / tile value.
interface CoachHint {
  probs: { [key: string]: number }; // sums to 1 over the legal actions
  value?: number;                   // net's $-scale estimate of the position
}

interface WinnersInfo {
  winners: { seat: Seat; score: number }[];
  loserSeat: Seat | null;  // null on self-draw win
  winningTile: number;
  isSelfWin: boolean;
}
```

## Decision actions (`game:action` payload `action` field)

| decision | action |
|---|---|
| `discard` | tile number from `validTiles` |
| `self_win`, `win` | `true` to win, `false` to pass |
| `pong`, `kong` | `true` / `false` |
| `chow` | position id from `positions`, or `null` to pass |
| `self_kong` | tile from `validTiles`, or `null` to pass |

Timeouts: server auto-acts (discard = last-drawn tile or first valid; win = accept; claims = pass) at `deadlineTs`. Discard timeout 45s, claims 20s.

## Game flow notes

- After `discard` event, the tile sits in `pendingDiscard` while others are asked
  to win/kong/pong/chow it (you may receive `game:decision`, others see
  `game:waiting`). If nobody claims, the next `draw` event moves it into `discards`.
- After a pong/chow/kong the claiming seat must discard (a `discard` decision arrives).
- Scoring: minimum 3 faan; payouts per score map {3:8, 4:16, 5:24, 6:32, 7:48, 8:64, 9:96, 10:128}.
  Discard win: loser pays each winner. Self-draw: the other three each pay half the tariff
  (winner receives 1.5×).
- Bot turns are paced ~600ms per event so the game is watchable.
