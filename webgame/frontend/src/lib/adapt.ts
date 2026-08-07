// Adapts `server/`'s wire shapes into the shapes this app's components already
// render (issue #47). Everything protocol-specific lives here so the UI keeps
// working unchanged: components still receive RoomState / GameView / Decision.

import type {
  CoachHint,
  Decision,
  DecisionKind,
  GameEvent,
  GameView,
  MeldGroup,
  RoomState,
  Seat,
  SeatInfo,
  WinnersInfo,
} from "./types";
import type {
  RoomCreds,
  ServerCoachHint,
  ServerPrompt,
  ServerRoom,
  ServerSnapshot,
} from "./server";
import { asSeat, chowFromWire, chowToWire, isBot, tileFromWire, tilesFromWire, tileToWire } from "./wire";

/** The coach model this server exposes. `server/` runs one champion (plus an
 *  optional danger model folded into the same hint), where the old backend
 *  offered a picker over several nets. */
export const COACH_MODEL = "Champion";

export function adaptRoom(room: ServerRoom, creds: RoomCreds | null): RoomState {
  const seats: SeatInfo[] = room.seats.map((s, i) => {
    // A seat can be kind "human" with nobody in it — the host converting a bot
    // seat back to a human one leaves it waiting for someone. Going by kind
    // alone would draw a phantom player there that nobody can be and the host
    // cannot clear, so occupancy decides.
    const vacant = s.kind === "open" || (!isBot(s.kind) && !s.occupied);
    return {
      seat: asSeat(s.index),
      name: vacant ? "—" : s.name,
      isBot: isBot(s.kind),
      // The server does not track presence yet; a seated human is shown as
      // present rather than falsely flagged as disconnected.
      connected: true,
      balance: room.balances[i] ?? 0,
      empty: vacant,
    };
  });
  return {
    code: room.code || room.id.slice(0, 6).toUpperCase(),
    // "finished" keeps the table on screen with the game-over modal over it
    status: room.status === "waiting" ? "lobby" : "playing",
    hostSeat: asSeat(room.hostSeat ?? 0),
    seats,
    youSeat: creds ? asSeat(creds.seat) : null,
    gamesPlayed: room.gamesPlayed,
    coachModels: [COACH_MODEL],
    enforceTimeLimit: true,
  };
}

function adaptMelds(snap: ServerSnapshot): MeldGroup[][] {
  return snap.players.map((p) =>
    p.fixedGroups.map((g) => ({ kind: g.kind, tiles: tilesFromWire(g.tiles) })),
  );
}

export function adaptEvent(snap: ServerSnapshot): GameEvent | null {
  const e = snap.event;
  if (!e || e.seat === null) return null;
  const seat = asSeat(e.seat);
  const tile = tileFromWire(e.tile);
  switch (e.kind) {
    case "draw":
      return { kind: "draw", seat, tile };
    case "discard":
      return tile === null ? null : { kind: "discard", seat, tile };
    case "pong":
    case "kong":
      return tile === null || e.fromSeat === null
        ? null
        : { kind: e.kind, seat, fromSeat: asSeat(e.fromSeat), tile };
    case "chow": {
      const pos = chowFromWire(e.position);
      return tile === null || e.fromSeat === null || pos === null
        ? null
        : { kind: "chow", seat, fromSeat: asSeat(e.fromSeat), tile, position: pos };
    }
    default:
      return null; // start / resume / end are not table events
  }
}

export function adaptView(snap: ServerSnapshot, roomId: string): GameView {
  const yourSeat = asSeat(snap.yourSeat ?? 0);
  const me = snap.players.find((p) => p.seat === yourSeat);
  const event = snap.event;
  return {
    // the server has no per-game id on the wire; room + game number is stable
    // for the lifetime of one game, which is all the UI keys off
    gameId: `${roomId}#${snap.gamesPlayed}`,
    yourSeat,
    dealerSeat: asSeat(snap.dealerSeat),
    curSeat: asSeat(snap.curPlayer),
    remaining: snap.remainingTiles,
    hand: tilesFromWire(me?.handTiles ?? []),
    handCounts: snap.players.map((p) => p.handCount),
    melds: adaptMelds(snap),
    discards: snap.discards.map((d) => ({
      seat: asSeat(d.seat),
      tile: tileFromWire(d.tile) ?? 0,
    })),
    pendingDiscard: snap.pendingDiscard
      ? {
          seat: asSeat(snap.pendingDiscard.seat),
          tile: tileFromWire(snap.pendingDiscard.tile) ?? 0,
        }
      : null,
    lastDrawnTile:
      event?.kind === "draw" && event.seat === yourSeat ? tileFromWire(event.tile) : null,
    lastEvent: adaptEvent(snap),
    hands: snap.isFinished
      ? snap.players.map((p) => (p.handTiles ? tilesFromWire(p.handTiles) : null))
      : undefined,
  };
}

export function adaptWinners(snap: ServerSnapshot): WinnersInfo | null {
  if (!snap.isFinished || snap.winners.length === 0) return null;
  const winningTile = tileFromWire(snap.winningTile);
  if (winningTile === null) return null;
  return {
    winners: snap.winners.map((w) => ({ seat: asSeat(w.seat), score: w.score })),
    loserSeat: snap.loserSeat === null ? null : asSeat(snap.loserSeat),
    winningTile,
    isSelfWin: snap.selfWin,
  };
}

/** Cumulative per-seat money including the game that just ended. */
export function adaptBalances(snap: ServerSnapshot): number[] {
  return [0, 1, 2, 3].map((i) => (snap.balances[i] ?? 0) + (snap.balanceDelta[i] ?? 0));
}

/** Server prob keys are wire names; this app's components key on tile ints. */
function adaptProbKeys(probs: Record<string, number>): Record<string, number> {
  const out: Record<string, number> = {};
  for (const [k, v] of Object.entries(probs)) {
    if (k === "pass" || k === "accept") {
      out[k] = v;
      continue;
    }
    const chow = chowFromWire(k);
    if (chow !== null) {
      out[String(chow)] = v;
      continue;
    }
    const tile = tileFromWire(k);
    if (tile !== null) out[String(tile)] = v;
  }
  return out;
}

export function adaptCoach(hint: ServerCoachHint): CoachHint {
  const danger = hint.dangerByTile
    ? Object.fromEntries(
        Object.entries(hint.dangerByTile)
          .map(([k, v]) => [tileFromWire(k), v] as const)
          .filter(([t]) => t !== null)
          .map(([t, v]) => [String(t), v]),
      )
    : undefined;
  return {
    probs: adaptProbKeys(hint.probs),
    value: hint.value,
    oppTenpai: hint.oppTenpai?.map((o) => ({ seat: asSeat(o.seat), p: o.p })),
    dangerByTile: danger,
  };
}

export function adaptPrompt(p: ServerPrompt, view: GameView): Decision {
  const positions = (p.chowPositions ?? [])
    .map(chowFromWire)
    .filter((x): x is 0 | 1 | 2 => x !== null);
  const validTiles =
    p.kind === "discard"
      ? tilesFromWire(p.handTiles ?? [])
      : p.kind === "self_kong"
        ? tilesFromWire(p.selfKongTiles ?? [])
        : undefined;
  return {
    requestId: p.promptId ?? 0,
    decision: p.kind as DecisionKind,
    context: {
      tile: tileFromWire(p.tile) ?? undefined,
      score: p.score ?? undefined,
      // whoever's discard is on the table is the one being claimed
      fromSeat: view.pendingDiscard ? view.pendingDiscard.seat : undefined,
      validTiles,
      positions: positions.length ? positions : undefined,
    },
    deadlineTs: p.timeoutMs ? Date.now() + p.timeoutMs : null,
    view,
    coach: p.coach ? { [COACH_MODEL]: adaptCoach(p.coach) } : undefined,
  };
}

/** The action encoding this app uses, translated for the server. */
export function actionToServer(
  kind: DecisionKind,
  action: boolean | number | null,
  promptId: number,
): Record<string, unknown> {
  const base: Record<string, unknown> = { kind, promptId };
  switch (kind) {
    case "discard":
    case "self_kong":
      // a null here is a pass, which only self_kong offers
      return typeof action === "number" ? { ...base, tile: tileToWire(action) } : base;
    case "chow":
      return typeof action === "number"
        ? { ...base, chowPos: chowToWire(action as 0 | 1 | 2) }
        : base;
    default:
      return { ...base, yes: action === true };
  }
}

export type { Seat };
