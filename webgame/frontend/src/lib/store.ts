"use client";

// Lightweight external store (useSyncExternalStore) holding all realtime game
// state. The websocket handlers write here; components subscribe.
//
// It talks to the Scala `server/` (issue #47). The state shape is unchanged
// from the Socket.IO days on purpose — every component keeps rendering the
// same RoomState / GameView / Decision, and all protocol translation is
// confined to adapt.ts.

import { useSyncExternalStore } from "react";
import {
  adaptBalances,
  adaptPrompt,
  adaptRoom,
  adaptView,
  adaptWinners,
  actionToServer,
} from "./adapt";
import {
  getRoom,
  loadCreds,
  wsUrl,
  type RoomCreds,
  type ServerFrame,
  type ServerPrompt,
  type ServerRoom,
  type ServerSnapshot,
} from "./server";
import type { Decision, DecisionKind, GameEvent, GameView, RoomState } from "./types";

export interface GameOverInfo {
  winnersInfo: ReturnType<typeof adaptWinners>;
  balances: number[];
  view: GameView;
  /** epoch ms when the game ended (for ordering/debug). */
  receivedTs: number;
}

export type ConnState = "connecting" | "open" | "reconnecting";

export interface StoreState {
  connected: boolean;
  /** Finer-grained than `connected`, so the UI can say "reconnecting". */
  conn: ConnState;
  room: RoomState | null;
  view: GameView | null;
  decision: Decision | null;
  decisionReceivedTs: number;
  waiting: { seat: number; decision: string } | null;
  lastEvent: GameEvent | null;
  lastEventId: number;
  gameOver: GameOverInfo | null;
  roomClosed: string | null;
  /** Set when the engine played a turn for you because time ran out (#42). */
  autoPlayed: string | null;
  error: string | null;
}

const initialState: StoreState = {
  connected: false,
  conn: "connecting",
  room: null,
  view: null,
  decision: null,
  decisionReceivedTs: 0,
  waiting: null,
  lastEvent: null,
  lastEventId: 0,
  gameOver: null,
  roomClosed: null,
  autoPlayed: null,
  error: null,
};

let state: StoreState = initialState;
const listeners = new Set<() => void>();

function setState(partial: Partial<StoreState>): void {
  state = { ...state, ...partial };
  listeners.forEach((l) => l());
}

function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}

export function useStore(): StoreState {
  return useSyncExternalStore(
    subscribe,
    () => state,
    () => initialState,
  );
}

export function storeReset(): void {
  state = initialState;
  listeners.forEach((l) => l());
}

export function storeSetRoom(room: ServerRoom, creds: RoomCreds | null): void {
  setState({ room: adaptRoom(room, creds) });
}

export function storeClearDecision(): void {
  setState({ decision: null });
}

export function storeDismissAutoPlayed(): void {
  setState({ autoPlayed: null });
}

/* ---------- the room connection ---------- */

let conn: {
  roomId: string;
  socket: WebSocket | null;
  closed: boolean;
  retry: number;
  keepalive: ReturnType<typeof setInterval> | null;
  retryTimer: ReturnType<typeof setTimeout> | null;
} | null = null;

/** Latest snapshot in server form, so a prompt can be adapted against it. */
let lastSnapshot: ServerSnapshot | null = null;

function handleFrame(frame: ServerFrame, roomId: string, creds: RoomCreds | null): void {
  switch (frame.type) {
    case "snapshot": {
      lastSnapshot = frame;
      const view = adaptView(frame, roomId);
      const event = view.lastEvent;
      setState({
        view,
        ...(event ? { lastEvent: event, lastEventId: state.lastEventId + 1 } : {}),
        ...(frame.isFinished
          ? {
              gameOver: {
                winnersInfo: adaptWinners(frame),
                balances: adaptBalances(frame),
                view,
                receivedTs: Date.now(),
              },
              decision: null,
              waiting: null,
            }
          : {}),
      });
      break;
    }
    case "prompt": {
      const p = frame as ServerPrompt;
      if (creds && p.seat !== creds.seat) break; // not ours to answer
      const view = state.view ?? (lastSnapshot ? adaptView(lastSnapshot, roomId) : null);
      if (!view) break;
      setState({
        decision: adaptPrompt(p, view),
        decisionReceivedTs: Date.now(),
        waiting: null,
      });
      break;
    }
    case "waiting":
      // only meaningful for the seats that are not being asked
      if (!creds || frame.seat !== creds.seat) {
        setState({ waiting: { seat: frame.seat, decision: frame.decision } });
      }
      break;
    case "timeout":
      if (creds && frame.seat === creds.seat) {
        setState({ decision: null, autoPlayed: frame.kind });
      }
      break;
    case "lobby":
      setState({ room: adaptRoom(frame.room, creds) });
      break;
    case "end":
      setState({ decision: null, waiting: null });
      break;
    case "error":
      setState({ error: frame.message });
      break;
    case "ready_update":
      break; // the lobby re-fetches the room instead
  }
}

/**
 * Connect to a room and keep the connection up.
 *
 * Retries with backoff and reports its state, so a dropped socket shows as
 * "reconnecting" rather than a table that has quietly stopped moving. The
 * server decides at connect time whether a game is running, so callers must
 * reconnect when the room's status changes.
 */
export function connectRoom(roomId: string): () => void {
  disconnectRoom();
  const creds = loadCreds(roomId);
  const c = {
    roomId,
    socket: null as WebSocket | null,
    closed: false,
    retry: 0,
    keepalive: null as ReturnType<typeof setInterval> | null,
    retryTimer: null as ReturnType<typeof setTimeout> | null,
  };
  conn = c;

  const open = () => {
    if (c.closed) return;
    if (c.keepalive) clearInterval(c.keepalive);
    setState({ conn: c.retry === 0 ? "connecting" : "reconnecting" });

    const ws = new WebSocket(wsUrl(roomId, creds));
    c.socket = ws;

    ws.onopen = () => {
      if (c.closed) return;
      c.retry = 0;
      setState({ connected: true, conn: "open", error: null });
      // the room may have moved on while we were away
      getRoom(roomId)
        .then((r) => storeSetRoom(r, creds))
        .catch(() => undefined);
    };

    ws.onmessage = (ev) => {
      if (c.closed) return;
      try {
        handleFrame(JSON.parse(ev.data) as ServerFrame, roomId, creds);
      } catch {
        /* ignore non-JSON frames (keepalive pongs) */
      }
    };

    ws.onclose = () => {
      if (c.socket === ws) c.socket = null;
      if (c.closed) return;
      setState({ connected: false, conn: "reconnecting" });
      const delay = Math.min(15_000, 500 * 2 ** c.retry);
      c.retry += 1;
      c.retryTimer = setTimeout(open, delay);
    };

    // the server closes idle sockets; a periodic ping keeps a quiet table alive
    c.keepalive = setInterval(() => {
      if (ws.readyState === WebSocket.OPEN) ws.send("ping");
    }, 30_000);
  };

  open();
  return disconnectRoom;
}

export function disconnectRoom(): void {
  const c = conn;
  if (!c) return;
  c.closed = true;
  if (c.retryTimer) clearTimeout(c.retryTimer);
  if (c.keepalive) clearInterval(c.keepalive);
  if (c.socket) {
    c.socket.onclose = null; // a deliberate teardown must not schedule a retry
    c.socket.onmessage = null;
    c.socket.close();
  }
  conn = null;
  lastSnapshot = null;
  setState({ connected: false, conn: "connecting" });
}

/** Answer the current decision. Returns false when we are not connected. */
export function sendAction(kind: DecisionKind, action: boolean | number | null): boolean {
  const ws = conn?.socket;
  const decision = state.decision;
  if (!ws || ws.readyState !== WebSocket.OPEN || !decision) return false;
  ws.send(JSON.stringify(actionToServer(kind, action, decision.requestId)));
  setState({ decision: null, autoPlayed: null });
  return true;
}

/** Drop everything game-specific, keeping the room (used between games). */
export function storeClearGame(): void {
  lastSnapshot = null;
  setState({
    view: null,
    decision: null,
    waiting: null,
    lastEvent: null,
    gameOver: null,
    autoPlayed: null,
  });
}
