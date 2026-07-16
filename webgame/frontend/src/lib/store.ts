"use client";

// Lightweight external store (useSyncExternalStore) holding all realtime
// game state. Socket event handlers write here; components subscribe.
// The server is authoritative: every `game:event` replaces the whole view.

import { useSyncExternalStore } from "react";
import { getSocket } from "./socket";
import type {
  Decision,
  GameEvent,
  GameEventPayload,
  GameOverPayload,
  GameStartedPayload,
  GameView,
  GameWaitingPayload,
  RoomClosedPayload,
  RoomState,
} from "./types";

export interface GameOverInfo extends GameOverPayload {
  /** epoch ms when we received game:over (for ordering/debug). */
  receivedTs: number;
}

export interface StoreState {
  connected: boolean;
  room: RoomState | null;
  view: GameView | null;
  decision: Decision | null;
  /** epoch ms when the current decision arrived (for the countdown bar). */
  decisionReceivedTs: number;
  waiting: GameWaitingPayload | null;
  lastEvent: GameEvent | null;
  /** monotonically increasing id so the event feed can re-flash on repeats. */
  lastEventId: number;
  gameOver: GameOverInfo | null;
  roomClosed: string | null;
}

const initialState: StoreState = {
  connected: false,
  room: null,
  view: null,
  decision: null,
  decisionReceivedTs: 0,
  waiting: null,
  lastEvent: null,
  lastEventId: 0,
  gameOver: null,
  roomClosed: null,
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

function getSnapshot(): StoreState {
  return state;
}

function getServerSnapshot(): StoreState {
  return initialState;
}

export function useStore(): StoreState {
  return useSyncExternalStore(subscribe, getSnapshot, getServerSnapshot);
}

// ---- imperative mutations used by pages ----

export function storeSetRoom(room: RoomState | null): void {
  setState({ room, roomClosed: null });
}

export function storeSync(
  room: RoomState | null,
  view: GameView | null,
  decision: Decision | null,
): void {
  setState({
    room,
    view,
    decision,
    decisionReceivedTs: decision ? Date.now() : 0,
    gameOver: view ? state.gameOver : null,
    roomClosed: null,
  });
}

/** Clear the decision (after we answered it). */
export function storeClearDecision(): void {
  setState({ decision: null });
}

/** Leaving the game-over modal back to lobby view. */
export function storeClearGame(): void {
  setState({
    view: null,
    decision: null,
    waiting: null,
    lastEvent: null,
    gameOver: null,
  });
}

export function storeReset(): void {
  state = { ...initialState, connected: getSocket().connected };
  listeners.forEach((l) => l());
}

// ---- socket bindings (idempotent) ----

let bound = false;

export function bindSocketToStore(): void {
  if (bound) return;
  bound = true;
  const s = getSocket();

  // The socket may already be connected by the time we bind (e.g. after a
  // client-side navigation) — sync the flag instead of waiting for an event.
  if (s.connected !== state.connected) setState({ connected: s.connected });

  s.on("connect", () => setState({ connected: true }));
  s.on("disconnect", () => setState({ connected: false }));

  s.on("room:update", (room: RoomState) => {
    setState({ room });
  });

  s.on("game:started", (payload: GameStartedPayload) => {
    setState({
      view: payload.view,
      decision: null,
      waiting: null,
      lastEvent: null,
      gameOver: null,
    });
  });

  s.on("game:event", (payload: GameEventPayload) => {
    // The view fully replaces the previous one. A stale decision whose
    // deadline has passed is dropped (server already auto-acted).
    // Untimed decisions (deadlineTs null) never go stale.
    const staleDecision =
      state.decision !== null &&
      state.decision.deadlineTs !== null &&
      Date.now() > state.decision.deadlineTs;
    setState({
      view: payload.view,
      lastEvent: payload.event,
      lastEventId: state.lastEventId + 1,
      waiting: null,
      ...(staleDecision ? { decision: null } : {}),
    });
  });

  s.on("game:decision", (decision: Decision) => {
    setState({
      decision,
      decisionReceivedTs: Date.now(),
      view: decision.view,
      waiting: null,
    });
  });

  s.on("game:waiting", (payload: GameWaitingPayload) => {
    setState({ waiting: payload });
  });

  s.on("game:over", (payload: GameOverPayload) => {
    setState({
      gameOver: { ...payload, receivedTs: Date.now() },
      view: payload.view,
      decision: null,
      waiting: null,
    });
  });

  s.on("room:closed", (payload: RoomClosedPayload) => {
    setState({ roomClosed: payload.reason ?? "Room closed" });
  });
}
