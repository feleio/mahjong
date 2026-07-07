"use client";

import { io, Socket } from "socket.io-client";
import { getName, getToken } from "./identity";

const URL =
  process.env.NEXT_PUBLIC_BACKEND_URL ?? "http://localhost:4000";

let socket: Socket | null = null;

/** Singleton socket. Created lazily; does not auto-connect. */
export function getSocket(): Socket {
  if (!socket) {
    socket = io(URL, {
      auth: { token: getToken(), name: getName() },
      autoConnect: false,
    });
  }
  return socket;
}

/** Refresh auth from localStorage and connect if not already connected. */
export function ensureConnected(): Socket {
  const s = getSocket();
  s.auth = { token: getToken(), name: getName() };
  if (!s.connected) s.connect();
  return s;
}

/** Promise wrapper around Socket.IO ack callbacks. */
export function emitAck<TRes>(
  event: string,
  payload: object = {},
  timeoutMs = 8000,
): Promise<TRes> {
  const s = ensureConnected();
  return new Promise<TRes>((resolve, reject) => {
    s.timeout(timeoutMs).emit(event, payload, (err: Error | null, res: TRes) => {
      if (err) reject(new Error("Server did not respond. Is it running?"));
      else resolve(res);
    });
  });
}
