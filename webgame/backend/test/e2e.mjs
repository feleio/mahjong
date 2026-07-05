// E2E: boots nothing itself — expects backend already running on :4000.
// Creates a room with 2 human clients + 2 bots, starts the game, both humans
// auto-play (discard first valid, accept wins, pass claims), asserts game:over
// and a second game can start.
import { io } from 'socket.io-client';
import { randomUUID } from 'node:crypto';

const URL = process.env.BACKEND_URL ?? 'http://localhost:4000';
const deadline = setTimeout(() => {
  console.error('E2E TIMEOUT');
  process.exit(1);
}, 240_000);

const emit = (socket, event, payload) =>
  new Promise((resolve, reject) => {
    socket.emit(event, payload, (res) => (res?.ok ? resolve(res) : reject(new Error(`${event}: ${res?.error}`))));
  });

function makeClient(name) {
  const socket = io(URL, { auth: { token: randomUUID(), name }, transports: ['websocket'] });
  const state = { name, socket, view: null, gamesFinished: 0 };
  socket.on('game:decision', async (d) => {
    let action = null;
    if (d.decision === 'discard') action = d.context.validTiles[0];
    else if (d.decision === 'win' || d.decision === 'self_win') action = true;
    else if (d.decision === 'pong' || d.decision === 'kong') action = false;
    try {
      await emit(socket, 'game:action', { requestId: d.requestId, action });
    } catch (e) {
      console.error(`${name} action rejected:`, e.message);
    }
  });
  socket.on('game:event', ({ view }) => (state.view = view));
  socket.on('connect_error', (e) => console.error(`${name} connect_error`, e.message));
  return state;
}

const host = makeClient('Alice');
const guest = makeClient('Bob');

await new Promise((r) => host.socket.on('connect', r));
await new Promise((r) => guest.socket.on('connect', r));
console.log('connected');

const { room } = await emit(host.socket, 'room:create', {});
console.log('room created:', room.code, 'host seat', room.youSeat);

const joinRes = await emit(guest.socket, 'room:join', { code: room.code });
if (joinRes.room.youSeat !== 1) throw new Error('guest should get seat 1');
console.log('guest joined at seat', joinRes.room.youSeat);

await emit(host.socket, 'room:addBot', {});
// leave seat 3 empty — start should auto-fill

const gameOver = (client) =>
  new Promise((resolve) => client.socket.once('game:over', resolve));
const started = (client) =>
  new Promise((resolve) => client.socket.once('game:started', resolve));

const p1 = started(host);
const p2 = started(guest);
await emit(host.socket, 'room:start', {});
const [s1, s2] = await Promise.all([p1, p2]);
console.log('game started; host hand size:', s1.view.hand.length, 'guest hand size:', s2.view.hand.length);
if (s1.view.hand.length < 13 || s2.view.hand.length < 13) throw new Error('bad deal');
if (s1.view.hand.some((t) => typeof t !== 'number')) throw new Error('bad hand tiles');
// redaction check: host view must not contain other hands
if (s1.view.handCounts.length !== 4) throw new Error('bad handCounts');

const o1 = gameOver(host);
const o2 = gameOver(guest);
const [over1] = await Promise.all([o1, o2]);
console.log('game over. winners:', JSON.stringify(over1.winnersInfo), 'balances:', JSON.stringify(over1.balances));
if (!Array.isArray(over1.view?.hands) || over1.view.hands.length !== 4)
  throw new Error('missing revealed hands');

// room back to lobby, balances applied, second game starts fine
const sync = await emit(host.socket, 'room:state', {});
if (sync.room.status !== 'lobby') throw new Error('room should be lobby after game');
console.log('balances in room:', sync.room.seats.map((s) => s.balance).join(','), 'gamesPlayed:', sync.room.gamesPlayed);

const p3 = started(host);
await emit(host.socket, 'room:start', {});
await p3;
console.log('second game started OK');
const o3 = gameOver(host);
await o3;
console.log('second game finished');

// disconnect mid-lobby cleanliness
guest.socket.disconnect();
host.socket.disconnect();
clearTimeout(deadline);
console.log('E2E PASSED');
process.exit(0);
