import random
from typing import List
from copy import deepcopy

from src.main.python.tile import pretty, symbol
from src.main.python.player import Player, Human, RandomP

draw = 0
pong = 1
chow = 2


class State:
    def __init__(self):
        self.hands = [[0 for _ in range(len(symbol))] for _ in range(4)]
        self.docks = [[0 for _ in range(len(symbol))] for _ in range(4)]
        self.walls = [4 for _ in range(len(symbol))]
        self.trash = [0 for _ in range(len(symbol))]
        for hand_idx in range(4):
            for _ in range(13):
                self.hands[hand_idx][self.draw()] += 1

    def __repr__(self) -> str:
        return "|".join("".join(map(str, hand)) for hand in self.hands)

    def pretty(self) -> str:
        return "|".join("".join(pretty[set_idx]*h for set_idx, h in enumerate(hand)) for hand in self.hands)

    def symbol(self, pid=None) -> str:
        if pid is not None:
            return ",".join(symbol[set_idx] * h for set_idx, h in enumerate(self.hands[pid]) if h)
        return "|".join(",".join(symbol[set_idx]*h for set_idx, h in enumerate(hand) if h) for hand in self.hands)

    def draw(self) -> int:
        if any(self.walls):
            picked = random.choices(range(len(symbol)), self.walls)[0]
            self.walls[picked] -= 1
            return picked
        else:
            raise StopIteration()

    @staticmethod
    def is_win(hand:List[int])->bool:
        for possible_eye_idx in range(len(hand)):
            if hand[possible_eye_idx] >= 2:
                new_hand = hand[:]
                new_hand[possible_eye_idx] -= 2
                for h in [new_hand[0:9], new_hand[9:18], new_hand[18:27]]:
                    for i in range(9):
                        while h[i] > 0:
                            if h[i] >= 3:
                                h[i] -= 3
                            elif i + 2 < 9 and h[i] > 0 and h[i + 1] > 0 and h[i + 2] > 0:
                                h[i] -= 1
                                h[i + 1] -= 1
                                h[i + 2] -= 1
                            else:
                                return False
                return all(h==3 or h==0 for h in new_hand[27:])

    @staticmethod
    def hand_to_chunk(hand:List[int]):
        return [hand[0:9], hand[9:18], hand[18:27], hand[27:]]

    @staticmethod
    def hand_coord_to_chunk(hand_coord:int) -> (int, int):
        return hand_coord // 9, hand_coord % 9

    def next(self, players: List[Player], event=None):
        event_pid = -1
        if event is not None:
            event_type, event_discard, event_drew, event_pid, *_ = event
            # Resolve new current state
            self.hands[event_pid][event_drew] += 1
            self.hands[event_pid][event_discard] -= 1
            self.trash[event_discard] += 1

            # Discard trigger
            # win trigger
            for hand_player_id, hand in enumerate(self.hands):
                if hand_player_id != event_pid:
                    new_hand = hand[:]
                    new_hand[event_discard] += 1
                    if State.is_win(new_hand) and players[hand_player_id].on_win(self):
                        return "win"
            # pong trigger
            for hand_player_id, hand in enumerate(self.hands):
                if hand_player_id != event_pid:
                    if hand[event_discard] >= 2:
                        on_pong_discard = players[hand_player_id].on_pong(self, (pong, [], event_discard, hand_player_id))
                        if on_pong_discard:
                            assert self.hands[hand_player_id][on_pong_discard] > 0
                            return pong, on_pong_discard, event_discard, hand_player_id
            # chow trigger
            hand = self.hands[(event_pid + 1) % 4]
            a, b = State.hand_coord_to_chunk(event_discard)
            if a in {0, 1, 2}:
                chunk = State.hand_to_chunk(hand)
                spree = 0
                for convolution in [-2, -1, 0, 1, 2]:
                    if (b + convolution > 0 and b + convolution < 9 and chunk[a][b + convolution] > 0) or convolution == 0:
                        spree += 1
                        if spree == 3:
                            on_chow_discard = players[(event_pid + 1) % 4].on_chow(self, (chow, [], event_discard, (event_pid + 1) % 4))
                            if on_chow_discard:
                                assert self.hands[(event_pid + 1) % 4][on_chow_discard] > 0
                                return chow, on_chow_discard, event_discard, (event_pid + 1) % 4

        drew = self.draw()
        discarded = players[(event_pid + 1) % 4].on_draw(self, (draw, [], drew, (event_pid + 1) % 4))
        assert self.hands[(event_pid + 1) % 4][discarded] > 0 or discarded == drew
        return draw, discarded, drew, (event_pid + 1) % 4

if __name__ == '__main__':
    state=State()
    event=None
    players=[Human()]+[RandomP()]*3
    while True:
        event=state.next(players, event)
        for player in players:
            player.on_event(state, event)

