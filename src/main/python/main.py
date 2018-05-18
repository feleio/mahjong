import random
from typing import List

from src.main.python.tile import tiles
from src.main.python.player import Player

SET_NUM = 9*3+4+3


class State:
    def __init__(self):
        self.event = None
        self.hands = [[0 for _ in range(SET_NUM)] for _ in range(4)]
        self.docks = [[0 for _ in range(SET_NUM)] for _ in range(4)]
        self.walls = [4 for _ in range(SET_NUM)]
        self.trash = [0 for _ in range(SET_NUM)]
        self.player_turn = 0
        for hand_idx in range(4):
            for _ in range(13):
                self.hands[hand_idx][self.draw()] += 1

    def __repr__(self) -> str:
        return "|".join("".join(map(str, hand)) for hand in self.hands)

    def pretty(self) -> str:
        return "|".join("".join(tiles[set_idx]*h for set_idx, h in enumerate(hand)) for hand in self.hands)

    def draw(self) -> int:
        if any(self.walls):
            picked = random.choices(range(SET_NUM), self.walls)[0]
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

    def discard_trigger(self, discarded) -> str:
        # win trigger
        for hand_player_id, hand in enumerate(self.hands):
            if hand_player_id != self.player_turn:
                new_hand = hand[:]
                new_hand[discarded] += 1
                if State.is_win(new_hand) and players[hand_player_id].on_win(self):
                    return "win"
        # pong trigger
        for hand_player_id, hand in enumerate(self.hands):
            if hand_player_id != self.player_turn:
                if hand[discarded] >= 2 and players[hand_player_id].on_pong(self):
                    self.player_turn = hand_player_id
                    self.drew = discarded
                    return "pong"
        # chow trigger
        hand = self.hands[(self.player_turn+1)%4]
        a,b = State.hand_coord_to_chunk(discarded)
        if a in {0, 1, 2}:
            chunk = State.hand_to_chunk(hand)
            spree=0
            for convolution in [-2, -1, 0, 1, 2]:
                if (b + convolution > 0 and b + convolution < 9 and chunk[a][b - convolution] > 0) or convolution == 0:
                    spree+=1
                    if spree == 3 and players[self.player_turn].on_chow(self): # TODO assert
                        players[self.player_turn].on_draw(self)  # TODO change player turn discard
                        return "chow"

        # TODO kong trigger
        return None

    def next(self, players:List[Player]):
        if self.event is None:
            drew = self.draw()
            self.event = ('draw', drew)
            self.hands[self.player_turn][drew] += 1 # TODO separate next state?
        # TODO check win
        # TODO should we add to hand before checking?
        discarded = players[self.player_turn].on_draw(self, drew)
        assert self.hands[self.player_turn][discarded] > 0
        discard_action = self.discarded_trigger(discarded)
        if discard_action is None:
            self.trash[discarded]+=1
        elif discard_action == "pong":
            continue

