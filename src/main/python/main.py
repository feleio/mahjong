import random
from src.main.python.tile import tiles

SET_NUM = 9*3+4+3


class State:
    def __init__(self):
        self.hands = [[0 for _ in range(SET_NUM)] for _ in range(4)]
        self.walls = [4 for _ in range(SET_NUM)]
        for hand_idx in range(4):
            for _ in range(13):
                self.hands[hand_idx][self.deal()] += 1

    def __repr__(self):
        return "|".join("".join(map(str, hand)) for hand in self.hands)

    def pretty(self):
        return "|".join("".join(tiles[set_idx]*h for set_idx, h in enumerate(hand)) for hand in self.hands)

    def deal(self):
        if any(self.walls):
            picked = random.choices(range(SET_NUM), self.walls)[0]
            self.walls[picked] -= 1
            return picked
        else:
            raise StopIteration()

