from abc import ABC, abstractclassmethod
from typing import List
from random import choices
from threading import Event

from src.main.python.tile import symbol

class Player(ABC):
    @abstractclassmethod
    def on_draw(self, state, event: (int, List[int], int, int)) -> int:
        pass

    @abstractclassmethod
    def on_win(self, state) -> bool:
        pass

    @abstractclassmethod
    def on_pong(self, state, event: (int, List[int], int, int)) -> int:
        """ (is_pong, discard) """
        pass

    @abstractclassmethod
    def on_chow(self, state, event: (int, List[int], int, int)) -> int:
        pass

    @abstractclassmethod
    def on_kong(self, state, event: (int, List[int], int, int)) -> int:
        pass

    @abstractclassmethod
    def on_event(self, state, event: (int, List[int], int, int)) -> None:
        pass


class Human(Player):

    def on_draw(self, state, event: (int, List[int], int, int)) -> int:
        input_str = input(f"on_draw{state.symbol(event[3])} *{symbol[event[2]]}")
        if input_str:
            return int(symbol.index(input_str))
        return event[2]

    def on_win(self, state) -> bool:
        return bool(input(f"on_win{state.symbol()}"))

    def on_pong(self, state, event: (int, List[int], int, int)) -> int:
        input_str = input(f"on_pong{state.symbol(event[3])} !{symbol[event[2]]}")
        if input_str:
            return int(symbol.index(input_str))
        return None

    def on_chow(self, state, event: (int, List[int], int, int)) -> int:
        input_str = input(f"on_chow{state.symbol(event[3])} !{symbol[event[2]]}")
        if input_str:
            return int(symbol.index(input_str))
        return None

    def on_kong(self, state, event: (int, List[int], int, int)) -> int:
        return int(symbol.index(input(f"on_kong{state.symbol(event[3])} !{symbol[event[2]]}")))

    def on_event(self, state, event: (int, List[int], int, int)) -> None:
        print(f"on_event{event} {symbol[event[1]]}")


class Step(Player):
    def __init__(self):
        self.thread_event = Event()
        self.event_log = []

    def on_draw(self, state, event: (int, List[int], int, int)) -> int:
        state
        input_str = input(f"on_draw{state.symbol(event[3])} *{symbol[event[2]]}")
        if input_str:
            return int(symbol.index(input_str))
        return event[2]

    def on_win(self, state) -> bool:
        return bool(input(f"on_win{state.symbol()}"))

    def on_pong(self, state, event: (int, List[int], int, int)) -> int:
        input_str = input(f"on_pong{state.symbol(event[3])} !{symbol[event[2]]}")
        if input_str:
            return int(symbol.index(input_str))
        return None

    def on_chow(self, state, event: (int, List[int], int, int)) -> int:
        input_str = input(f"on_chow{state.symbol(event[3])} !{symbol[event[2]]}")
        if input_str:
            return int(symbol.index(input_str))
        return None

    def on_kong(self, state, event: (int, List[int], int, int)) -> int:
        return int(symbol.index(input(f"on_kong{state.symbol(event[3])} !{symbol[event[2]]}")))

    def on_event(self, state, event: (int, List[int], int, int)) -> None:
        print(f"on_event{event} {symbol[event[1]]}")


class RandomP(Player):

    def on_draw(self, state, event: (int, List[int], int, int)) -> int:
        return choices(range(len(symbol)), state.hands[event[3]])[0]

    def on_win(self, state) -> bool:
        return True

    def on_pong(self, state, event: (int, List[int], int, int)) -> int:
        return choices(range(len(symbol)), state.hands[event[3]])[0]

    def on_chow(self, state, event: (int, List[int], int, int)) -> int:
        return choices(range(len(symbol)), state.hands[event[3]])[0]

    def on_kong(self, state, event: (int, List[int], int, int)) -> int:
        return choices(range(len(symbol)), state.hands[event[3]])[0]

    def on_event(self, state, event: (int, List[int], int, int)) -> None:
        pass

