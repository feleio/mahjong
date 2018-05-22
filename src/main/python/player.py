from abc import ABC, abstractclassmethod
from src.main.python.tile import symbol

class Player(ABC):
    @abstractclassmethod
    def on_draw(self, state, drew) -> int:
        pass

    @abstractclassmethod
    def on_win(self, state) -> bool:
        pass

    @abstractclassmethod
    def on_pong(self, state) -> bool:
        """ (is_pong, discard) """
        pass

    @abstractclassmethod
    def on_chow(self, state) -> int:
        pass

    @abstractclassmethod
    def gon(self):
        pass

class Human(Player):

    def on_draw(self, state, event) -> int:
        return int(symbol.index(input(f"on_draw{state.symbol()}{event}")))

    def on_win(self, state) -> bool:
        return bool(input(f"on_win{state}"))

    def on_pong(self, state) -> bool:
        """ (is_pong, discard) """
        return bool(input(f"on_draw{state}"))

    def on_chow(self, state) -> int:
        return bool(input(f"on_draw{state}"))

    def gon(self):
        return bool(input(f"on_draw{state}"))