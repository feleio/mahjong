from abc import ABC, abstractclassmethod

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