from src.main.python.main import State
from src.main.python.player import Human
from mock import MagicMock

from src.main.python.tile import symbol

assert State.is_win([2, 0, 0, 0, 0, 0, 0, 0, 0] + ([0] * (34 - 9)))
assert not State.is_win([2, 1, 0, 0, 0, 0, 0, 0, 0] + ([0] * (34 - 9)))
assert State.is_win([2, 0, 0, 0, 0, 0, 0, 0, 3] + ([0] * (34 - 9)))
assert State.is_win([2, 0, 0, 0, 0, 0, 1, 1, 4] + ([0] * (34 - 9)))
assert not State.is_win([2, 0, 0, 0, 0, 0, 0, 1, 4, 1] + ([0] * (34 - 9 - 1)))

assert not State.is_win(([0] * (34 - 4)) + [2, 1, 1, 1])
assert State.is_win(([0] * (34 - 4)) + [2, 0, 0, 3])
