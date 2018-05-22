from src.main.python.main import State
from src.main.python.player import Human
from mock import MagicMock

from src.main.python.tile import symbol

state = State()
state.hands = [
    [1] * 9 + [0] * (34 - 9),
    [2, 0, 0, 2, 0, 0, 0, 4, 4] + ([0] * (34 - 9)),
    [0]*34,
    [0, 2, 0, 2, 0, 0, 0, 4, 4] + ([0] * (34 - 9)),
]

mock_humans = [MagicMock(), MagicMock(), MagicMock(), MagicMock()]
mock_humans[0].on_draw.return_value = symbol.index('A0')
mock_humans[1].on_pong.return_value = symbol.index('A1')
mock_humans[3].on_pong.return_value = symbol.index('A3')

event=state.next(mock_humans)

mock_humans[0].on_draw.assert_called_once()

event=state.next(mock_humans, event)

mock_humans[1].on_pong.assert_called_once()

event=state.next(mock_humans, event)

mock_humans[3].on_pong.assert_called_once()

