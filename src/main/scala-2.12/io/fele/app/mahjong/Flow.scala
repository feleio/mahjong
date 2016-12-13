package io.fele.app.mahjong

import java.util.Random

/**
  * Created by felix.ling on 12/12/2016.
  */
class Flow {
  val drawer = new RandomTileDrawer()
  val discards: List[Tile] = List.empty[Tile]
  val players: List[Player] = List.fill(4)(new DummyPlayer(drawer.popHand()))
  var curPlayerIdx: Int = (new Random).nextInt(4)
  var winner: Option[Int] = None

  private def nextPlayer(): Unit = curPlayerIdx = curPlayerIdx + 1 % 4
  private def curPlayer(): Player = players(curPlayerIdx)
  private def otherPlayers(): List[Player] = {
    List(curPlayerIdx+1%4, curPlayerIdx+2%4, curPlayerIdx+3%4).map(x => (x, players(x)))
  }

  def start(): Unit = {
    var drawn: Option[Tile] = drawer.pop()
    while (drawn.isDefined || winner.isEmpty) {
      if (curPlayer().hand.canWin(drawn.get) && curPlayer().isWin(drawn.get))
        winner = Some(curPlayerIdx)
      else {
        while (curPlayer().hand.canKong(drawn.get) && curPlayer().isKong(drawn.get)) {
          curPlayer().hand.kong(drawn.get)
          drawn = drawer.pop()
        }
        var discardTile = curPlayer().pickToDiscard()
        curPlayer().hand.discard(discardTile)

        // ask others
        otherPlayers().foreach {
          case (i, p) if curPlayer().hand.canWin(discardTile) && curPlayer().isWin(discardTile) => winner = Some(i)
          case (i, p) if curPlayer().hand.canKong(discardTile) && curPlayer().isKong(discardTile) => {
          }
        }
      }
    }
  }
}
