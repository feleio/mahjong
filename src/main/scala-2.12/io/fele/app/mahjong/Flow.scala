package io.fele.app.mahjong

import java.util.Random

import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.DrawResult._

/**
  * Created by felix.ling on 12/12/2016.
  */

object DiscardReason extends Enumeration {
  type DiscardReason = Value
  val FIRST_DRAW, DRAW, SELF_KONG, KONG, PONG, CHOW = Value
}
import DiscardReason._

case class GameResult(winners: Set[Int])

class Flow {
  // public states
  val discards: List[Tile] = List.empty[Tile]

  // private states
  val players: List[Player] = List.fill(4)(new DummyPlayer(drawer.popHand()))

  // tool
  val drawer = new RandomTileDrawer()

  // flow
  var curPlayerIdx: Int = (new Random).nextInt(4)
  var discardReason: DiscardReason = FIRST_DRAW

  var winners = Set.empty[Int]

  //private def nextPlayer(): Unit = curPlayerIdx = curPlayerIdx + 1 % 4
  private def curPlayer(): Player = players(curPlayerIdx)
  private def nextPlayer(): Player = players(curPlayerIdx + 1 % 4)
  private def nextPlayerIdx(): Int = curPlayerIdx + 1 % 4
  private def otherPlayers(): List[(Int, Player)] = {
    (1 to 3).map(_ + curPlayerIdx % 4).map(x => (x, players(x))).toList
  }

  private def askToDiscard(player: Player): Tile = TileValue.BAMBOO_1

  private def checkPlayersTile(f: ((Int, Player)) => Boolean): Set[Int] = {
    otherPlayers().filter(f).map(_._1).toSet
  }

  private object WiningTile {
    def unapply(tile: Tile): Option[Set[Int]] = {
      val ws = checkPlayersTile{
        case (i: Int, p: Player) => {p.canWin(tile) && p.isWin(tile, false)}
      }
      if (ws.isEmpty) None else Some(ws)
    }
  }

  private object KongableTile {
    def unapply(tile: Tile): Option[Int] = {
      checkPlayersTile{
        case (i: Int, p: Player) => {p.canKong(tile) && p.isKong(tile)}
      }.headOption
    }
  }

  private object PongableTile {
    def unapply(tile: Tile): Option[Int] = {
      checkPlayersTile{
        case (i: Int, p: Player) => {p.canPong(tile) && p.isPong(tile)}
      }.headOption
    }
  }

  private object ChowableTile {
    def unapply(tile: Tile): Option[(Int, ChowPosition)] = {
      val canChowPositions = nextPlayer().canChow(tile)
      if (canChowPositions.nonEmpty) {
        val chowPosition = nextPlayer().isChow(tile, canChowPositions)
        if (chowPosition.isDefined)
          Some((nextPlayerIdx(), chowPosition.get))
        else
          None
      }
      else
        None
    }
  }

  def start(): GameResult = {
    // TODO: check if kong at the first place is allowed?
    var discardedTile = curPlayer().draw(drawer) match {
      case (DISCARD, discarded: Option[Tile]) => discarded
      case (WIN, _) => winners = Set(curPlayerIdx); None
    }

    while (discardedTile.isDefined) {
      discardedTile = discardedTile.get match {
        case WiningTile(playerIds) => winners = playerIds; None
        case KongableTile(playerId) => {
          curPlayerIdx = playerId
          curPlayer().kong(discardedTile.get, drawer) match {
            case (DISCARD, discarded: Option[Tile]) => discarded
            case (WIN, _) => winners = Set(playerId); None
            case (NO_TILE, _) => None
          }
        }
        case PongableTile(playerId) => {
          curPlayerIdx = playerId
          Some(curPlayer().pong(discardedTile.get))
        }
        case ChowableTile(playerId, chowPosition) => {
          curPlayerIdx = playerId
          Some(curPlayer().chow(discardedTile.get, chowPosition))
        }
        case _ => {
          curPlayerIdx = nextPlayerIdx()
          curPlayer().draw(drawer) match {
            case (DISCARD, discarded: Option[Tile]) => discarded
            case (WIN, _) => winners = Set(curPlayerIdx); None
            case (NO_TILE, _) => None
          }
        }
      }
    }

    GameResult(winners)
  }
}
