package io.fele.app.mahjong

import java.util.Random

import scala.io.StdIn.readLine

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.DrawResult._

/**
  * Created by felix.ling on 12/12/2016.
  */
case class GameResult(winners: Set[Int])
case class GameState(players: List[Player],
                     var winners: Set[Int],
                     var discards: List[Tile],
                     private var curPlayerId: Int){
  def addDiscarded(tile: Tile) = discards = tile :: discards
  def setCurPlayer(playerId: Int) = curPlayerId = playerId

  def curPlayer(): Player = players(curPlayerId)
  def getCurPlayerId(): Int = curPlayerId
  def nextPlayer(): Player = players((curPlayerId + 1) % 4)
  def getNextPlayerId(): Int = (curPlayerId + 1) % 4
  def otherPlayers(): List[(Int, Player)] = {
    (1 to 3).map(x => (x + curPlayerId) % 4).map(x => (x, players(x))).toList
  }
}

class Flow(seed: Long) {
  // logger
  val logger = Logger(classOf[Flow])

  // tool
  val drawer = new RandomTileDrawer(seed)

  // public states
  val state = GameState(
    (0 to 3).map(new DummyPlayer(_, drawer.popHand())).toList,
    Set.empty[Int],
    Nil,
    new Random(seed).nextInt(4) )

  implicit val gameLogger = new GameLogger(state)

  private def checkPlayersTile(f: ((Int, Player)) => Boolean): Set[Int] = {
    state.otherPlayers().filter(f).map(_._1).toSet
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
      val canChowPositions = state.nextPlayer().canChow(tile)
      if (canChowPositions.nonEmpty) {
        val chowPosition = state.nextPlayer().isChow(tile, canChowPositions)
        if (chowPosition.isDefined)
          Some((state.getNextPlayerId(), chowPosition.get))
        else
          None
      }
      else
        None
    }
  }

  def start(): GameResult = {
    gameLogger.start()

    // TODO: check if kong at the first place is allowed?
    var discardedTile = state.curPlayer().draw(drawer) match {
      case (DISCARD, discarded: Option[Tile]) => discarded
      case (WIN, _) => state.winners = Set(state.getCurPlayerId()); None
    }
    state.addDiscarded(discardedTile.get)

    while (discardedTile.isDefined) {
      gameLogger.discard(state.getCurPlayerId(), discardedTile.get)
      state.addDiscarded(discardedTile.get)

      discardedTile = discardedTile.get match {
        case WiningTile(playerIds) => state.winners = playerIds; None
        case KongableTile(playerId) => {
          state.setCurPlayer(playerId)
          state.curPlayer().kong(discardedTile.get, drawer) match {
            case (DISCARD, discarded: Option[Tile]) => discarded
            case (WIN, _) => state.winners = Set(playerId); None
            case (NO_TILE, _) => None
          }
        }
        case PongableTile(playerId) => {
          state.setCurPlayer(playerId)
          Some(state.curPlayer().pong(discardedTile.get))
        }
        case ChowableTile(playerId, chowPosition) => {
          state.setCurPlayer(playerId)
          Some(state.curPlayer().chow(discardedTile.get, chowPosition))
        }
        case _ => {
          state.setCurPlayer(state.getNextPlayerId())
          state.curPlayer().draw(drawer) match {
            case (DISCARD, discarded: Option[Tile]) => discarded
            case (WIN, _) => state.winners = Set(state.getCurPlayerId()); None
            case (NO_TILE, _) => None
          }
        }
      }
    }
    if (state.winners.nonEmpty)
      gameLogger.win(state.winners)
    else
      gameLogger.noOneWin()

    GameResult(state.winners)
  }
}

object Main extends App{
  val logger = Logger("main")
  val flow = new Flow(2)
  logger.debug(s"result${flow.start()}")
}
