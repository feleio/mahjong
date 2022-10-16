package io.fele.app.mahjong

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.ChowPosition.ChowPosition

import scala.io.StdIn.readLine

import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}

/**
  * Created by felix.ling on 01/01/2017.
  */
trait GameLogger {
  def start()
  def resume()
  def discard(discardEvent: DiscardEvent)
  def kong(kongEvent: KongEvent)
  def pong(pongEvent: PongEvent)
  def chow(chowEvent: ChowEvent)
  def end(endEvent: EndEvent)
  def draw(drawEvent: DrawEvent)
}

trait Event

case class StartEvent() extends Event
case class ResumeEvent() extends Event
case class DiscardEvent(playerId: Int, tile: Tile) extends Event
case class KongEvent(playerId: Int, sourcePlayerId: Int, tile: Tile) extends Event
case class PongEvent(playerId: Int, sourcePlayerId: Int, tile: Tile) extends Event
case class ChowEvent(playerId: Int, sourcePlayerId: Int, tile: Tile, position: ChowPosition) extends Event
case class EndEvent(winnersInfo: Option[WinnersInfo]) extends Event
case class DrawEvent(playerId: Int, tile: Tile) extends Event

case class GameSnapShot(event: Event, gameState: GameState)

class DummyGameLogger extends GameLogger {
  override def start(): Unit = ()
  override def resume(): Unit = ()
  override def discard(discardEvent: DiscardEvent): Unit = ()
  override def kong(kongEvent: KongEvent): Unit = ()
  override def pong(pongEvent: PongEvent): Unit = ()
  override def chow(chowEvent: ChowEvent): Unit = ()
  override def end(endEvent: EndEvent): Unit = ()
  override def draw(drawEvent: DrawEvent): Unit = ()
}

class DebugGameLogger(val gameState: GameState, val visibleToPlayerID: Option[Int] = None)(implicit val config: Config) extends GameLogger {
  val logger = Logger("DebugGameLogger")

  private def logCurStates(msg: String) = {
    var playerInfo = ""
    gameState.players.foreach(
      p => {
        val curMark = if (gameState.curPlayerId == p.id) " **** " + msg + " **** " else ""
        val playerInfoStr = visibleToPlayerID match {
          case Some(id) if id != p.id => s"fixed: ${p.publicInfo.tileGroups.mkString(" ")}\n"
          case _ => s"tiles: ${p.privateInfo.tiles.sortBy(t => t.toTileValue).mkString(" ")}\n" +
            s"fixed: ${p.privateInfo.tileGroups.mkString(" ")}\n"
        }
        playerInfo += s"#### Player ${p.id}$curMark:\n$playerInfoStr\n"
      }
    )

    logger.info("\n" + playerInfo +
      s"#### discards: ${gameState.discards.mkString(", ")}\n" +
      s"#### remaining : ${gameState.drawer.remainingTiles.size}" )
    //logger.info(s"drawer tiles: ${gameState.drawer.drawerState.shuffledTiles}")
    //logger.info(s"drawer curPos: ${gameState.drawer.drawerState.curPos}")
  }

  private def logAndPause(playerId: Option[Int], msg: String): Unit = {
    logCurStates(msg)
    logger.info(s"***** ${if(playerId.isDefined) "player " + playerId.get else ""} $msg *****\n")
    if (config.isPauseWhenLog) readLine()
  }

  def start() = logAndPause(None, "Game Start.")
  def resume() = logAndPause(None, "Game Resume.")
  def discard(discardEvent: DiscardEvent) = logAndPause(Some(discardEvent.playerId), s"> discarded ${discardEvent.tile.toString}")
  def kong(kongEvent: KongEvent) = logAndPause(Some(kongEvent.playerId), s"< kong with ${kongEvent.tile.toString}")
  def pong(pongEvent: PongEvent) = logAndPause(Some(pongEvent.playerId), s"< pong with ${pongEvent.tile.toString}")
  def chow(chowEvent: ChowEvent) = logAndPause(Some(chowEvent.playerId), s"< chow with ${chowEvent.tile.toString} in position ${chowEvent.position}")
  def end(endEvent: EndEvent) = endEvent.winnersInfo match {
    case None => logAndPause(None, s"no player wins")
    case Some(info) => logAndPause(None, s"player ${info.winners.mkString(", ")} < " +
      s"${if(info.isSelfWin) "draws and self wins " else "wins"}" +
      s" with ${info.winningTile.toString}\n" +
      s"winners info:\n"
      + info.winners.map(winner => {
        val p = gameState.players(winner.id)
        s"Player ${winner.id} :" +
          s"fixed: ${p.privateInfo.tileGroups.mkString(" ")}\n" +
          s"tiles: ${p.privateInfo.tiles.sortBy(t => t.toTileValue).mkString(" ")}\n" +
          s"score: ${winner.score}\n"
      }).mkString
    )
  }
  def draw(drawEvent: DrawEvent) = logAndPause(Some(drawEvent.playerId), s"< draws ${
    visibleToPlayerID match {
      case Some(id) if id != drawEvent.playerId => "a tile"
      case _ => drawEvent.tile
    }
  }")
}
