package io.fele.app.mahjong

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.ChowPosition.ChowPosition

import scala.io.StdIn.readLine

/**
  * Created by felix.ling on 01/01/2017.
  */
trait GameLogger {
  def start()
  def resume()
  def discard(playerId: Int, tile: Tile)
  def kong(playerId: Int, tile: Tile)
  def pong(playerId: Int, tile: Tile)
  def chow(playerId: Int, tile: Tile, position: ChowPosition)
  def end(winnersInfo: Option[WinnersInfo])
  def draw(playerId: Int, tile: Tile)
}

class DummyGameLogger extends GameLogger {
  override def start(): Unit = ()
  override def resume(): Unit = ()
  override def discard(playerId: Int, tile: Tile): Unit = ()
  override def kong(playerId: Int, tile: Tile): Unit = ()
  override def pong(playerId: Int, tile: Tile): Unit = ()
  override def chow(playerId: Int, tile: Tile, position: ChowPosition): Unit = ()
  override def end(winnersInfo: Option[WinnersInfo]): Unit = ()
  override def draw(playerId: Int, tile: Tile): Unit = ()
}

class DebugGameLogger(val gameState: GameState, val visibleToPlayerID: Option[Int] = None)(implicit val config: Config) extends GameLogger {
  val logger = Logger("EventLogger")

  private def logCurStates(msg: String) = {
    var playerInfo = ""
    gameState.players.foreach(
      p => {
        val curMark = if (gameState.curPlayerId == p.id) " **** " + msg + " **** " else ""
        val playerInfoStr = visibleToPlayerID match {
          case Some(id) if id != p.id => s"fixed: ${p.publicInfo.tileGroups.mkString(" ")}\n"
          case _ => s"tiles: ${p.privateInfo.tiles.sortBy(t => t.value.id).mkString(" ")}\n" +
            s"fixed: ${p.privateInfo.tileGroups.mkString(" ")}\n"
        }
        playerInfo += s"#### Player ${p.id}$curMark:\n$playerInfoStr\n"
      }
    )

    logger.info("\n" + playerInfo +
      s"#### discards: ${gameState.discards.mkString(", ")}\n" +
      s"#### remaining : ${gameState.drawer.remainingTileNum}" )
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
  def discard(playerId: Int, tile: Tile) = logAndPause(Some(playerId), s"> discarded ${tile.toString}")
  def kong(playerId: Int, tile: Tile) = logAndPause(Some(playerId), s"< kong with ${tile.toString}")
  def pong(playerId: Int, tile: Tile) = logAndPause(Some(playerId), s"< pong with ${tile.toString}")
  def chow(playerId: Int, tile: Tile, position: ChowPosition) = logAndPause(Some(playerId), s"< chow with ${tile.toString} in position $position")
  def end(winnersInfo: Option[WinnersInfo]) = winnersInfo match {
    case None => logAndPause(None, s"no player wins")
    case Some(info) => logAndPause(None, s"player ${info.winners.mkString(", ")} < " +
      s"${if(info.isSelfWin) "draws and self wins " else "wins"}" +
      s" with ${info.winningTile.toString}\n" +
      s"winners info:\n"
      + info.winners.map(id => {
        val p = gameState.players(id)
        s"Player $id :" +
          s"fixed: ${p.privateInfo.tileGroups.mkString(" ")}\n" +
          s"tiles: ${p.privateInfo.tiles.sortBy(t => t.value.id).mkString(" ")}\n"
      }).mkString
    )
  }
  def draw(playerId: Int, tile: Tile) = logAndPause(Some(playerId), s"< draws ${
    visibleToPlayerID match {
      case Some(id) if id != playerId => "a tile"
      case _ => tile
    }
  }")
}
