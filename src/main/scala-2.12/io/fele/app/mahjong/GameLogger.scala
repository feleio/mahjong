package io.fele.app.mahjong

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.ChowPosition.ChowPosition

import scala.io.StdIn.readLine

/**
  * Created by felix.ling on 01/01/2017.
  */
trait
GameLogger {
  def start()
  def resume()
  def discard(playerId: Int, tile: Tile)
  def kong(playerId: Int, tile: Tile)
  def pong(playerId: Int, tile: Tile)
  def chow(playerId: Int, tile: Tile, position: ChowPosition)
  def end(playerIds: Set[Int], winningTile: Option[Tile])
  def draw(playerId: Int, tile: Tile)
}

class DebugGameLogger(val gameState: GameState)(implicit val config: Config) extends GameLogger {
  val logger = Logger("EventLogger")

  private def logCurStates() = {
    gameState.players.foreach(
      p => {
        val curMark = if (gameState.getCurPlayerId == p.id) " ****" else ""
        logger.debug(s"#### Player ${p.id}$curMark:\n${p.toString}\n")
      }
    )
    logger.debug(s"discards: ${gameState.discards.mkString(", ")}\n")
    logger.debug(s"drawer tiles: ${gameState.drawer.drawerState.shuffledTiles}")
    logger.debug(s"drawer curPos: ${gameState.drawer.drawerState.curPos}")
  }

  private def logAndPause(msg: String): Unit = {
    logger.debug("***** " + msg + " *****\n")
    logCurStates()
    if (config.isPauseWhenLog) readLine()
  }

  def start() = logAndPause("Game Start.")
  def resume() = logAndPause("Game Resume.")
  def discard(playerId: Int, tile: Tile) = logAndPause(s"player $playerId > discarded ${tile.toString}")
  def kong(playerId: Int, tile: Tile) = logAndPause(s"player $playerId < kong with ${tile.toString}")
  def pong(playerId: Int, tile: Tile) = logAndPause(s"player $playerId < pong with ${tile.toString}")
  def chow(playerId: Int, tile: Tile, position: ChowPosition) = logAndPause(s"player $playerId < chow with ${tile.toString} in position $position")
  def end(playerIds: Set[Int], winningTile: Option[Tile]) = playerIds.size match {
    case 0 => logAndPause(s"no player wins")
    case _ => logAndPause(s"player ${playerIds.mkString(", ")} < wins with ${winningTile.get.toString}")
  }
  def draw(playerId: Int, tile: Tile) = logAndPause(s"player $playerId < draws ${tile.toString}")
}
