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
  def discard(playerId: Int, tile: Tile)
  def kong(playerId: Int, tile: Tile)
  def pong(playerId: Int, tile: Tile)
  def chow(playerId: Int, tile: Tile, position: ChowPosition)
  def win(playerIds: Set[Int], winningTile: Tile)
  def draw(playerId: Int, tile: Tile)
  def noOneWin()
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
  }

  private def logAndPause(msg: String): Unit = {
    logger.debug("***** " + msg + " *****\n")
    logCurStates()
    if (config.isPauseWhenLog) readLine()
  }

  def start() = logAndPause("Game Start.")
  def discard(playerId: Int, tile: Tile) = logAndPause(s"player $playerId > discarded ${tile.toString}")
  def kong(playerId: Int, tile: Tile) = logAndPause(s"player $playerId < kong with ${tile.toString}")
  def pong(playerId: Int, tile: Tile) = logAndPause(s"player $playerId < pong with ${tile.toString}")
  def chow(playerId: Int, tile: Tile, position: ChowPosition) = logAndPause(s"player $playerId < chow with ${tile.toString} in position $position")
  def win(playerIds: Set[Int], winningTile: Tile) = logAndPause(s"player ${playerIds.mkString(", ")} < wins with ${winningTile.toString}")
  def draw(playerId: Int, tile: Tile) = logAndPause(s"player $playerId < draws ${tile.toString}")
  def noOneWin() = logAndPause(s"no player wins")
}
