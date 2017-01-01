package io.fele.app.mahjong

import com.typesafe.scalalogging.Logger

/**
  * Created by felix.ling on 01/01/2017.
  */
class GameLogger(val gameState: GameState){
  val logger = Logger("EventLogger")

  private def logCurStates() = {
    gameState.players.zipWithIndex.foreach(p => logger.debug(s"Player ${p._2}:\n${p._1.toString}\n"))
    logger.debug(s"discards: ${gameState.discards.mkString(", ")}\n")
  }

  private def logAndPause(msg: String) = {
    logger.debug(msg + "\n")
    logCurStates()
  }

  def start() = logAndPause("Game Start.")
  def discard(playerIdx: Int, tile: Tile) = logAndPause(s"player $playerIdx discarded ${tile.toString}")
}
