package io.fele.mahjong.server

import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.player.Player
import io.fele.app.mahjong.{ChowPosition => CP, _}

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

/**
 * A Player whose decisions arrive over a websocket. Each `decide*` method
 * blocks (on the engine's worker thread) waiting for the matching action to
 * be supplied via `submit`. If the websocket disconnects or no action arrives
 * within `timeout`, we fall back to a sensible default so the game does not
 * stall.
 */
class WebSocketPlayer(
  id:         Int,
  tiles:      List[Tile],
  tileGroups: List[TileGroup],
  promptSink: WebSocketPlayer.PromptSink,
  timeoutMs:  Long = 60000L
)(implicit c: Config) extends Player(id, tiles, tileGroups)(c) {

  private val mailbox = new LinkedBlockingQueue[WebSocketPlayer.ClientAction](1)
  override def name: String = "Human"

  /** Called by the websocket route when a client sends an action message. */
  def submit(action: WebSocketPlayer.ClientAction): Unit = {
    mailbox.clear()
    mailbox.offer(action)
  }

  private def waitFor(): Option[WebSocketPlayer.ClientAction] =
    Option(mailbox.poll(timeoutMs, TimeUnit.MILLISECONDS))

  override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean = {
    promptSink.selfWin(id, tile, score, curState)
    waitFor().flatMap(_.yes).getOrElse(true)
  }

  override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean = {
    promptSink.win(id, tile, score, curState)
    waitFor().flatMap(_.yes).getOrElse(false)
  }

  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = {
    promptSink.selfKong(id, selfKongTiles, curState)
    waitFor().flatMap(_.tile).filter(selfKongTiles.contains)
  }

  override def decideKong(tile: Tile, curState: CurState): Boolean = {
    promptSink.kong(id, tile, curState)
    waitFor().flatMap(_.yes).getOrElse(false)
  }

  override def decidePong(tile: Tile, curState: CurState): Boolean = {
    promptSink.pong(id, tile, curState)
    waitFor().flatMap(_.yes).getOrElse(false)
  }

  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = {
    promptSink.chow(id, tile, positions, curState)
    waitFor().flatMap(_.chowPos).filter(positions.contains)
  }

  override def decideDiscard(curState: CurState): Tile = {
    promptSink.discard(id, curState)
    waitFor().flatMap(_.tile)
      .filter(t => hand.dynamicTiles.contains(t))
      .getOrElse(hand.dynamicTiles.head)
  }
}

object WebSocketPlayer {
  /** Decoded action submitted from the client. */
  case class ClientAction(
    yes:     Option[Boolean],
    tile:    Option[Tile],
    chowPos: Option[ChowPosition]
  )

  /** Hook used by the player to publish decision prompts to the room's event topic. */
  trait PromptSink {
    def selfWin(seat: Int, tile: Tile, score: Int, st: CurState): Unit
    def win(seat: Int, tile: Tile, score: Int, st: CurState): Unit
    def selfKong(seat: Int, tiles: Set[Tile], st: CurState): Unit
    def kong(seat: Int, tile: Tile, st: CurState): Unit
    def pong(seat: Int, tile: Tile, st: CurState): Unit
    def chow(seat: Int, tile: Tile, positions: Set[ChowPosition], st: CurState): Unit
    def discard(seat: Int, st: CurState): Unit
  }
}
