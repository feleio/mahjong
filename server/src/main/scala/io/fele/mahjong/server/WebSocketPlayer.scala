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
  timeoutMs:  Long = WebSocketPlayer.DefaultTimeoutMs
)(implicit c: Config) extends Player(id, tiles, tileGroups)(c) {

  private val mailbox = new LinkedBlockingQueue[WebSocketPlayer.ClientAction](1)
  override def name: String = "Human"

  /** Called by the websocket route when a client sends an action message. */
  def submit(action: WebSocketPlayer.ClientAction): Unit = {
    mailbox.clear()
    mailbox.offer(action)
  }

  /** Wait for the client's action. A timeout means the engine is about to act
    * on this player's behalf, which is NOT a decision the human made — tell
    * the sink so it can be recorded as such (issue #42) and shown in the UI,
    * instead of silently entering the record as a real choice. */
  private def waitFor(kind: String): Option[WebSocketPlayer.ClientAction] =
    Option(mailbox.poll(timeoutMs, TimeUnit.MILLISECONDS)) match {
      case some @ Some(_) => some
      case None           => promptSink.timedOut(id, kind); None
    }

  override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean = {
    promptSink.selfWin(id, tile, score, curState)
    waitFor("self_win").flatMap(_.yes).getOrElse(true)
  }

  override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean = {
    promptSink.win(id, tile, score, curState)
    waitFor("win").flatMap(_.yes).getOrElse(false)
  }

  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = {
    promptSink.selfKong(id, selfKongTiles, curState)
    waitFor("self_kong").flatMap(_.tile).filter(selfKongTiles.contains)
  }

  override def decideKong(tile: Tile, curState: CurState): Boolean = {
    promptSink.kong(id, tile, curState)
    waitFor("kong").flatMap(_.yes).getOrElse(false)
  }

  override def decidePong(tile: Tile, curState: CurState): Boolean = {
    promptSink.pong(id, tile, curState)
    waitFor("pong").flatMap(_.yes).getOrElse(false)
  }

  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = {
    promptSink.chow(id, tile, positions, curState)
    waitFor("chow").flatMap(_.chowPos).filter(positions.contains)
  }

  override def decideDiscard(curState: CurState): Tile = {
    promptSink.discard(id, curState)
    waitFor("discard").flatMap(_.tile)
      .filter(t => hand.dynamicTiles.contains(t))
      .getOrElse(hand.dynamicTiles.head)
  }
}

object WebSocketPlayer {
  /** How long a human seat has to answer before the engine plays a default. */
  val DefaultTimeoutMs: Long = 60000L

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

    /** A prompt of `kind` for `seat` expired; the engine is about to play a
      * default on the player's behalf. */
    def timedOut(seat: Int, kind: String): Unit
  }
}
