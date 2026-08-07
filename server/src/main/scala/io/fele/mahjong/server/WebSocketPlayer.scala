package io.fele.mahjong.server

import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.player.Player
import io.fele.app.mahjong.{ChowPosition => CP, _}

import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

/**
 * A Player whose decisions arrive over a websocket. Each `decide*` method
 * blocks (on the engine's worker thread) waiting for the matching action to
 * be supplied via `submit`. If the websocket disconnects or no action arrives
 * within `timeout`, we fall back to a sensible default so the game does not
 * stall.
 *
 * Every decision carries a monotonic id. An answer is only accepted for the
 * decision it was written for: without that, an answer that arrives after its
 * prompt expired (or a reply to a prompt the client was re-sent on reconnect)
 * sits in the mailbox and is applied instantly to the *next* decision — which
 * both plays a tile the player chose a minute earlier for a different board
 * and records that move as a deliberate choice, defeating the timeout
 * accounting in issue #42.
 */
class WebSocketPlayer(
  id:         Int,
  tiles:      List[Tile],
  tileGroups: List[TileGroup],
  promptSink: WebSocketPlayer.PromptSink,
  timeoutMs:  Long = WebSocketPlayer.DefaultTimeoutMs
)(implicit c: Config) extends Player(id, tiles, tileGroups)(c) {

  private val mailbox    = new LinkedBlockingQueue[(Long, WebSocketPlayer.ClientAction)](1)
  private val promptSeq  = new AtomicLong(0)
  override def name: String = "Human"

  /** Id of the decision currently awaiting an answer (0 = none yet). */
  def currentPromptId: Long = promptSeq.get()

  /** Called by the websocket route when a client sends an action message.
    * `promptId` identifies which decision the client was answering; an answer
    * for any other decision is dropped. Actions without an id are accepted for
    * the current decision, so hand-written clients still work. */
  def submit(action: WebSocketPlayer.ClientAction, promptId: Option[Long] = None): Unit = {
    val current = promptSeq.get()
    if (promptId.forall(_ == current)) {
      mailbox.clear()
      mailbox.offer((current, action))
    }
  }

  /** Open a decision: take the next id and drop anything the mailbox still
    * holds, since it can only be an answer to an earlier decision. */
  private def begin(): Long = {
    val gen = promptSeq.incrementAndGet()
    mailbox.clear()
    gen
  }

  /** Wait for this decision's answer, ignoring answers stamped for others.
    * On expiry the id is retired so a straggler cannot be mistaken for the
    * next decision's answer, and the sink is told the engine is stepping in. */
  private def waitFor(kind: String, gen: Long): Option[WebSocketPlayer.ClientAction] = {
    val deadline = System.currentTimeMillis() + timeoutMs
    var answer: Option[WebSocketPlayer.ClientAction] = None
    var waiting  = true
    while (waiting) {
      val remaining = deadline - System.currentTimeMillis()
      if (remaining <= 0) waiting = false
      else Option(mailbox.poll(remaining, TimeUnit.MILLISECONDS)) match {
        case Some((g, a)) if g == gen => answer = Some(a); waiting = false
        case Some(_)                  => ()          // an older decision's answer; keep waiting
        case None                     => waiting = false
      }
    }
    if (answer.isEmpty) {
      promptSeq.incrementAndGet()
      mailbox.clear()
      promptSink.timedOut(id, kind)
    }
    answer
  }

  override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean = {
    val gen = begin()
    promptSink.selfWin(id, tile, score, curState, gen)
    waitFor("self_win", gen).flatMap(_.yes).getOrElse(true)
  }

  override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean = {
    val gen = begin()
    promptSink.win(id, tile, score, curState, gen)
    waitFor("win", gen).flatMap(_.yes).getOrElse(false)
  }

  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = {
    val gen = begin()
    promptSink.selfKong(id, selfKongTiles, curState, gen)
    waitFor("self_kong", gen).flatMap(_.tile).filter(selfKongTiles.contains)
  }

  override def decideKong(tile: Tile, curState: CurState): Boolean = {
    val gen = begin()
    promptSink.kong(id, tile, curState, gen)
    waitFor("kong", gen).flatMap(_.yes).getOrElse(false)
  }

  override def decidePong(tile: Tile, curState: CurState): Boolean = {
    val gen = begin()
    promptSink.pong(id, tile, curState, gen)
    waitFor("pong", gen).flatMap(_.yes).getOrElse(false)
  }

  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = {
    val gen = begin()
    promptSink.chow(id, tile, positions, curState, gen)
    waitFor("chow", gen).flatMap(_.chowPos).filter(positions.contains)
  }

  override def decideDiscard(curState: CurState): Tile = {
    val gen = begin()
    promptSink.discard(id, curState, gen)
    waitFor("discard", gen).flatMap(_.tile)
      .filter(t => hand.dynamicTiles.contains(t))
      .getOrElse(hand.dynamicTiles.head)
  }
}

object WebSocketPlayer {
  /** How long a human seat has to answer before the engine plays a default. */
  val DefaultTimeoutMs: Long = 60000L

  /** Decoded action submitted from the client. */
  case class ClientAction(
    yes:      Option[Boolean],
    tile:     Option[Tile],
    chowPos:  Option[ChowPosition],
    promptId: Option[Long] = None
  )

  /** Hook used by the player to publish decision prompts to the room's event
    * topic. `promptId` identifies the decision so the client can echo it back. */
  trait PromptSink {
    def selfWin(seat: Int, tile: Tile, score: Int, st: CurState, promptId: Long): Unit
    def win(seat: Int, tile: Tile, score: Int, st: CurState, promptId: Long): Unit
    def selfKong(seat: Int, tiles: Set[Tile], st: CurState, promptId: Long): Unit
    def kong(seat: Int, tile: Tile, st: CurState, promptId: Long): Unit
    def pong(seat: Int, tile: Tile, st: CurState, promptId: Long): Unit
    def chow(seat: Int, tile: Tile, positions: Set[ChowPosition], st: CurState, promptId: Long): Unit
    def discard(seat: Int, st: CurState, promptId: Long): Unit

    /** A prompt of `kind` for `seat` expired; the engine is about to play a
      * default on the player's behalf. */
    def timedOut(seat: Int, kind: String): Unit
  }
}
