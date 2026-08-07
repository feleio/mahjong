package io.fele.mahjong.server

import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.{Config => EngineConfig, _}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

/** An answer must only ever apply to the decision it was written for.
  *
  * Without that, an answer arriving after its prompt expired — or a reply to a
  * prompt the client was re-sent on reconnect — sits in the mailbox and is
  * consumed instantly by the *next* decision: the engine plays a tile chosen
  * for a different board, and the review records it as a deliberate choice,
  * which is exactly what issue #42 set out to stop.
  */
class WebSocketPlayerSpec extends AnyFlatSpec with Matchers {

  implicit val engineConfig: EngineConfig = new EngineConfig()

  private class RecordingSink extends WebSocketPlayer.PromptSink {
    val timeouts = ListBuffer.empty[(Int, String)]
    val prompts  = ListBuffer.empty[(String, Long)]
    override def selfWin(seat: Int, tile: Tile, score: Int, st: CurState, promptId: Long): Unit = prompts += (("self_win", promptId))
    override def win(seat: Int, tile: Tile, score: Int, st: CurState, promptId: Long): Unit = prompts += (("win", promptId))
    override def selfKong(seat: Int, tiles: Set[Tile], st: CurState, promptId: Long): Unit = prompts += (("self_kong", promptId))
    override def kong(seat: Int, tile: Tile, st: CurState, promptId: Long): Unit = prompts += (("kong", promptId))
    override def pong(seat: Int, tile: Tile, st: CurState, promptId: Long): Unit = prompts += (("pong", promptId))
    override def chow(seat: Int, tile: Tile, ps: Set[ChowPosition], st: CurState, promptId: Long): Unit = prompts += (("chow", promptId))
    override def discard(seat: Int, st: CurState, promptId: Long): Unit = prompts += (("discard", promptId))
    override def timedOut(seat: Int, kind: String): Unit = timeouts += ((seat, kind))
  }

  private def tile(w: String): Tile = Models.tileFromWire(w).toOption.get

  private val hand = List("D1", "D2", "D3", "B1", "B2", "B3", "C1", "C2", "C3", "D5", "D5", "B7", "B8")
  private def state(p: WebSocketPlayer): CurState =
    CurState(p.privateInfo, List(PublicState(Nil), PublicState(Nil), PublicState(Nil)), Nil, 0, 60)

  private def player(sink: WebSocketPlayer.PromptSink, timeoutMs: Long) =
    new WebSocketPlayer(0, hand.map(tile), Nil, sink, timeoutMs)

  "an answer that arrives after its prompt expired" should "not be applied to the next decision" in {
    val sink = new RecordingSink
    val p    = player(sink, 120)

    // decision 1: let it expire, then the straggler arrives
    val first = p.decideDiscard(state(p))
    sink.timeouts.toList shouldBe List((0, "discard"))
    val stalePromptId = sink.prompts.head._2
    p.submit(WebSocketPlayer.ClientAction(None, Some(tile("B7")), None), Some(stalePromptId))

    // decision 2 must NOT silently consume it
    val before = System.currentTimeMillis()
    val second = p.decideDiscard(state(p))
    val waited = System.currentTimeMillis() - before

    waited should be >= 100L                      // it really waited rather than taking the straggler
    sink.timeouts.toList shouldBe List((0, "discard"), (0, "discard"))
    second shouldBe p.hand.dynamicTiles.head      // the engine's default, not B7
    first shouldBe p.hand.dynamicTiles.head
  }

  "an answer for the current decision" should "be applied" in {
    val sink = new RecordingSink
    val p    = player(sink, 5000)
    val answered = new java.util.concurrent.CountDownLatch(1)

    val t = new Thread(new Runnable {
      override def run(): Unit = {
        while (sink.prompts.isEmpty) Thread.sleep(5)
        p.submit(WebSocketPlayer.ClientAction(None, Some(tile("B7")), None), Some(p.currentPromptId))
        answered.countDown()
      }
    })
    t.setDaemon(true); t.start()

    val chosen = p.decideDiscard(state(p))
    answered.await(2, java.util.concurrent.TimeUnit.SECONDS)
    chosen shouldBe tile("B7")
    sink.timeouts shouldBe empty
  }

  "an answer carrying another decision's id" should "be ignored" in {
    val sink = new RecordingSink
    val p    = player(sink, 120)
    // a reply to a prompt that is not the one being asked (e.g. a prompt the
    // client was re-served on reconnect and answered)
    p.submit(WebSocketPlayer.ClientAction(None, Some(tile("B7")), None), Some(999L))
    val chosen = p.decideDiscard(state(p))
    chosen shouldBe p.hand.dynamicTiles.head
    sink.timeouts.toList shouldBe List((0, "discard"))
  }

  "prompt ids" should "advance so each decision is distinguishable" in {
    val sink = new RecordingSink
    val p    = player(sink, 60)
    p.decideDiscard(state(p))
    p.decidePong(tile("D5"), state(p))
    val ids = sink.prompts.map(_._2).toList
    ids.distinct.size shouldBe ids.size
    ids shouldBe ids.sorted
  }
}
