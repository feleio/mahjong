package io.fele.mahjong.server

import io.fele.app.mahjong.{Config => EngineConfig, _}
import io.fele.app.mahjong.player._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.collection.mutable.ListBuffer

/** Broad replay-fidelity sweep: many seeds x several opponent mixes x every
  * dealer seat. The narrower per-seed test can pass on luck; this one is what
  * caught the replayer assuming seat 0 always starts. Fast (<1s) — keep it. */
class ReplayStressSpec extends AnyFlatSpec with Matchers {

  implicit val engineConfig: EngineConfig = new EngineConfig()

  private def mk(kind: Int, id: Int, hand: List[Tile]): Player = kind match {
    case 0 => new Chicken(id, hand)
    case 1 => new ThreePointChicken(id, hand)
    case 2 => new FirstFelix(id, hand, 5)
    case _ => new RandomDiscard(id, hand)
  }

  private def record(seed: Long, mix: List[Int]): (List[String], List[GameEventRow], GameOutcome, Int, Int) = {
    val drawer = new RandomTileDrawer(Some(seed))
    val wall   = drawer.drawerState.shuffledTiles.map(Models.tileToWire).toList
    val players = mix.zipWithIndex.map { case (k, i) => mk(k, i, drawer.popHand()) }
    val dealer  = (seed % 4).toInt.abs
    val state   = GameState(players, None, Nil, dealer, drawer)
    val events  = ListBuffer.empty[GameEventRow]
    def ev(t: String, seat: Option[Int] = None, src: Option[Int] = None,
           tile: Option[Tile] = None, cp: Option[String] = None): Unit =
      events += GameEventRow("mem", events.size, t, seat, src, tile.map(Models.tileToWire), cp, Instant.now())
    implicit val gl: GameLogger = new GameLogger {
      override def start():  Unit = ev("start")
      override def resume(): Unit = ev("resume")
      override def draw(e: DrawEvent):       Unit = ev("draw",    Some(e.playerId), tile = Some(e.tile))
      override def discard(e: DiscardEvent): Unit = ev("discard", Some(e.playerId), tile = Some(e.tile))
      override def kong(e: KongEvent):       Unit = ev("kong",    Some(e.playerId), Some(e.sourcePlayerId), Some(e.tile))
      override def pong(e: PongEvent):       Unit = ev("pong",    Some(e.playerId), Some(e.sourcePlayerId), Some(e.tile))
      override def chow(e: ChowEvent):       Unit = ev("chow",    Some(e.playerId), Some(e.sourcePlayerId), Some(e.tile), Some(e.position.toString))
      override def end(e: EndEvent):         Unit = ev("end")
    }
    val result = new FlowImpl(state).start()
    val claims = events.count(e => Set("pong", "chow", "kong").contains(e.eventType))
    (wall, events.toList, GameOutcome.from(result.winnersInfo), claims, dealer)
  }

  "GameReplayer" should "replay hundreds of games across opponent mixes with zero divergence" in {
    val mixes = List(
      List(0, 0, 0, 0), List(2, 2, 2, 2), List(0, 1, 2, 3),
      List(2, 0, 2, 0), List(1, 1, 2, 2), List(3, 3, 3, 3)
    )
    var games = 0; var claims = 0; var selfWins = 0; var draws = 0; var decisions = 0
    mixes.foreach { mix =>
      (1L to 60L).foreach { seed =>
        val (wall, events, outcome, cl, dealer) = record(seed * 7 + mix.sum, mix)
        games += 1; claims += cl
        if (outcome.drawn) draws += 1 else if (outcome.isSelfWin) selfWins += 1
        (0 to 3).foreach { seat =>
          val obs = new GameReplayer.DecisionObserver {
            override def onDecision(eventPos: Int, kind: String, cs: CurState, contextTile: Option[Int],
                                    head: io.fele.app.mahjong.rl.PolicyOut => Array[Float],
                                    keys: List[(String, Int)], chosen: String): Unit = {
              decisions += 1
              // the recorded choice must always be one of the legal keys, and
              // the cursor must point at a real position in the stream
              keys.map(_._1) should contain(chosen)
              eventPos should (be >= 0 and be <= events.size)
            }
          }
          withClue(s"mix=$mix seed=$seed seat=$seat: ") {
            noException should be thrownBy GameReplayer.replay(wall, events, outcome, dealer, Some(seat), obs)
          }
        }
      }
    }
    info(s"games=$games claims=$claims selfWins=$selfWins draws=$draws observedDecisions=$decisions")
    games shouldBe 360
    claims should be > 200      // the sweep really did exercise pong/chow/kong paths
    selfWins should be > 0
    draws should be > 0
  }
}
