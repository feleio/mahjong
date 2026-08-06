package io.fele.mahjong.server

import io.fele.app.mahjong.{Config => EngineConfig, _}
import io.fele.app.mahjong.player._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.collection.mutable.ListBuffer

/** Replay fidelity without a database: record a real bot game in memory using
  * the exact event mapping GameRecorder persists, then re-drive the engine
  * from (wall, events) and require zero divergence. */
class GameReplayerSpec extends AnyFlatSpec with Matchers {

  implicit val engineConfig: EngineConfig = new EngineConfig()

  /** Mirrors GameRecorder.logger's event mapping, minus the DB. */
  private def recordBotGame(seed: Long): (List[String], List[GameEventRow], GameOutcome) = {
    val drawer = new RandomTileDrawer(Some(seed))
    val wall   = drawer.drawerState.shuffledTiles.map(Models.tileToWire).toList
    val players: List[Player] = List(
      new Chicken(0, drawer.popHand()),
      new ThreePointChicken(1, drawer.popHand()),
      new FirstFelix(2, drawer.popHand(), 5),
      new Chicken(3, drawer.popHand())
    )
    val state  = GameState(players, None, Nil, 0, drawer)
    val events = ListBuffer.empty[GameEventRow]
    def ev(t: String, seat: Option[Int] = None, src: Option[Int] = None,
           tile: Option[Tile] = None, cp: Option[String] = None): Unit = {
      events += GameEventRow("mem", events.size, t, seat, src, tile.map(Models.tileToWire), cp, Instant.now())
    }
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
    (wall, events.toList, GameOutcome.from(result.winnersInfo))
  }

  "GameReplayer" should "replay recorded bot games without divergence" in {
    // A spread of seeds to cover self-wins, discard wins, claims and draws
    (1L to 25L).foreach { seed =>
      val (wall, events, outcome) = recordBotGame(seed)
      withClue(s"seed $seed: ") {
        noException should be thrownBy
          GameReplayer.replay(wall, events, outcome, None, GameReplayer.NoopObserver)
      }
    }
  }

  it should "surface every decision of the observed seat with the chosen key among the legal keys" in {
    (1L to 10L).foreach { seed =>
      val (wall, events, outcome) = recordBotGame(seed)
      (0 to 3).foreach { seat =>
        var discards = 0
        val observer = new GameReplayer.DecisionObserver {
          override def onDecision(eventPos: Int, kind: String, cs: CurState, contextTile: Option[Int],
                                  head: io.fele.app.mahjong.rl.PolicyOut => Array[Float],
                                  keys: List[(String, Int)], chosen: String): Unit = {
            keys.map(_._1) should contain(chosen)
            if (kind == "discard") {
              discards += 1
              cs.myInfo.tiles should not be empty
            }
          }
        }
        GameReplayer.replay(wall, events, outcome, Some(seat), observer)
        withClue(s"seed $seed seat $seat: ") { discards should be > 0 }
      }
    }
  }

  it should "refuse a tampered stream instead of producing a wrong analysis" in {
    // Tamper a draw event: draws are dictated by the wall, so the verifying
    // logger must flag the divergence the moment the engine re-emits it.
    val (wall, events, outcome) = recordBotGame(3L)
    val firstDraw = events.indexWhere(_.eventType == "draw")
    val original  = events(firstDraw).tile.get
    val swapped   = if (original == "HW_E") "HW_S" else "HW_E"
    val tampered  = events.updated(firstDraw, events(firstDraw).copy(tile = Some(swapped)))
    a[GameReplayer.ReplayMismatchException] should be thrownBy
      GameReplayer.replay(wall, tampered, outcome, None, GameReplayer.NoopObserver)
  }

  it should "refuse a truncated stream" in {
    val (wall, events, outcome) = recordBotGame(4L)
    a[GameReplayer.ReplayMismatchException] should be thrownBy
      GameReplayer.replay(wall, events.dropRight(1), outcome, None, GameReplayer.NoopObserver)
  }
}
