package io.fele.mahjong.server

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import fs2.concurrent.Topic
import io.circe.Json
import io.fele.app.mahjong.{Config => EngineConfig, RandomTileDrawer}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.fele.mahjong.server.Models._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/** End-to-end: run a real bot game through [[GameRunner]] with recording on and
  * verify the persisted record is complete and replay-consistent. This is the
  * same code path human games take — only the Player implementations differ. */
class GameRecordingSpec extends AnyFlatSpec with Matchers {
  import TestDb.{available, repo}

  implicit val engineConfig: EngineConfig = new EngineConfig()
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val botSeats = List(
    Seat(0, SeatKind.AiChicken,           None, "Bot A"),
    Seat(1, SeatKind.AiRandom,            None, "Bot B"),
    Seat(2, SeatKind.AiFirstFelix,        None, "Bot C"),
    Seat(3, SeatKind.AiThreePointChicken, None, "Bot D")
  )

  private def runBotGame(seed: Long): GameRunner = {
    val runner = Dispatcher.parallel[IO].allocated.flatMap { case (dispatcher, release) =>
      for {
        topic  <- Topic[IO, Json]
        runner <- IO.delay(GameRunner.create(
                    "test-room", botSeats, Some(seed), topic, dispatcher, recordRepo = Some(repo)))
        _      <- IO.delay(runner.start())
        _      <- waitFinished(runner, 30.seconds)
        _      <- release
      } yield runner
    }.unsafeRunSync()
    runner
  }

  private def waitFinished(runner: GameRunner, timeout: FiniteDuration): IO[Unit] =
    if (timeout <= Duration.Zero) IO.raiseError(new RuntimeException("game did not finish in time"))
    else if (runner.isFinished) IO.unit
    else IO.sleep(100.millis) >> waitFinished(runner, timeout - 100.millis)

  "a recorded bot game" should "persist a complete, replay-consistent event stream" in {
    assume(available, "Postgres not reachable")
    repo.init.unsafeRunSync()

    val seed   = 20260722L
    val runner = runBotGame(seed)
    val gameId = runner.recorder.get.gameId
    try {
      val game   = repo.getGame(gameId).unsafeRunSync().get
      val events = repo.eventsFor(gameId).unsafeRunSync()

      // game row
      game.roomId shouldBe "test-room"
      game.seats shouldBe botSeats
      game.seed shouldBe Some(seed)
      game.status shouldBe GameRecordStatus.Finished
      game.finishedAt shouldBe defined
      game.outcome shouldBe defined
      game.wall should have size 136

      // the persisted wall is exactly what the seed regenerates
      val regenerated = new RandomTileDrawer(Some(seed)).drawerState.shuffledTiles.map(Models.tileToWire).toList
      game.wall shouldBe regenerated

      // event stream shape
      events.map(_.seq) shouldBe events.indices.toList
      events.head.eventType shouldBe "start"
      events.last.eventType shouldBe "end"
      events.count(_.eventType == "end") shouldBe 1
      events.map(_.ts).sliding(2).foreach { case List(a, b) => assert(!b.isBefore(a)); case _ => () }

      // replay consistency: the initial 4x13 deal comes off the front of the
      // wall, and every draw (incl. post-kong replacement draws) pops the wall
      // sequentially from position 52
      val draws = events.filter(_.eventType == "draw").flatMap(_.tile)
      draws should not be empty
      draws shouldBe game.wall.slice(52, 52 + draws.size)

      // every non-terminal event carries an acting seat and a tile
      events.filter(e => e.eventType != "start" && e.eventType != "end").foreach { e =>
        e.seat shouldBe defined
        e.tile shouldBe defined
      }
      events.filter(_.eventType == "chow").foreach(_.chowPosition shouldBe defined)
      events.filter(e => e.eventType == "pong" || e.eventType == "chow").foreach { e =>
        e.sourceSeat shouldBe defined
        e.sourceSeat should not be e.seat
      }

      // outcome matches the engine's final state
      val outcome = game.outcome.get
      if (outcome.drawn) {
        runner.state.winnersInfo shouldBe None
        outcome.winners shouldBe Nil
      } else {
        val wi = runner.state.winnersInfo.get
        outcome.isSelfWin shouldBe wi.isSelfWin
        outcome.winningTile shouldBe Some(Models.tileToWire(wi.winningTile))
        outcome.loserSeat shouldBe wi.loserId
        outcome.winners.map(w => (w.seat, w.score)).toSet shouldBe wi.winners.map(w => (w.id, w.score))
      }
    } finally repo.deleteGame(gameId).unsafeRunSync()
  }

  it should "record identical walls for identical seeds" in {
    assume(available, "Postgres not reachable")
    repo.init.unsafeRunSync()

    val r1 = runBotGame(7L)
    val r2 = runBotGame(7L)
    try {
      val g1 = repo.getGame(r1.recorder.get.gameId).unsafeRunSync().get
      val g2 = repo.getGame(r2.recorder.get.gameId).unsafeRunSync().get
      g1.wall shouldBe g2.wall
    } finally {
      repo.deleteGame(r1.recorder.get.gameId).unsafeRunSync()
      repo.deleteGame(r2.recorder.get.gameId).unsafeRunSync()
    }
  }
}
