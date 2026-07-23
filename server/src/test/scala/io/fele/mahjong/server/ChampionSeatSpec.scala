package io.fele.mahjong.server

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import fs2.concurrent.Topic
import io.circe.Json
import io.fele.app.mahjong.{Config => EngineConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.fele.mahjong.server.Models._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/** The champion (net D) plays a full game through the same GameRunner path as
  * every other seat. Skipped when the ONNX export or Postgres is unavailable. */
class ChampionSeatSpec extends AnyFlatSpec with Matchers {
  import TestDb.{available, repo}

  implicit val engineConfig: EngineConfig = new EngineConfig()
  implicit val ec: ExecutionContext = ExecutionContext.global

  "SeatKind" should "round-trip ai_champion on the wire" in {
    SeatKind.fromString("ai_champion") shouldBe Some(SeatKind.AiChampion)
    SeatKind.toWire(SeatKind.AiChampion) shouldBe "ai_champion"
    SeatKind.all should contain(SeatKind.AiChampion)
  }

  "a champion-seated bot game" should "run to completion and be recorded" in {
    assume(available, "Postgres not reachable")
    assume(ChampionService.unavailableReason.isEmpty,
      s"champion model unavailable: ${ChampionService.unavailableReason.getOrElse("")}")
    repo.init.unsafeRunSync()

    val seats = List(
      Seat(0, SeatKind.AiChampion,   None, "Bot Champion"),
      Seat(1, SeatKind.AiFirstFelix, None, "Bot Felix"),
      Seat(2, SeatKind.AiFirstFelix, None, "Bot Felix"),
      Seat(3, SeatKind.AiFirstFelix, None, "Bot Felix")
    )

    val runner = Dispatcher.parallel[IO].allocated.flatMap { case (dispatcher, release) =>
      for {
        topic  <- Topic[IO, Json]
        runner <- IO.delay(GameRunner.create(
                    "champion-test", seats, Some(123L), topic, dispatcher, recordRepo = Some(repo)))
        _      <- IO.delay(runner.start())
        _      <- waitFinished(runner, 60.seconds)
        _      <- release
      } yield runner
    }.unsafeRunSync()

    val gameId = runner.recorder.get.gameId
    try {
      val game   = repo.getGame(gameId).unsafeRunSync().get
      val events = repo.eventsFor(gameId).unsafeRunSync()
      game.status shouldBe GameRecordStatus.Finished
      game.seats.head.kind shouldBe SeatKind.AiChampion
      events.head.eventType shouldBe "start"
      events.last.eventType shouldBe "end"
      // the champion actually acted: seat 0 drew and discarded like any player
      events.exists(e => e.eventType == "draw" && e.seat.contains(0)) shouldBe true
    } finally repo.deleteGame(gameId).unsafeRunSync()
  }

  private def waitFinished(runner: GameRunner, timeout: FiniteDuration): IO[Unit] =
    if (timeout <= Duration.Zero) IO.raiseError(new RuntimeException("game did not finish in time"))
    else if (runner.isFinished) IO.unit
    else IO.sleep(100.millis) >> waitFinished(runner, timeout - 100.millis)
}
