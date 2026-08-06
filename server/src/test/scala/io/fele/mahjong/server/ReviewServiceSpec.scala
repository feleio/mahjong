package io.fele.mahjong.server

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import fs2.concurrent.Topic
import io.circe.Json
import io.fele.app.mahjong.{Config => EngineConfig}
import io.fele.mahjong.server.Models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/** End-to-end review path: record a real bot game through GameRunner (the
  * exact code path human games take), then replay-review it against the
  * champion. Needs Postgres + the champion ONNX export. */
class ReviewServiceSpec extends AnyFlatSpec with Matchers {
  import TestDb.{available, repo}

  implicit val engineConfig: EngineConfig = new EngineConfig()
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val seats = List(
    Seat(0, SeatKind.AiChicken,           None, "Reviewed Bot"),
    Seat(1, SeatKind.AiFirstFelix,        None, "Bot B"),
    Seat(2, SeatKind.AiFirstFelix,        None, "Bot C"),
    Seat(3, SeatKind.AiThreePointChicken, None, "Bot D")
  )

  private def runBotGame(seed: Long): GameRunner =
    Dispatcher.parallel[IO].allocated.flatMap { case (dispatcher, release) =>
      for {
        topic  <- Topic[IO, Json]
        runner <- IO.delay(GameRunner.create(
                    "review-test", seats, Some(seed), topic, dispatcher, recordRepo = Some(repo)))
        _      <- IO.delay(runner.start())
        _      <- waitFinished(runner, 60.seconds)
        _      <- release
      } yield runner
    }.unsafeRunSync()

  private def waitFinished(runner: GameRunner, timeout: FiniteDuration): IO[Unit] =
    if (timeout <= Duration.Zero) IO.raiseError(new RuntimeException("game did not finish in time"))
    else if (runner.isFinished) IO.unit
    else IO.sleep(100.millis) >> waitFinished(runner, timeout - 100.millis)

  "ReviewService" should "review a recorded game against the champion" in {
    assume(available, "Postgres not reachable")
    assume(ChampionService.unavailableReason.isEmpty,
      s"champion model unavailable: ${ChampionService.unavailableReason.getOrElse("")}")
    repo.init.unsafeRunSync()

    val runner = runBotGame(20260806L)
    val gameId = runner.recorder.get.gameId
    try {
      val svc = new ReviewService(repo)
      val result = svc.review(gameId, 0).unsafeRunSync()
      result match {
        case Left(e)  => fail(s"review failed: ${e.message}")
        case Right(r) =>
          r.gameId shouldBe gameId
          r.seat shouldBe 0
          r.playerName shouldBe "Reviewed Bot"
          r.summary.decisions should be > 0
          r.summary.decisions shouldBe r.decisions.size
          r.summary.agreementRate should (be >= 0.0 and be <= 1.0)
          r.decisions.foreach { d =>
            d.chosenProb should (be >= 0.0 and be <= 1.0)
            d.bestProb should (be >= 0.0 and be <= 1.0)
            d.agree shouldBe (d.chosen == d.best)
            if (d.agree) d.gap shouldBe 0.0 +- 1e-9
          }
          // disagreements sorted first, by gap descending
          val (dis, agr) = r.decisions.span(!_.agree)
          agr.forall(_.agree) shouldBe true
          dis.map(_.gap) shouldBe dis.map(_.gap).sortBy(-(_: Double))

          // second call must hit the cache and agree with itself
          svc.review(gameId, 0).unsafeRunSync() shouldBe Right(r)
      }
    } finally repo.deleteGame(gameId).unsafeRunSync()
  }

  it should "reject unknown games and bad seats" in {
    assume(available, "Postgres not reachable")
    assume(ChampionService.unavailableReason.isEmpty, "champion model unavailable")
    repo.init.unsafeRunSync()
    val svc = new ReviewService(repo)
    svc.review("no-such-game", 0).unsafeRunSync() shouldBe Left(ReviewService.GameNotFound)
    svc.review("whatever", 7).unsafeRunSync() match {
      case Left(_: ReviewService.BadSeat) => succeed
      case other                          => fail(s"expected BadSeat, got $other")
    }
  }
}
