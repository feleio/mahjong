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

  it should "keep timed-out turns out of the agreement metric (#42)" in {
    assume(available, "Postgres not reachable")
    assume(ChampionService.unavailableReason.isEmpty, "champion model unavailable")
    repo.init.unsafeRunSync()

    val runner = runBotGame(20260807L)
    val gameId = runner.recorder.get.gameId
    try {
      val svc  = new ReviewService(repo)
      val full = svc.review(gameId, 0).unsafeRunSync().toOption.get
      full.summary.timedOut shouldBe 0   // bots never time out

      // Mark two of seat 0's real decisions as expired prompts, exactly as
      // GameRecorder would, and re-review with a cold cache.
      val marked = full.decisions.sortBy(_.seq).take(2)
      marked.foreach { d =>
        repo.insertTimeout(DecisionTimeoutRow(gameId, d.seq, 0, d.kind, java.time.Instant.now()))
          .unsafeRunSync()
      }
      val after = new ReviewService(repo).review(gameId, 0).unsafeRunSync().toOption.get

      after.summary.timedOut shouldBe 2
      after.summary.decisions shouldBe full.summary.decisions - 2
      after.decisions.size shouldBe full.decisions.size          // still shown, just not scored
      after.decisions.count(_.timedOut) shouldBe 2
      after.decisions.takeRight(2).forall(_.timedOut) shouldBe true  // sorted to the bottom

      // the rate is recomputed over the played decisions only
      val played = after.decisions.filterNot(_.timedOut)
      after.summary.agreements shouldBe played.count(_.agree)
      after.summary.agreementRate shouldBe (played.count(_.agree).toDouble / played.size) +- 1e-9
    } finally repo.deleteGame(gameId).unsafeRunSync()
  }

  it should "explain disagreements in grounded terms (#44)" in {
    assume(available, "Postgres not reachable")
    assume(ChampionService.unavailableReason.isEmpty, "champion model unavailable")
    repo.init.unsafeRunSync()

    val runner = runBotGame(20260808L)
    val gameId = runner.recorder.get.gameId
    try {
      val r = new ReviewService(repo).review(gameId, 0).unsafeRunSync().toOption.get
      r.decisions.filter(_.agree).foreach(_.why shouldBe None)   // nothing to explain
      val explained = r.decisions.filterNot(_.agree).flatMap(d => d.why.map((d, _)))
      explained should not be empty
      explained.foreach { case (d, text) =>
        text.trim should not be empty
        d.bucket.get should (be("shape") or be("tempo") or be("safety") or be("accept") or be("other"))
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
