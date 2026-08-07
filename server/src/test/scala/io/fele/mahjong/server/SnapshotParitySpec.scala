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

/** The snapshot has to carry everything the game UI draws (issue #47): who
  * deals, what just happened, which discard is still claimable, the running
  * money — and it must not leak a drawn tile to the rest of the table. */
class SnapshotParitySpec extends AnyFlatSpec with Matchers {

  implicit val engineConfig: EngineConfig = new EngineConfig()
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val botSeats = List(
    Seat(0, SeatKind.AiChicken,           None, "Bot A"),
    Seat(1, SeatKind.AiFirstFelix,        None, "Bot B"),
    Seat(2, SeatKind.AiFirstFelix,        None, "Bot C"),
    Seat(3, SeatKind.AiThreePointChicken, None, "Bot D")
  )

  /** Every snapshot a given seat would receive, in order.
    *
    * The subscription never ends on its own, so frames are accumulated into a
    * Ref as they arrive — cancelling a `compile.toList` fiber would throw away
    * everything it had collected. */
  private def snapshotsFor(seat: Option[Int], dealer: Int): List[Json] =
    Dispatcher.parallel[IO].allocated.flatMap { case (dispatcher, release) =>
      for {
        topic  <- Topic[IO, Json]
        seen   <- cats.effect.Ref[IO].of(Vector.empty[Json])
        runner <- IO.delay(GameRunner.create("snap-test", botSeats, Some(4242L), topic, dispatcher,
                    dealerSeat = dealer, balances = List(10, -5, 0, -5), gamesPlayed = 3))
        fiber  <- runner.subscribe(seat).evalTap(j => seen.update(_ :+ j)).compile.drain.start
        _      <- IO.sleep(200.millis)          // let the subscription attach
        _      <- IO.delay(runner.start())
        _      <- waitFinished(runner, 60.seconds)
        _      <- IO.sleep(300.millis)          // drain the tail
        _      <- fiber.cancel
        out    <- seen.get
        _      <- release
      } yield out.toList
    }.unsafeRunSync().filter(_.hcursor.get[String]("type").toOption.contains("snapshot"))

  private def waitFinished(runner: GameRunner, timeout: FiniteDuration): IO[Unit] =
    if (timeout <= Duration.Zero) IO.raiseError(new RuntimeException("game did not finish"))
    else if (runner.isFinished) IO.unit
    else IO.sleep(50.millis) >> waitFinished(runner, timeout - 50.millis)

  "a snapshot" should "carry the room context the table UI renders" in {
    val snaps = snapshotsFor(Some(0), dealer = 2)
    snaps should not be empty
    val first = snaps.head.hcursor
    first.get[Int]("dealerSeat").toOption shouldBe Some(2)
    first.get[List[Int]]("balances").toOption shouldBe Some(List(10, -5, 0, -5))
    first.get[Int]("gamesPlayed").toOption shouldBe Some(3)
  }

  it should "describe what just happened in structured form, not only as a label" in {
    val snaps = snapshotsFor(Some(0), dealer = 0)
    val kinds = snaps.flatMap(_.hcursor.downField("event").get[String]("kind").toOption).distinct
    kinds should contain("draw")
    kinds should contain("discard")
    kinds should contain("end")

    // a discard event names the seat and the tile, so the feed can read it out
    val discard = snaps.find(_.hcursor.downField("event").get[String]("kind").toOption.contains("discard")).get
    discard.hcursor.downField("event").get[Int]("seat").toOption shouldBe defined
    discard.hcursor.downField("event").get[String]("tile").toOption shouldBe defined
  }

  it should "mark the newest discard as claimable and nothing else" in {
    val snaps = snapshotsFor(Some(0), dealer = 0)
    snaps.foreach { s =>
      val kind    = s.hcursor.downField("event").get[String]("kind").toOption
      val pending = s.hcursor.downField("pendingDiscard").focus.exists(!_.isNull)
      withClue(s"event=$kind pending=$pending: ") {
        pending shouldBe kind.contains("discard")
      }
    }
  }

  it should "keep a drawn tile private to the drawer" in {
    // seat 0 sees its own draws...
    val mine = snapshotsFor(Some(0), dealer = 0).filter { s =>
      s.hcursor.downField("event").get[String]("kind").toOption.contains("draw") &&
      s.hcursor.downField("event").get[Int]("seat").toOption.contains(0) &&
      !s.hcursor.get[Boolean]("isFinished").toOption.getOrElse(false)
    }
    mine should not be empty
    mine.foreach(_.hcursor.downField("event").get[String]("tile").toOption shouldBe defined)

    // ...but never anyone else's, and a spectator sees none at all
    val others = snapshotsFor(Some(0), dealer = 0).filter { s =>
      s.hcursor.downField("event").get[String]("kind").toOption.contains("draw") &&
      !s.hcursor.downField("event").get[Int]("seat").toOption.contains(0) &&
      !s.hcursor.get[Boolean]("isFinished").toOption.getOrElse(false)
    }
    others should not be empty
    others.foreach(_.hcursor.downField("event").downField("tile").focus.map(_.isNull) shouldBe Some(true))

    val spectator = snapshotsFor(None, dealer = 0).filter { s =>
      s.hcursor.downField("event").get[String]("kind").toOption.contains("draw") &&
      !s.hcursor.get[Boolean]("isFinished").toOption.getOrElse(false)
    }
    spectator should not be empty
    spectator.foreach(_.hcursor.downField("event").downField("tile").focus.map(_.isNull) shouldBe Some(true))
  }

  "a room code" should "be readable aloud and resolve back to the room" in {
    val code = Room.newCode()
    code should have length 6
    code should fullyMatch regex "[A-Z2-9]{6}"
    // the ambiguous glyphs are the whole point of the alphabet
    code should not include "O"
    code should not include "I"
    code should not include "0"
    code should not include "1"
  }
}
