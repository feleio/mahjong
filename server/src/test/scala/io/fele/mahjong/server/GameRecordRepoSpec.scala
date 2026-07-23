package io.fele.mahjong.server

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import doobie.Transactor
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import io.fele.mahjong.server.Models._

import java.time.Instant
import java.util.UUID

/** Shared test-DB plumbing. Uses the same dev Postgres as the server
  * (docker-compose, host port 5434); overridable via MAHJONG_DB_* env vars. */
object TestDb {
  private def env(k: String, default: String) = sys.env.getOrElse(k, default)

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    driver      = env("MAHJONG_DB_DRIVER", "org.postgresql.Driver"),
    url         = env("MAHJONG_DB_URL", "jdbc:postgresql://localhost:5434/mahjong"),
    user        = env("MAHJONG_DB_USER", "mahjong"),
    password    = env("MAHJONG_DB_PASSWORD", "mahjong"),
    logHandler  = None
  )

  lazy val available: Boolean = {
    import doobie.implicits._
    scala.util.Try(sql"SELECT 1".query[Int].unique.transact(xa).unsafeRunSync()).isSuccess
  }

  val repo = new GameRecordRepo(xa)

  def sampleSeats: List[Seat] = List(
    Seat(0, SeatKind.Human,     Some("p0"), "Alice"),
    Seat(1, SeatKind.AiChicken, None,       "Bot Chicken"),
    Seat(2, SeatKind.AiRandom,  None,       "Bot Random"),
    Seat(3, SeatKind.AiFirstFelix, None,    "Bot Felix")
  )
}

class GameRecordRepoSpec extends AnyFlatSpec with Matchers {
  import TestDb._

  private def withGame[A](f: String => A): A = {
    val id = UUID.randomUUID().toString
    try f(id) finally repo.deleteGame(id).unsafeRunSync()
  }

  private val wall = List.fill(34)(List("D1", "B5", "C9", "HW_E")).flatten

  "GameRecordRepo.init" should "be idempotent" in {
    assume(available, "Postgres not reachable")
    repo.init.unsafeRunSync()
    repo.init.unsafeRunSync()
  }

  "insertGame/getGame" should "round-trip seats, seed, wall and status" in {
    assume(available, "Postgres not reachable")
    repo.init.unsafeRunSync()
    withGame { id =>
      repo.insertGame(id, "room-1", sampleSeats, Some(42L), wall, Instant.now()).unsafeRunSync()
      val got = repo.getGame(id).unsafeRunSync().get
      got.roomId shouldBe "room-1"
      got.seats shouldBe sampleSeats
      got.seed shouldBe Some(42L)
      got.wall should have size 136
      got.wall shouldBe wall
      got.status shouldBe GameRecordStatus.InProgress
      got.outcome shouldBe None
      got.finishedAt shouldBe None
    }
  }

  "insertEvent/eventsFor" should "preserve order and all fields" in {
    assume(available, "Postgres not reachable")
    repo.init.unsafeRunSync()
    withGame { id =>
      repo.insertGame(id, "room-2", sampleSeats, None, wall, Instant.now()).unsafeRunSync()
      val now = Instant.now()
      val rows = List(
        GameEventRow(id, 0, "start",   None,    None,    None,       None,         now),
        GameEventRow(id, 1, "draw",    Some(0), None,    Some("D1"), None,         now),
        GameEventRow(id, 2, "discard", Some(0), None,    Some("D1"), None,         now),
        GameEventRow(id, 3, "chow",    Some(1), Some(0), Some("D1"), Some("LEFT"), now),
        GameEventRow(id, 4, "end",     None,    None,    None,       None,         now)
      )
      rows.foreach(r => repo.insertEvent(r).unsafeRunSync())
      val got = repo.eventsFor(id).unsafeRunSync()
      got.map(_.seq) shouldBe List(0, 1, 2, 3, 4)
      got.map(_.eventType) shouldBe List("start", "draw", "discard", "chow", "end")
      got(3).seat shouldBe Some(1)
      got(3).sourceSeat shouldBe Some(0)
      got(3).tile shouldBe Some("D1")
      got(3).chowPosition shouldBe Some("LEFT")
    }
  }

  "finishGame" should "store the outcome and flip the status" in {
    assume(available, "Postgres not reachable")
    repo.init.unsafeRunSync()
    withGame { id =>
      repo.insertGame(id, "room-3", sampleSeats, None, wall, Instant.now()).unsafeRunSync()
      val outcome = GameOutcome(drawn = false, isSelfWin = false, Some("B5"), Some(2), List(OutcomeWinner(0, 4)))
      repo.finishGame(id, outcome, Instant.now()).unsafeRunSync()
      val got = repo.getGame(id).unsafeRunSync().get
      got.status shouldBe GameRecordStatus.Finished
      got.outcome shouldBe Some(outcome)
      got.finishedAt shouldBe defined
    }
  }

  "abortGame" should "only touch in-progress games" in {
    assume(available, "Postgres not reachable")
    repo.init.unsafeRunSync()
    withGame { id =>
      repo.insertGame(id, "room-4", sampleSeats, None, wall, Instant.now()).unsafeRunSync()
      repo.abortGame(id, Instant.now()).unsafeRunSync()
      repo.getGame(id).unsafeRunSync().get.status shouldBe GameRecordStatus.Aborted

      // a finished game must not be re-marked
      withGame { id2 =>
        repo.insertGame(id2, "room-4", sampleSeats, None, wall, Instant.now()).unsafeRunSync()
        repo.finishGame(id2, GameOutcome(drawn = true, isSelfWin = false, None, None, Nil), Instant.now()).unsafeRunSync()
        repo.abortGame(id2, Instant.now()).unsafeRunSync()
        repo.getGame(id2).unsafeRunSync().get.status shouldBe GameRecordStatus.Finished
      }
    }
  }

  "deleteGame" should "cascade to events" in {
    assume(available, "Postgres not reachable")
    repo.init.unsafeRunSync()
    val id = UUID.randomUUID().toString
    repo.insertGame(id, "room-5", sampleSeats, None, wall, Instant.now()).unsafeRunSync()
    repo.insertEvent(GameEventRow(id, 0, "start", None, None, None, None, Instant.now())).unsafeRunSync()
    repo.deleteGame(id).unsafeRunSync()
    repo.getGame(id).unsafeRunSync() shouldBe None
    repo.eventsFor(id).unsafeRunSync() shouldBe Nil
  }
}
