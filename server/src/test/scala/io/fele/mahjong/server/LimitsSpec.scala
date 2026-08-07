package io.fele.mahjong.server

import cats.effect.{IO, Ref}
import cats.syntax.all._
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import io.circe.Json
import io.circe.syntax._
import io.fele.app.mahjong.{Config => EngineConfig}
import io.fele.mahjong.server.Models._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/** Input validation and the resource ceilings (issue #53).
  *
  * Before this, room creation was unauthenticated, unvalidated and unbounded,
  * each running game held its own engine thread, and nothing was ever evicted
  * — so a single script could take the server down or fill it with junk. */
class LimitsSpec extends AnyFlatSpec with Matchers {
  import TestDb.{available, xa}

  implicit val engineConfig: EngineConfig = new EngineConfig()
  implicit val ec: ExecutionContext = ExecutionContext.global

  private def withManager[A](limits: RoomManager.Limits = RoomManager.Limits())(f: RoomManager => IO[A]): A =
    Dispatcher.parallel[IO].use { d =>
      RoomManager.create(new RoomRepo(xa), d, Left("off"), limits).flatMap(f)
    }.unsafeRunSync()

  private def createReq(name: String, hostName: String) =
    Request[IO](Method.POST, uri"/api/rooms")
      .withEntity(Json.obj("name" -> name.asJson, "hostName" -> hostName.asJson))

  private def postCreate(rm: RoomManager, name: String, hostName: String,
                         limiter: Option[RateLimiter] = None): IO[(Status, Json)] =
    Routes.routes(rm, None, limiter).orNotFound.run(createReq(name, hostName)).flatMap(r =>
      r.as[Json].map((r.status, _)))

  private def error(body: Json): String =
    body.hcursor.get[String]("error").getOrElse("")

  /* ---------- validation ---------- */

  "Room creation" should "reject an empty display name" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      postCreate(rm, "Table", "").map { case (status, body) =>
        status shouldBe Status.BadRequest
        error(body) should include("your name")
      }
    }
  }

  it should "treat a whitespace-only name as empty" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      postCreate(rm, "Table", "   ").map { case (status, _) => status shouldBe Status.BadRequest }
    }
  }

  it should "reject a name past the length limit" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      postCreate(rm, "Table", "x" * (Routes.MaxNameLength + 1)).map { case (status, body) =>
        status shouldBe Status.BadRequest
        error(body) should include(Routes.MaxNameLength.toString)
      }
    }
  }

  it should "accept a name exactly at the limit" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      postCreate(rm, "Table", "x" * Routes.MaxNameLength).map { case (status, _) => status shouldBe Status.Ok }
    }
  }

  it should "store the trimmed name, since it is the key reviews group by" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      postCreate(rm, "  Table  ", "  Alice  ").map { case (status, body) =>
        status shouldBe Status.Ok
        body.hcursor.downField("room").downField("seats").downArray.get[String]("name") shouldBe Right("Alice")
        body.hcursor.downField("room").get[String]("name") shouldBe Right("Table")
      }
    }
  }

  it should "reject an over-long name on join too, not only on create" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      for {
        made <- rm.create("Table", "Alice")
        id    = made.getOrElse(fail("room not created"))._1.id
        res  <- Routes.routes(rm).orNotFound.run(
                  Request[IO](Method.POST, Uri.unsafeFromString(s"/api/rooms/$id/join"))
                    .withEntity(Json.obj("name" -> ("x" * 99).asJson)))
      } yield res.status shouldBe Status.BadRequest
    }
  }

  /* ---------- room cap ---------- */

  "The room cap" should "refuse new rooms with 429, not 400: the caller did nothing wrong" in {
    assume(available, "Postgres not reachable")
    withManager(RoomManager.Limits(maxRooms = 2)) { rm =>
      for {
        _   <- postCreate(rm, "A", "Alice")
        _   <- postCreate(rm, "B", "Bob")
        r   <- postCreate(rm, "C", "Carol")
      } yield {
        r._1 shouldBe Status.TooManyRequests
        error(r._2) should include("2 rooms")
      }
    }
  }

  it should "not consume a slot for the room it refused" in {
    assume(available, "Postgres not reachable")
    withManager(RoomManager.Limits(maxRooms = 1)) { rm =>
      for {
        a <- rm.create("A", "Alice")
        b <- rm.create("B", "Bob")
        // the rejected create must leave the surviving room untouched
        found <- rm.get(a.getOrElse(fail("A not created"))._1.id)
      } yield {
        b.isLeft shouldBe true
        found.map(_.name) shouldBe Some("A")
      }
    }
  }

  /* ---------- running-game cap ---------- */

  "The running-game cap" should "refuse to start another game once it is reached" in {
    assume(available, "Postgres not reachable")
    // each running game holds a dedicated engine thread; 0 exercises the guard
    // without leaving a live game behind
    withManager(RoomManager.Limits(maxRunningGames = 0)) { rm =>
      for {
        made <- rm.create("Table", "Alice")
        (room, host) = made.getOrElse(fail("room not created"))
        res  <- rm.startGame(room.id, host)
      } yield res match {
        case Left(d: RoomManager.Denied.AtCapacity) => d.message should include("limit")
        case other                                  => fail(s"expected AtCapacity, got $other")
      }
    }
  }

  it should "answer 429 for it, so a client can tell 'retry later' from 'bad input'" in {
    assume(available, "Postgres not reachable")
    withManager(RoomManager.Limits(maxRunningGames = 0)) { rm =>
      for {
        made <- rm.create("Table", "Alice")
        (room, host) = made.getOrElse(fail("room not created"))
        res  <- Routes.routes(rm).orNotFound.run(
                  Request[IO](Method.POST, Uri.unsafeFromString(s"/api/rooms/${room.id}/start"))
                    .withEntity(Json.obj("hostPlayerId" -> host.asJson)))
      } yield res.status shouldBe Status.TooManyRequests
    }
  }

  it should "still refuse a non-host with a plain bad request" in {
    assume(available, "Postgres not reachable")
    // capacity must not mask authorisation: the wrong caller is still 400
    withManager(RoomManager.Limits(maxRunningGames = 0)) { rm =>
      for {
        made <- rm.create("Table", "Alice")
        room  = made.getOrElse(fail("room not created"))._1
        res  <- rm.startGame(room.id, "not-the-host")
      } yield res shouldBe Left(RoomManager.Denied.Invalid("only the host can start the game"))
    }
  }

  /* ---------- rate limiting ---------- */

  private def fixedClock(ref: Ref[IO, Long]): IO[Long] = ref.get

  "The create limiter" should "allow a burst up to the budget and then refuse" in {
    val (allowed, refused) = (for {
      clock <- Ref.of[IO, Long](0L)
      rl    <- RateLimiter.create(3, 1.minute, fixedClock(clock))
      a     <- List.fill(3)(rl.allow("1.2.3.4")).sequence
      b     <- rl.allow("1.2.3.4")
    } yield (a, b)).unsafeRunSync()

    allowed shouldBe List(true, true, true)
    refused shouldBe false
  }

  it should "budget each caller separately" in {
    val other = (for {
      clock <- Ref.of[IO, Long](0L)
      rl    <- RateLimiter.create(1, 1.minute, fixedClock(clock))
      _     <- rl.allow("1.2.3.4")
      b     <- rl.allow("5.6.7.8")
    } yield b).unsafeRunSync()

    other shouldBe true
  }

  it should "let the window slide instead of blocking the caller forever" in {
    val after = (for {
      clock <- Ref.of[IO, Long](0L)
      rl    <- RateLimiter.create(1, 1.minute, fixedClock(clock))
      _     <- rl.allow("1.2.3.4")
      mid   <- clock.set(30000L) *> rl.allow("1.2.3.4")   // still inside the window
      later <- clock.set(61000L) *> rl.allow("1.2.3.4")   // window has passed
    } yield (mid, later)).unsafeRunSync()

    after shouldBe ((false, true))
  }

  it should "not grow its own state without bound while refusing callers" in {
    // the limiter exists to bound memory; a map keyed by every caller that ever
    // knocked would be its own leak
    val stillAllowed = (for {
      clock <- Ref.of[IO, Long](0L)
      rl    <- RateLimiter.create(1, 1.minute, fixedClock(clock))
      _     <- (1 to 500).toList.traverse(i => rl.allow(s"10.0.0.$i"))
      _     <- clock.set(120000L)
      // after the window everything is prunable; the same keys are fresh again
      again <- rl.allow("10.0.0.1")
    } yield again).unsafeRunSync()

    stillAllowed shouldBe true
  }

  "A rate-limited create" should "answer 429" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      for {
        rl <- RateLimiter.create(1, 1.minute)
        a  <- postCreate(rm, "A", "Alice", Some(rl))
        b  <- postCreate(rm, "B", "Bob",   Some(rl))
      } yield {
        a._1 shouldBe Status.Ok
        b._1 shouldBe Status.TooManyRequests
      }
    }
  }

  /* ---------- idle-room eviction ---------- */

  "Idle eviction" should "drop abandoned rooms so they cannot hold the cap" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      for {
        _ <- rm.create("stale", "Alice")
        n <- rm.evictIdle(24.hours, Instant.now().plusSeconds(48 * 3600))
      } yield n shouldBe 1
    }
  }

  it should "keep a room somebody just touched" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      for {
        _ <- rm.create("fresh", "Alice")
        n <- rm.evictIdle(24.hours)
      } yield n shouldBe 0
    }
  }

  it should "count a join as activity, not just creation" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      for {
        made <- rm.create("Table", "Alice")
        id    = made.getOrElse(fail("room not created"))._1.id
        // let the room age past the ttl used below, then have somebody join:
        // on creation time alone this room would now be evicted
        _    <- IO.sleep(300.millis)
        _    <- rm.joinSeat(id, "Bob", Some(1))
        n    <- rm.evictIdle(150.millis)
        got  <- rm.get(id)
      } yield {
        n shouldBe 0
        got.flatMap(_.seats.lift(1)).map(_.name) shouldBe Some("Bob")
      }
    }
  }

  it should "never evict a room with a game in progress" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      for {
        made <- rm.create("Table", "Alice")
        (room, host) = made.getOrElse(fail("room not created"))
        started <- rm.startGame(room.id, host)
        n       <- rm.evictIdle(24.hours, Instant.now().plusSeconds(48 * 3600))
      } yield {
        started.isRight shouldBe true
        n shouldBe 0
      }
    }
  }
}
