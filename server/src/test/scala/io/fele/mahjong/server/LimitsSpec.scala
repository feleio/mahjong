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
import java.util.UUID
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

  it should "hold under a concurrent burst, not just sequential calls" in {
    assume(available, "Postgres not reachable")
    // a cap checked outside the atomic update lets a burst overshoot it, which
    // is exactly the case an attacker produces and a sequential test misses
    withManager(RoomManager.Limits(maxRooms = 5)) { rm =>
      List.range(0, 60).parTraverse(i => rm.create(s"room$i", "Alice")).map { results =>
        results.count(_.isRight) shouldBe 5
        results.count(_.isLeft)  shouldBe 55
      }
    }
  }

  private def persistedRoom(name: String, ageDays: Int): Room = Room(
    id        = UUID.randomUUID().toString,
    name      = name,
    hostId    = UUID.randomUUID().toString,
    seats     = List(Seat(0, SeatKind.Human, Some("h"), "Ghost")),
    status    = RoomStatus.Finished,
    createdAt = Instant.now().minusSeconds(ageDays.toLong * 24 * 3600),
    code      = Room.newCode(), balances = List(0, 0, 0, 0), gamesPlayed = 0)

  "Restoring rooms after a restart" should "not let long-dead rooms hold the cap" in {
    assume(available, "Postgres not reachable")
    // The rooms table outlives any single process. Restoring it unconditionally
    // means that once enough rooms have ever been created, every restart boots
    // at capacity and refuses every new room — a create outage caused by a
    // routine deploy. RoomRepo.list returns up to 200 rows and maxRooms
    // defaults to 200, so the two numbers meet exactly.
    val repo = new RoomRepo(xa)
    val dead = List.tabulate(3)(i => persistedRoom(s"dead$i", ageDays = 30))
    (repo.init *> dead.traverse_(repo.upsert)).unsafeRunSync()

    withManager() { rm =>
      rm.restoreFromDb(24.hours) *> dead.traverse(d => rm.get(d.code)).map { found =>
        // a code only resolves through the in-memory map, so "not found" means
        // these never took a slot
        withClue("long-dead rooms were pulled back into memory: ") { found.flatten shouldBe empty }
      }
    }
  }

  it should "still be able to create a room when the database is full of recent ones" in {
    assume(available, "Postgres not reachable")
    // Observed for real: a dev database with 355 rooms, all created that day,
    // made the server answer 429 to every create immediately after a restart.
    // Age filtering alone does not help when the rows filling the table are
    // themselves recent, so the restore is capped to a share of the ceiling.
    val repo  = new RoomRepo(xa)
    val fresh = List.tabulate(10)(i => persistedRoom(s"fresh$i", ageDays = 0))
    (repo.init *> fresh.traverse_(repo.upsert)).unsafeRunSync()

    withManager(RoomManager.Limits(maxRooms = 4)) { rm =>
      for {
        _       <- rm.restoreFromDb(24.hours)
        n       <- rm.roomCount
        created <- rm.create("after the restart", "Bob")
      } yield {
        withClue("a restart must not consume the whole cap: ") { n should be < 4 }
        withClue("a restart must leave room to create: ") { created.isRight shouldBe true }
      }
    }
  }

  it should "still restore a room that is genuinely in use" in {
    assume(available, "Postgres not reachable")
    // the cheap fix (restore nothing) would also pass the test above; this is
    // what stops it — a reconnecting player must still find their room
    val repo  = new RoomRepo(xa)
    val alive = persistedRoom("still in use", ageDays = 0)
    (repo.init *> repo.upsert(alive)).unsafeRunSync()

    withManager() { rm =>
      rm.restoreFromDb(24.hours) *> rm.get(alive.code).map { found =>
        found.map(_.name) shouldBe Some("still in use")
      }
    }
  }

  /* ---------- running-game cap ---------- */

  "The running-game cap" should "hold under a concurrent burst of starts" in {
    assume(available, "Postgres not reachable")
    // each admitted start spawns a real engine thread, so overshooting here
    // costs threads, not just map entries
    val allowed = 2
    withManager(RoomManager.Limits(maxRunningGames = allowed)) { rm =>
      for {
        made    <- List.range(0, 8).traverse(i => rm.create(s"room$i", "Alice"))
        rooms    = made.collect { case Right((r, host)) => (r.id, host) }
        results <- rooms.parTraverse { case (id, host) => rm.startGame(id, host) }
      } yield {
        rooms.size shouldBe 8
        results.count(_.isRight) shouldBe allowed
      }
    }
  }

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

  "The reclaim rule" should "reclaim an abandoned room whose game has ended" in {
    // The case eviction exists for, and the one an earlier version missed: a
    // finished room keeps its runner attached forever, so any liveness test
    // based on the runner exempts every room that has ever played a game.
    val cutoff = Instant.now().minusSeconds(24 * 3600)
    val stale  = Instant.now().minusSeconds(48 * 3600)
    RoomManager.reclaimable(RoomStatus.Finished, stale, cutoff) shouldBe true
    RoomManager.reclaimable(RoomStatus.Waiting,  stale, cutoff) shouldBe true
  }

  it should "never reclaim a room with a live game, however old" in {
    val cutoff = Instant.now().minusSeconds(24 * 3600)
    val stale  = Instant.now().minusSeconds(365 * 24 * 3600)
    RoomManager.reclaimable(RoomStatus.Playing, stale, cutoff) shouldBe false
  }

  it should "never reclaim a room somebody touched inside the window" in {
    val cutoff = Instant.now().minusSeconds(24 * 3600)
    RoomManager.reclaimable(RoomStatus.Finished, Instant.now(), cutoff) shouldBe false
  }

  it should "forget the evicted room in Postgres too, so a restart cannot refill the cap" in {
    assume(available, "Postgres not reachable")
    withManager() { rm =>
      val repo = new RoomRepo(xa)
      for {
        made <- rm.create("Table", "Alice")
        id    = made.getOrElse(fail("room not created"))._1.id
        _    <- rm.evictIdle(24.hours, Instant.now().plusSeconds(48 * 3600))
        row  <- repo.get(id)
      } yield row shouldBe None
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
