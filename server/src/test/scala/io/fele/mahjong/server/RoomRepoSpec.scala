package io.fele.mahjong.server

import cats.effect.unsafe.implicits.global
import io.fele.mahjong.server.Models._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.time.Instant
import java.time.temporal.ChronoUnit
import java.util.UUID

/** Rooms have to survive a restart intact (issue #54).
  *
  * They did not: the table had no column for the join code, the running
  * balances or the games count, so `restoreFromDb` rebuilt every room with an
  * empty code — breaking its /room/CODE link and the join-by-code path — and
  * silently reset the money and the dealer rotation. */
class RoomRepoSpec extends AnyFlatSpec with Matchers {
  import TestDb.{available, xa}

  private val repo = new RoomRepo(xa)

  private def room(seed: Int): Room = Room(
    id     = UUID.randomUUID().toString,
    name   = s"table $seed",
    hostId = UUID.randomUUID().toString,
    seats  = List(
      Seat(0, SeatKind.Human,       Some(UUID.randomUUID().toString), s"Player$seed"),
      Seat(1, SeatKind.AiChampion,  None, "Bot Champion"),
      Seat(2, SeatKind.Open,        None, "Seat 3"),
      Seat(3, SeatKind.AiFirstFelix, None, "Bot Felix")
    ),
    status      = if (seed % 2 == 0) RoomStatus.Waiting else RoomStatus.Finished,
    // Postgres keeps microseconds, so compare at that resolution
    createdAt   = Instant.now().truncatedTo(ChronoUnit.MICROS),
    code        = Room.newCode(),
    balances    = List(seed, -seed, 2 * seed, -2 * seed),
    gamesPlayed = seed
  )

  private def roundTrip(r: Room): Room = {
    repo.init.unsafeRunSync()
    repo.upsert(r).unsafeRunSync()
    repo.get(r.id).unsafeRunSync().getOrElse(fail(s"room ${r.id} did not come back"))
  }

  "A stored room" should "come back exactly as it went in" in {
    assume(available, "Postgres not reachable")
    // every field, across many shapes: a partial round-trip is what caused #54
    (1 to 25).foreach { seed =>
      val original = room(seed)
      roundTrip(original) shouldBe original
    }
  }

  it should "keep the join code, which is how players reach the room" in {
    assume(available, "Postgres not reachable")
    val original = room(3)
    roundTrip(original).code shouldBe original.code
  }

  it should "keep the running money and games count" in {
    assume(available, "Postgres not reachable")
    val original = room(7)
    val restored = roundTrip(original)
    restored.balances shouldBe List(7, -7, 14, -14)
    // the dealer rotates on gamesPlayed % 4; losing it silently reseats the dealer
    restored.gamesPlayed shouldBe 7
  }

  it should "persist later updates, not just the first insert" in {
    assume(available, "Postgres not reachable")
    val original = room(2)
    roundTrip(original)
    val after = original.copy(balances = List(9, 9, 9, 9), gamesPlayed = 4, status = RoomStatus.Finished)
    repo.upsert(after).unsafeRunSync()
    repo.get(original.id).unsafeRunSync() shouldBe Some(after)
  }

  it should "appear in the listing with its code and balances intact" in {
    assume(available, "Postgres not reachable")
    // restoreFromDb reads `list`, so the restore path needs the same fidelity
    val original = room(5)
    roundTrip(original)
    repo.list.unsafeRunSync().find(_.id == original.id) shouldBe Some(original)
  }

  it should "still load when it predates the new columns" in {
    assume(available, "Postgres not reachable")
    import doobie.implicits._
    import doobie.postgres.implicits._
    import io.circe.syntax._
    val legacy    = room(11)
    val seatsJson = legacy.seats.asJson.noSpaces
    val status    = RoomStatus.toWire(legacy.status)
    val createdAt = legacy.createdAt
    repo.init.unsafeRunSync()
    // exactly what an older server wrote: no code, no balances, no count
    sql"""INSERT INTO rooms (id, name, host_id, seats_json, status, created_at)
          VALUES (${legacy.id}, ${legacy.name}, ${legacy.hostId}, $seatsJson, $status, $createdAt)"""
      .update.run.transact(xa).unsafeRunSync()

    val restored = repo.get(legacy.id).unsafeRunSync().getOrElse(fail("legacy room did not load"))
    restored.name shouldBe legacy.name
    restored.code shouldBe ""                       // it never had one to lose
    restored.balances shouldBe List(0, 0, 0, 0)
    restored.gamesPlayed shouldBe 0
  }
}
