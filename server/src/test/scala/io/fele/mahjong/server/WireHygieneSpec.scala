package io.fele.mahjong.server

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import io.circe.Json
import io.circe.generic.auto._   // GameListItem is encoded by auto-derivation in Routes
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

/** `hostId` and a seat's `playerId` are bearer credentials: holding one lets
  * you run a room or play someone else's hand (RoomManager.setSeatKind,
  * WsRoutes.authorisedSeat). They used to be serialized to anyone who asked,
  * which made every other access check ornamental — issue #51.
  *
  * These tests are about *absence*, so they assert on the raw JSON rather than
  * on a decoded view: a leak reintroduced by a widened encoder, an added field,
  * or a nested payload has to fail here, not be silently re-typed away. */
class WireHygieneSpec extends AnyFlatSpec with Matchers {
  import TestDb.{available, xa}

  implicit val engineConfig: EngineConfig = new EngineConfig()
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val secretHost = "host-secret-" + UUID.randomUUID()
  private val secretSeat = "seat-secret-" + UUID.randomUUID()

  private val room = Room(
    id      = UUID.randomUUID().toString,
    name    = "Alice's table",
    hostId  = secretHost,
    seats   = List(
      Seat(0, SeatKind.Human,      Some(secretHost), "Alice"),
      Seat(1, SeatKind.Human,      Some(secretSeat), "Bob"),
      Seat(2, SeatKind.AiChampion, None,             "Bot Champion"),
      Seat(3, SeatKind.Open,       None,             "Seat 4")
    ),
    status      = RoomStatus.Waiting,
    createdAt   = Instant.now(),
    code        = "ABC234",
    balances    = List(3, -1, 0, -2),
    gamesPlayed = 2
  )

  /** Every string anywhere in the document — keys, values, nested, in arrays. */
  private def strings(j: Json): List[String] = j.fold(
    jsonNull    = Nil,
    jsonBoolean = _ => Nil,
    jsonNumber  = _ => Nil,
    jsonString  = s => List(s),
    jsonArray   = _.toList.flatMap(strings),
    jsonObject  = o => o.toList.flatMap { case (k, v) => k :: strings(v) }
  )

  private def leaks(j: Json): List[String] =
    strings(j).filter(s => s == secretHost || s == secretSeat || s == "hostId" || s == "playerId")

  private def payloadIsClean(label: String, j: Json): Unit =
    withClue(s"$label leaked a credential: ${j.noSpaces}") { leaks(j) shouldBe empty }

  "RoomView" should "carry no credential and no credential-shaped field name" in {
    payloadIsClean("RoomView", RoomView.of(room).asJson)
  }

  it should "preserve everything the lobby actually renders" in {
    val v = RoomView.of(room)
    v.id shouldBe room.id
    v.name shouldBe room.name
    v.code shouldBe "ABC234"
    v.balances shouldBe List(3, -1, 0, -2)
    v.gamesPlayed shouldBe 2
    v.status shouldBe RoomStatus.Waiting
    v.seats.map(_.name) shouldBe List("Alice", "Bob", "Bot Champion", "Seat 4")
    v.seats.map(_.kind) shouldBe List(SeatKind.Human, SeatKind.Human, SeatKind.AiChampion, SeatKind.Open)
  }

  it should "mark occupancy so the UI can tell a taken seat from an open one without ids" in {
    RoomView.of(room).seats.map(_.occupied) shouldBe List(true, true, false, false)
  }

  it should "point at the host's seat rather than the host's credential" in {
    RoomView.of(room).hostSeat shouldBe 0
  }

  it should "fall back to seat 0 when no seat holds the host id" in {
    // restored-from-DB rooms can have seats reassigned; the marker must not throw
    val orphaned = room.copy(seats = room.seats.map(_.copy(playerId = None)))
    RoomView.of(orphaned).hostSeat shouldBe 0
  }

  "The room's own encoder" should "still carry the credentials it persists" in {
    // Room is the storage/domain type: RoomRepo and GameRecordRepo need the ids.
    // This asserts the split is real — the view is sanitized, the model is not.
    leaks(room.asJson) should not be empty
  }

  "GameListItem" should "not republish the player ids stored in a game record" in {
    val item = Routes.GameListItem(
      id = "g1", roomId = room.id, startedAt = Instant.now(), finishedAt = None,
      seats = room.seats.map(SeatView.of), outcome = None, mySeat = Some(0), myMoney = Some(3)
    )
    payloadIsClean("GameListItem", item.asJson)
  }

  /* ---- route level: what a client can actually pull off the wire ---- */

  private def routes(rm: RoomManager) = Routes.routes(rm).orNotFound

  private def withRoom[A](f: (RoomManager, Json, String) => IO[A]): A =
    Dispatcher.parallel[IO].use { d =>
      for {
        rm   <- RoomManager.create(new RoomRepo(xa), d)
        resp <- routes(rm).run(
                  Request[IO](Method.POST, uri"/api/rooms")
                    .withEntity(Json.obj("name" -> "T".asJson, "hostName" -> "Alice".asJson)))
        body <- resp.as[Json]
        id    = body.hcursor.downField("room").downField("id").as[String].getOrElse("")
        out  <- f(rm, body, id)
      } yield out
    }.unsafeRunSync()

  "POST /api/rooms" should "hand the creator their host credential and publish none" in {
    assume(available, "Postgres not reachable")
    withRoom { (_, body, _) =>
      IO {
        // the creator's own credential arrives in its dedicated field...
        body.hcursor.downField("hostPlayerId").as[String].toOption.map(_.nonEmpty) shouldBe Some(true)
        // ...and nowhere inside the room it publishes
        payloadIsClean("POST /api/rooms room", body.hcursor.downField("room").focus.getOrElse(Json.Null))
      }
    }
  }

  "GET /api/rooms/:id" should "not serve the credentials of the people in the room" in {
    assume(available, "Postgres not reachable")
    withRoom { (rm, _, id) =>
      for {
        _    <- rm.joinSeat(id, "Bob", Some(1))
        resp <- routes(rm).run(Request[IO](Method.GET, Uri.unsafeFromString(s"/api/rooms/$id")))
        body <- resp.as[Json]
      } yield {
        resp.status shouldBe Status.Ok
        // the room really does hold two human credentials at this point
        rm.get(id).unsafeRunSync().get.seats.count(_.playerId.isDefined) shouldBe 2
        payloadIsClean("GET /api/rooms/:id", body)
      }
    }
  }

  "POST /api/rooms/:id/join" should "give the joiner their seat credential and no one else's" in {
    assume(available, "Postgres not reachable")
    withRoom { (rm, _, id) =>
      for {
        resp <- routes(rm).run(
                  Request[IO](Method.POST, Uri.unsafeFromString(s"/api/rooms/$id/join"))
                    .withEntity(Json.obj("name" -> "Bob".asJson, "seatIndex" -> 1.asJson)))
        body <- resp.as[Json]
      } yield {
        body.hcursor.downField("playerId").as[String].toOption.map(_.nonEmpty) shouldBe Some(true)
        payloadIsClean("join room", body.hcursor.downField("room").focus.getOrElse(Json.Null))
      }
    }
  }

  "The room listing" should "be gone: rooms are reachable only by id or code" in {
    assume(available, "Postgres not reachable")
    withRoom { (rm, _, _) =>
      routes(rm).run(Request[IO](Method.GET, uri"/api/rooms")).map { resp =>
        resp.status shouldBe Status.NotFound
      }
    }
  }
}
