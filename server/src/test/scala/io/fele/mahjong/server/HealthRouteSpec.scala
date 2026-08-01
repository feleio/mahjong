package io.fele.mahjong.server

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import io.circe.Json
import io.fele.app.mahjong.{Config => EngineConfig}
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.implicits._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.ExecutionContext

/** /api/health must expose the game-recording state: 200/ok when the record
  * repo is wired and the DB answers, 503/degraded when recording is off —
  * the silent-outage mode fixed in #34. */
class HealthRouteSpec extends AnyFlatSpec with Matchers {
  import TestDb.{available, repo, xa}

  implicit val engineConfig: EngineConfig = new EngineConfig()
  implicit val ec: ExecutionContext = ExecutionContext.global

  private def healthResponse(gameRepo: Either[String, GameRecordRepo]): (Status, Json) =
    Dispatcher.parallel[IO].use { dispatcher =>
      for {
        rm   <- RoomManager.create(new RoomRepo(xa), dispatcher, gameRepo)
        resp <- Routes.routes(rm).orNotFound.run(Request[IO](Method.GET, uri"/api/health"))
        body <- resp.as[Json]
      } yield (resp.status, body)
    }.unsafeRunSync()

  private def field(body: Json, name: String): Json =
    body.hcursor.downField(name).focus.getOrElse(Json.Null)

  "GET /api/health" should "report ok with a games-recorded count when recording is live" in {
    assume(available, "Postgres not reachable")
    repo.init.unsafeRunSync()

    val (status, body) = healthResponse(Right(repo))
    status shouldBe Status.Ok
    field(body, "status").asString shouldBe Some("ok")
    field(body, "recording").asBoolean shouldBe Some(true)
    field(body, "gamesRecorded").asNumber.flatMap(_.toLong).getOrElse(-1L) should be >= 0L
    field(body, "recordingError") shouldBe Json.Null
  }

  it should "report degraded with 503 when recording is disabled" in {
    assume(available, "Postgres not reachable")

    val (status, body) = healthResponse(Left("init failed at boot: simulated"))
    status shouldBe Status.ServiceUnavailable
    field(body, "status").asString shouldBe Some("degraded")
    field(body, "recording").asBoolean shouldBe Some(false)
    // The boot-failure cause must surface in the health body, not only in logs.
    field(body, "recordingError").asString.getOrElse("") should include("simulated")
  }
}
