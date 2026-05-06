package io.fele.mahjong.server

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all._
import io.circe.generic.auto._
import io.circe.syntax._
import io.fele.mahjong.server.Models._
import org.http4s._
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.io._

object Routes {

  case class CreateRoomReq(name: String, hostName: String)
  case class CreateRoomResp(room: Room, hostPlayerId: PlayerId)
  case class JoinReq(name: String, seatIndex: Option[Int])
  case class JoinResp(room: Room, seat: Int, playerId: PlayerId)
  case class SetSeatReq(hostPlayerId: PlayerId, seatIndex: Int, kind: SeatKind)
  case class StartReq(hostPlayerId: PlayerId)
  case class ErrResp(error: String)

  /** CORS for the Next.js dev server. Adds the headers when the inner routes
    * matched the request, and answers OPTIONS preflight directly. */
  private val corsHeaders: Headers = Headers(
    Header.Raw(org.typelevel.ci.CIString("Access-Control-Allow-Origin"),  "*"),
    Header.Raw(org.typelevel.ci.CIString("Access-Control-Allow-Methods"), "GET,POST,PATCH,DELETE,OPTIONS"),
    Header.Raw(org.typelevel.ci.CIString("Access-Control-Allow-Headers"), "Content-Type")
  )

  def withCors(inner: HttpRoutes[IO]): HttpRoutes[IO] =
    HttpRoutes[IO] { req =>
      if (req.method == Method.OPTIONS && req.uri.path.toString.startsWith("/api"))
        OptionT.pure[IO](Response[IO](Status.Ok).withHeaders(corsHeaders))
      else
        inner.run(req).map(_.withHeaders(corsHeaders))
    }

  def routes(rm: RoomManager): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case GET -> Root / "api" / "health" =>
      Ok(Map("status" -> "ok").asJson)

    /* List rooms */
    case GET -> Root / "api" / "rooms" =>
      rm.list.flatMap(rs => Ok(rs.asJson))

    /* Get a single room */
    case GET -> Root / "api" / "rooms" / id =>
      rm.get(id).flatMap {
        case Some(r) => Ok(r.asJson)
        case None    => NotFound(ErrResp("room not found").asJson)
      }

    /* Create a room. Body: { name, hostName } */
    case req @ POST -> Root / "api" / "rooms" =>
      req.as[CreateRoomReq].flatMap { body =>
        rm.create(body.name, body.hostName).flatMap { case (room, hostId) =>
          Ok(CreateRoomResp(room, hostId).asJson)
        }
      }

    /* Join a room as a human guest */
    case req @ POST -> Root / "api" / "rooms" / id / "join" =>
      req.as[JoinReq].flatMap { body =>
        rm.joinSeat(id, body.name, body.seatIndex).flatMap {
          case Right((room, seat, pid)) => Ok(JoinResp(room, seat, pid).asJson)
          case Left(err)                => BadRequest(ErrResp(err).asJson)
        }
      }

    /* Host changes a seat */
    case req @ PATCH -> Root / "api" / "rooms" / id / "seat" =>
      req.as[SetSeatReq].flatMap { body =>
        rm.setSeatKind(id, body.hostPlayerId, body.seatIndex, body.kind).flatMap {
          case Right(r)  => Ok(r.asJson)
          case Left(err) => BadRequest(ErrResp(err).asJson)
        }
      }

    /* Host starts the game */
    case req @ POST -> Root / "api" / "rooms" / id / "start" =>
      req.as[StartReq].flatMap { body =>
        rm.startGame(id, body.hostPlayerId).flatMap {
          case Right(r)  => Ok(r.asJson)
          case Left(err) => BadRequest(ErrResp(err).asJson)
        }
      }
  }
}
