package io.fele.mahjong.server

import cats.effect.IO
import cats.syntax.all._
import fs2.{Pipe, Stream}
import io.circe.parser.parse
import io.circe.syntax._
import io.fele.app.mahjong.{ChowPosition => CP, Tile}
import io.fele.mahjong.server.Models._
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.server.websocket.WebSocketBuilder2
import org.http4s.websocket.WebSocketFrame

object WsRoutes {

  /** WebSocket endpoint:
    *
    *   /ws/rooms/:id?seat=<n>&player=<playerId>
    *
    * - `seat` and `player` are optional. If both supplied and they match a
    *   human seat in the room, this connection can submit actions for that seat.
    *   Otherwise the connection is read-only (a spectator). */
  /** A websocket upgrade is not a CORS-protected request: the browser sends it
    * cross-site without a preflight, so the CORS middleware on the REST routes
    * cannot defend it. Any page a player visits could otherwise open a socket
    * to a room they hold credentials for. Hence an explicit check here.
    *
    * A request with no `Origin` is allowed through: that is a non-browser
    * client (curl, a test, a native app), which the allowlist was never
    * protecting against — its purpose is to stop *other websites* from acting
    * as the user. */
  private def originAllowed(req: org.http4s.Request[IO], policy: OriginPolicy): Boolean =
    policy.allowsAll ||
      req.headers.get(org.typelevel.ci.CIString("Origin")).forall(h => policy.permits(h.head.value))

  def routes(rm: RoomManager, wsb: WebSocketBuilder2[IO],
             policy: OriginPolicy = OriginPolicy.allowAll): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case req @ GET -> Root / "ws" / "rooms" / roomId if !originAllowed(req, policy) =>
      Forbidden("origin not allowed")

    case req @ GET -> Root / "ws" / "rooms" / roomId =>
      val seat   = req.params.get("seat").flatMap(s => scala.util.Try(s.toInt).toOption)
      val player = req.params.get("player")

      rm.get(roomId).flatMap {
        case None => NotFound("room not found")
        case Some(room) =>
          val canonicalId = room.id
          val authorisedSeat: Option[Int] = for {
            s <- seat
            p <- player
            seatRow <- room.seats.find(_.index == s)
            if seatRow.kind == SeatKind.Human && seatRow.playerId.contains(p)
          } yield s

          rm.runner(canonicalId).flatMap {
            case None =>
              // Game has not started yet — push a single lobby snapshot and keep the connection open.
              // RoomView, not Room: a spectator socket must not receive the
              // host's or any seat's credential (issue #51)
              val lobby = WebSocketFrame.Text(io.circe.Json.obj(
                "type" -> "lobby".asJson,
                "room" -> RoomView.of(room).asJson
              ).noSpaces)
              val out: Stream[IO, WebSocketFrame] =
                Stream.emit(lobby).covary[IO] ++ Stream.never[IO]
              wsb.build(send = out, receive = _.drain)

            case Some(runner) =>
              val outFrames: Stream[IO, WebSocketFrame] =
                runner.subscribe(authorisedSeat).map(j => WebSocketFrame.Text(j.noSpaces))

              val inSink: Pipe[IO, WebSocketFrame, Unit] = _.evalMap {
                case WebSocketFrame.Text(text, _) =>
                  rm.runner(canonicalId).flatMap { currentRunner =>
                    IO.delay {
                      for {
                        r     <- currentRunner
                        j     <- parse(text).toOption
                        a     <- j.as[Action].toOption
                        seatI <- authorisedSeat
                      } {
                        val ca = WebSocketPlayer.ClientAction(
                          yes      = a.yes,
                          tile     = a.tile.flatMap(t => Models.tileFromWire(t).toOption),
                          chowPos  = a.chowPos.flatMap(p => scala.util.Try(CP.withName(p.toUpperCase)).toOption),
                          promptId = a.promptId
                        )
                        r.submitAction(seatI, ca)
                      }
                    }.void
                  }
                case _ => IO.unit
              }

              wsb.build(send = outFrames, receive = inSink)
          }
      }
  }
}
