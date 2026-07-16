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
  def routes(rm: RoomManager, wsb: WebSocketBuilder2[IO]): HttpRoutes[IO] = HttpRoutes.of[IO] {

    case req @ GET -> Root / "ws" / "rooms" / roomId =>
      val seat   = req.params.get("seat").flatMap(s => scala.util.Try(s.toInt).toOption)
      val player = req.params.get("player")

      rm.get(roomId).flatMap {
        case None => NotFound("room not found")
        case Some(room) =>
          val authorisedSeat: Option[Int] = for {
            s <- seat
            p <- player
            seatRow <- room.seats.find(_.index == s)
            if seatRow.kind == SeatKind.Human && seatRow.playerId.contains(p)
          } yield s

          rm.runner(roomId).flatMap {
            case None =>
              // Game has not started yet — push a single lobby snapshot and keep the connection open.
              val lobby = WebSocketFrame.Text(io.circe.Json.obj(
                "type" -> "lobby".asJson,
                "room" -> room.asJson
              ).noSpaces)
              val out: Stream[IO, WebSocketFrame] =
                Stream.emit(lobby).covary[IO] ++ Stream.never[IO]
              wsb.build(send = out, receive = _.drain)

            case Some(runner) =>
              val outFrames: Stream[IO, WebSocketFrame] =
                runner.subscribe(authorisedSeat).map(j => WebSocketFrame.Text(j.noSpaces))

              val inSink: Pipe[IO, WebSocketFrame, Unit] = _.evalMap {
                case WebSocketFrame.Text(text, _) =>
                  rm.runner(roomId).flatMap { currentRunner =>
                    IO.delay {
                      for {
                        r     <- currentRunner
                        j     <- parse(text).toOption
                        a     <- j.as[Action].toOption
                        seatI <- authorisedSeat
                      } {
                        val ca = WebSocketPlayer.ClientAction(
                          yes     = a.yes,
                          tile    = a.tile.flatMap(t => Models.tileFromWire(t).toOption),
                          chowPos = a.chowPos.flatMap(p => scala.util.Try(CP.withName(p.toUpperCase)).toOption)
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
