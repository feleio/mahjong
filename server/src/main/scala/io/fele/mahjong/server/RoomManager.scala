package io.fele.mahjong.server

import cats.effect.IO
import cats.effect.std.{AtomicCell, Dispatcher}
import cats.syntax.all._
import fs2.concurrent.Topic
import io.circe.Json
import io.circe.syntax._
import io.fele.app.mahjong.Config
import io.fele.mahjong.server.Models._

import java.time.Instant
import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

/**
 * Coordinates room CRUD and the live in-memory game runners. All mutating
 * operations route through an [[AtomicCell]] so concurrent HTTP / WS calls
 * see a consistent view.
 */
class RoomManager private (
  repo:       RoomRepo,
  gameRepo:   Either[String, GameRecordRepo],
  dispatcher: Dispatcher[IO],
  cell:       AtomicCell[IO, Map[Models.RoomId, RoomManager.Live]]
)(implicit config: Config, ec: ExecutionContext) {
  import RoomManager.Live

  /** Fresh explicit seed per game so the shuffle is reproducible from the record. */
  private def newSeed(): Option[Long] = Some(scala.util.Random.nextLong())

  /** Reason a game with a champion seat cannot start (model missing/broken), if any. */
  private def championBlocked(room: Room): Option[String] =
    if (room.seats.exists(_.kind == SeatKind.AiChampion)) ChampionService.unavailableReason else None

  /** Live game-recording probe: Left(reason) when recording is off or the DB is
    * unreachable right now, Right(total games recorded) when the path works.
    * The probe is time-boxed so a network partition (vs a clean refusal)
    * degrades the health check instead of hanging it. */
  def recordingHealth: IO[Either[String, Long]] = gameRepo match {
    case Left(reason) => IO.pure(Left(s"recording disabled ($reason)"))
    case Right(repo) =>
      repo.countGames.timeout(5.seconds).attempt.map {
        case Right(n) => Right(n)
        case Left(t)  => Left(s"db unreachable: ${t.getMessage}")
      }
  }

  /* --- room CRUD --- */

  def create(name: String, hostName: String): IO[(Room, PlayerId)] = {
    val hostId = UUID.randomUUID().toString
    val seats = List(
      Seat(0, SeatKind.Human, Some(hostId), hostName),
      Seat(1, SeatKind.Open,  None, "Seat 2"),
      Seat(2, SeatKind.Open,  None, "Seat 3"),
      Seat(3, SeatKind.Open,  None, "Seat 4")
    )
    val room = Room(Room.newId(), name, hostId, seats, RoomStatus.Waiting, Instant.now(),
      code = Room.newCode())
    for {
      _     <- repo.upsert(room)
      topic <- Topic[IO, Json]
      _     <- cell.update(_.updated(room.id, Live(room, None, topic)))
    } yield (room, hostId)
  }

  def list: IO[List[Room]] = repo.list

  /** Resolve by room id or by the short join code (case-insensitive). */
  def get(idOrCode: RoomId): IO[Option[Room]] = cell.get.flatMap { m =>
    m.get(idOrCode).map(l => IO.pure(Option(l.room))).getOrElse {
      m.values.find(_.room.code.equalsIgnoreCase(idOrCode)) match {
        case Some(l) => IO.pure(Option(l.room))
        case None    => repo.get(idOrCode)
      }
    }
  }

  /** Room id for an id or short code, for routes that need the canonical id. */
  def resolveId(idOrCode: RoomId): IO[Option[RoomId]] =
    cell.get.map { m =>
      if (m.contains(idOrCode)) Some(idOrCode)
      else m.values.find(_.room.code.equalsIgnoreCase(idOrCode)).map(_.room.id)
    }

  /** Replace the kind of a seat. Only the host can call this and only while waiting. */
  def setSeatKind(roomId: RoomId, hostId: PlayerId, seatIndex: Int, kind: SeatKind): IO[Either[String, Room]] =
    cell.modify { m =>
      m.get(roomId) match {
        case None => (m, Left("room not found"))
        case Some(live) if live.room.hostId != hostId =>
          (m, Left("only the host can change seats"))
        case Some(live) if live.room.status != RoomStatus.Waiting =>
          (m, Left("room is not in waiting state"))
        case Some(live) if seatIndex == 0 =>
          (m, Left("the host's seat cannot be changed"))
        case Some(live) =>
          val seats = live.room.seats.map { s =>
            if (s.index != seatIndex) s
            else kind match {
              case SeatKind.Open  => s.copy(kind = SeatKind.Open,  playerId = None, name = s"Seat ${seatIndex + 1}")
              case SeatKind.Human => s.copy(kind = SeatKind.Human, playerId = None, name = s"Seat ${seatIndex + 1}")
              case ai             => s.copy(kind = ai,             playerId = None, name = aiLabel(ai, seatIndex))
            }
          }
          val updated = live.room.copy(seats = seats)
          (m.updated(roomId, live.copy(room = updated)), Right(updated))
      }
    }.flatMap {
      case Right(r) => repo.upsert(r).as(Right(r))
      case l        => IO.pure(l)
    }

  private def aiLabel(k: SeatKind, idx: Int): String = k match {
    case SeatKind.AiChicken           => "Bot Chicken"
    case SeatKind.AiRandom            => "Bot Random"
    case SeatKind.AiFirstFelix        => "Bot Felix"
    case SeatKind.AiThreePointChicken => "Bot 3PChicken"
    case SeatKind.AiChampion          => "Bot Champion"
    case _                            => s"Seat ${idx + 1}"
  }

  /** Have a guest claim an Open human seat. Returns the seat they got and a player id. */
  def joinSeat(roomId: RoomId, name: String, seatIndex: Option[Int]): IO[Either[String, (Room, Int, PlayerId)]] = {
    val pid = UUID.randomUUID().toString
    cell.modify { m =>
      m.get(roomId) match {
        case None =>
          (m, Left("room not found"))
        case Some(live) if live.room.status != RoomStatus.Waiting =>
          (m, Left("game already started"))
        case Some(live) =>
          val target = seatIndex
            .flatMap(i => live.room.seats.find(_.index == i))
            .orElse(live.room.seats.find(_.kind == SeatKind.Open))
            .filter(_.kind == SeatKind.Open)

          target match {
            case None =>
              (m, Left("no open seat available"))
            case Some(seat) =>
              val newSeats = live.room.seats.map { s =>
                if (s.index == seat.index) s.copy(kind = SeatKind.Human, playerId = Some(pid), name = name)
                else s
              }
              val updated = live.room.copy(seats = newSeats)
              (m.updated(roomId, live.copy(room = updated)), Right((updated, seat.index, pid)))
          }
      }
    }.flatMap {
      case Right(t @ (r, _, _)) => repo.upsert(r).as(Right(t))
      case Left(e)              => IO.pure(Left(e))
    }
  }

  /** The host starts the game once every seat is non-Open. */
  def startGame(roomId: RoomId, hostId: PlayerId): IO[Either[String, Room]] =
    cell.modify { m =>
      m.get(roomId) match {
        case None => (m, Left("room not found"))
        case Some(live) if live.room.hostId != hostId =>
          (m, Left("only the host can start the game"))
        case Some(live) if live.room.status != RoomStatus.Waiting =>
          (m, Left("game already started"))
        case Some(live) if !live.room.isFull && fillEmptySeats(live.room) == live.room =>
          (m, Left("room is not full"))
        case Some(live) if championBlocked(live.room).isDefined =>
          (m, Left(championBlocked(live.room).get))
        case Some(live0) =>
          // the lobby promises empty seats get a bot at start; honour it
          val live = live0.copy(room = fillEmptySeats(live0.room))
          val runner = GameRunner.create(live.room.id, live.room.seats, newSeed(), live.topic, dispatcher,
            onFinished = onGameFinished(roomId), recordRepo = gameRepo.toOption,
            dealerSeat = live.room.gamesPlayed % 4,   // dealer moves on each game
            balances = live.room.balances, gamesPlayed = live.room.gamesPlayed,
            pacingMs = pacingFor(live.room))
          val updated = live.room.copy(status = RoomStatus.Playing)
          (m.updated(roomId, live.copy(room = updated, runner = Some(runner))), Right((updated, runner)))
      }
    }.flatMap {
      case Right((r, runner)) => IO.delay(runner.start()) *> repo.upsert(r).as(Right(r))
      case Left(e)            => IO.pure(Left(e))
    }

  /** Mark a human seat as ready for the next game; bots are also auto-readied. */
  def markReady(roomId: RoomId, playerId: PlayerId): IO[Either[String, Set[Int]]] =
    cell.modify { m =>
      m.get(roomId) match {
        case None => (m, IO.pure(Left("room not found"): Either[String, Set[Int]]))
        case Some(live) =>
          live.room.seats.find(_.playerId.contains(playerId)) match {
            case None => (m, IO.pure(Left("player not found in this room")))
            case Some(seat) =>
              val aiSeats  = live.room.seats.filter(s => s.kind != SeatKind.Human && s.kind != SeatKind.Open).map(_.index).toSet
              val newReady = live.readySeats + seat.index ++ aiSeats
              val io = live.topic.publish1(
                Json.obj("type" -> "ready_update".asJson, "readySeats" -> newReady.toList.sorted.asJson)
              ).void.as(Right(newReady): Either[String, Set[Int]])
              (m.updated(roomId, live.copy(readySeats = newReady)), io)
          }
      }
    }.flatten

  /** Seat a bot in every open seat so a lone player can just press start.
    * Prefers the champion — playing it is the point — but falls back when its
    * model is missing, since an unstartable table is worse than a weaker bot. */
  private def fillEmptySeats(room: Room): Room = {
    val botKind =
      if (ChampionService.unavailableReason.isEmpty) SeatKind.AiChampion else SeatKind.AiFirstFelix
    if (!room.seats.exists(_.kind == SeatKind.Open)) room
    else room.copy(seats = room.seats.map { s =>
      if (s.kind != SeatKind.Open) s
      else s.copy(kind = botKind, playerId = None, name = aiLabel(botKind, s.index))
    })
  }

  /** Only pace the table when a person is watching; bot-only games (tests,
    * evals) must stay as fast as the engine can run. */
  private def pacingFor(room: Room): Long =
    if (room.seats.exists(_.kind == SeatKind.Human)) 600L else 0L

  /** When a game ends: bank the per-seat money into the room's running totals,
    * count the game, then do the usual auto-ready. */
  private def onGameFinished(roomId: RoomId): IO[Unit] =
    cell.modify { m =>
      m.get(roomId) match {
        case None => (m, IO.unit)
        case Some(live) =>
          val delta = live.runner
            .flatMap(r => r.state.winnersInfo)
            .map(_.winnersBalance.sortBy(_.id).map(_.amount))
            .getOrElse(List(0, 0, 0, 0))
          val updated = live.room.copy(
            balances    = live.room.balances.zipAll(delta, 0, 0).map { case (a, b) => a + b },
            gamesPlayed = live.room.gamesPlayed + 1
          )
          (m.updated(roomId, live.copy(room = updated)), repo.upsert(updated).void)
      }
    }.flatten *> autoReadyBots(roomId)

  /** Auto-ready all AI seats when a game ends (used as onFinished callback). */
  private def autoReadyBots(roomId: RoomId): IO[Unit] =
    cell.modify { m =>
      m.get(roomId) match {
        case None => (m, IO.unit)
        case Some(live) =>
          val aiSeats  = live.room.seats.filter(s => s.kind != SeatKind.Human && s.kind != SeatKind.Open).map(_.index).toSet
          val newReady = live.readySeats ++ aiSeats
          val io = live.topic.publish1(
            Json.obj("type" -> "ready_update".asJson, "readySeats" -> newReady.toList.sorted.asJson)
          ).void *> markRoomFinished(roomId)
          (m.updated(roomId, live.copy(readySeats = newReady)), io)
      }
    }.flatten

  private def markRoomFinished(roomId: RoomId): IO[Unit] =
    cell.modify { m =>
      m.get(roomId) match {
        case None => (m, IO.unit)
        case Some(live) if live.room.status == RoomStatus.Playing =>
          val updated = live.room.copy(status = RoomStatus.Finished)
          (m.updated(roomId, live.copy(room = updated)), repo.upsert(updated).void)
        case _ => (m, IO.unit)
      }
    }.flatten

  /** Host starts the next game once all seats (incl. bots) are ready. */
  def startNextGame(roomId: RoomId, hostId: PlayerId): IO[Either[String, Room]] =
    cell.modify { m =>
      m.get(roomId) match {
        case None => (m, IO.pure(Left("room not found"): Either[String, Room]))
        case Some(live) if live.room.hostId != hostId =>
          (m, IO.pure(Left("only the host can start the game")))
        case Some(live) if live.room.status != RoomStatus.Finished =>
          (m, IO.pure(Left("game has not finished yet")))
        case Some(live) if live.readySeats.size < 4 =>
          (m, IO.pure(Left("not all seats are ready")))
        case Some(live) if championBlocked(live.room).isDefined =>
          (m, IO.pure(Left(championBlocked(live.room).get): Either[String, Room]))
        case Some(live) =>
          val runner  = GameRunner.create(live.room.id, live.room.seats, newSeed(), live.topic, dispatcher,
            onFinished = onGameFinished(roomId), recordRepo = gameRepo.toOption,
            dealerSeat = live.room.gamesPlayed % 4,
            balances = live.room.balances, gamesPlayed = live.room.gamesPlayed,
            pacingMs = pacingFor(live.room))
          val updated = live.room.copy(status = RoomStatus.Playing)
          val io = IO.delay(runner.start()) *>
            live.topic.publish1(Json.obj("type" -> "ready_update".asJson, "readySeats" -> Json.arr())).void *>
            repo.upsert(updated).as(Right(updated): Either[String, Room])
          (m.updated(roomId, live.copy(room = updated, runner = Some(runner), readySeats = Set.empty)), io)
      }
    }.flatten

  def runner(id: RoomId): IO[Option[GameRunner]] = cell.get.map(_.get(id).flatMap(_.runner))

  /** Restore in-memory entries for any rooms persisted in Postgres. */
  def restoreFromDb: IO[Unit] = repo.list.flatMap { rooms =>
    rooms.traverse_ { r =>
      Topic[IO, Json].flatMap { t =>
        val restored = if (r.status == RoomStatus.Playing) r.copy(status = RoomStatus.Finished) else r
        cell.update(_.updated(r.id, Live(restored, None, t))) *>
          (if (restored ne r) repo.upsert(restored) else IO.unit)
      }
    }
  }
}

object RoomManager {
  case class Live(room: Models.Room, runner: Option[GameRunner], topic: Topic[IO, Json], readySeats: Set[Int] = Set.empty)

  def create(repo: RoomRepo, dispatcher: Dispatcher[IO],
             gameRepo: Either[String, GameRecordRepo] = Left("not configured"))
            (implicit config: Config, ec: ExecutionContext): IO[RoomManager] =
    AtomicCell[IO].of(Map.empty[Models.RoomId, Live]).map { cell =>
      new RoomManager(repo, gameRepo, dispatcher, cell)
    }
}
