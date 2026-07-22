package io.fele.mahjong.server

import cats.effect.IO
import cats.syntax.all._
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import io.circe.parser.decode
import io.circe.syntax._
import io.fele.app.mahjong.WinnersInfo
import io.fele.mahjong.server.Models._

import java.time.Instant

/** Final result of a recorded game. `drawn` means the wall was exhausted with
  * no winner. Seat indices coincide with engine player ids. */
case class OutcomeWinner(seat: Int, score: Int)
object OutcomeWinner {
  implicit val enc: Encoder[OutcomeWinner] = deriveEncoder
  implicit val dec: Decoder[OutcomeWinner] = deriveDecoder
}

case class GameOutcome(
  drawn:       Boolean,
  isSelfWin:   Boolean,
  winningTile: Option[String],
  loserSeat:   Option[Int],
  winners:     List[OutcomeWinner]
)
object GameOutcome {
  implicit val enc: Encoder[GameOutcome] = deriveEncoder
  implicit val dec: Decoder[GameOutcome] = deriveDecoder

  def from(wi: Option[WinnersInfo]): GameOutcome = wi match {
    case None => GameOutcome(drawn = true, isSelfWin = false, None, None, Nil)
    case Some(w) => GameOutcome(
      drawn       = false,
      isSelfWin   = w.isSelfWin,
      winningTile = Some(Models.tileToWire(w.winningTile)),
      loserSeat   = w.loserId,
      winners     = w.winners.toList.sortBy(_.id).map(x => OutcomeWinner(x.id, x.score))
    )
  }
}

object GameRecordStatus {
  val InProgress = "in_progress"
  val Finished   = "finished"
  val Aborted    = "aborted"
}

case class GameRecordRow(
  id:         String,
  roomId:     RoomId,
  seats:      List[Seat],
  seed:       Option[Long],
  wall:       List[String],          // 136 tiles in draw order; slice(13*i, 13*(i+1)) = seat i's deal
  status:     String,
  outcome:    Option[GameOutcome],
  startedAt:  Instant,
  finishedAt: Option[Instant]
)

case class GameEventRow(
  gameId:       String,
  seq:          Int,
  eventType:    String,               // start | resume | draw | discard | kong | pong | chow | end
  seat:         Option[Int],          // acting seat (kong/pong/chow: the claiming seat)
  sourceSeat:   Option[Int],          // seat whose discard was claimed (== seat for self-kong)
  tile:         Option[String],
  chowPosition: Option[String],       // LEFT | MIDDLE | RIGHT
  ts:           Instant
)

/** Postgres persistence of complete per-game event streams (issue #30).
  *
  * Events, not observations: the wall + seat kinds + the ordered event stream
  * are sufficient for the engine to deterministically replay a game and
  * reconstruct every hidden hand at every decision point, at any obs version.
  */
class GameRecordRepo(xa: Transactor[IO]) {

  def init: IO[Unit] = {
    val games = sql"""
      CREATE TABLE IF NOT EXISTS game_records (
        id           TEXT PRIMARY KEY,
        room_id      TEXT NOT NULL,
        seats_json   TEXT NOT NULL,
        seed         BIGINT,
        wall_json    TEXT NOT NULL,
        status       TEXT NOT NULL,
        outcome_json TEXT,
        started_at   TIMESTAMPTZ NOT NULL,
        finished_at  TIMESTAMPTZ
      )
    """.update.run
    val gamesIdx = sql"""
      CREATE INDEX IF NOT EXISTS game_records_room_idx ON game_records (room_id)
    """.update.run
    val events = sql"""
      CREATE TABLE IF NOT EXISTS game_events (
        game_id       TEXT NOT NULL REFERENCES game_records(id) ON DELETE CASCADE,
        seq           INT NOT NULL,
        event_type    TEXT NOT NULL,
        seat          INT,
        source_seat   INT,
        tile          TEXT,
        chow_position TEXT,
        ts            TIMESTAMPTZ NOT NULL,
        PRIMARY KEY (game_id, seq)
      )
    """.update.run
    (games *> gamesIdx *> events).transact(xa).void
  }

  def insertGame(
    id:        String,
    roomId:    RoomId,
    seats:     List[Seat],
    seed:      Option[Long],
    wall:      List[String],
    startedAt: Instant
  ): IO[Unit] = {
    val seatsJson = seats.asJson.noSpaces
    val wallJson  = wall.asJson.noSpaces
    val status    = GameRecordStatus.InProgress
    sql"""
      INSERT INTO game_records (id, room_id, seats_json, seed, wall_json, status, started_at)
      VALUES ($id, $roomId, $seatsJson, $seed, $wallJson, $status, $startedAt)
    """.update.run.transact(xa).void
  }

  def insertEvent(e: GameEventRow): IO[Unit] =
    sql"""
      INSERT INTO game_events (game_id, seq, event_type, seat, source_seat, tile, chow_position, ts)
      VALUES (${e.gameId}, ${e.seq}, ${e.eventType}, ${e.seat}, ${e.sourceSeat}, ${e.tile}, ${e.chowPosition}, ${e.ts})
    """.update.run.transact(xa).void

  def finishGame(id: String, outcome: GameOutcome, finishedAt: Instant): IO[Unit] = {
    val outcomeJson = outcome.asJson.noSpaces
    val status      = GameRecordStatus.Finished
    sql"""
      UPDATE game_records SET status = $status, outcome_json = $outcomeJson, finished_at = $finishedAt
      WHERE id = $id
    """.update.run.transact(xa).void
  }

  def abortGame(id: String, finishedAt: Instant): IO[Unit] = {
    val status = GameRecordStatus.Aborted
    sql"""
      UPDATE game_records SET status = $status, finished_at = $finishedAt
      WHERE id = $id AND status = ${GameRecordStatus.InProgress}
    """.update.run.transact(xa).void
  }

  /** Mark any games left in_progress by a previous process as aborted (runners
    * are not restored across restarts). Returns the number of rows touched. */
  def abortStale: IO[Int] = {
    val aborted = GameRecordStatus.Aborted
    sql"""
      UPDATE game_records SET status = $aborted, finished_at = now()
      WHERE status = ${GameRecordStatus.InProgress}
    """.update.run.transact(xa)
  }

  def getGame(id: String): IO[Option[GameRecordRow]] =
    sql"""
      SELECT id, room_id, seats_json, seed, wall_json, status, outcome_json, started_at, finished_at
      FROM game_records WHERE id = $id
    """.query[(String, String, String, Option[Long], String, String, Option[String], Instant, Option[Instant])]
      .option
      .transact(xa)
      .map(_.flatMap(rowToGame))

  def listGames(roomId: Option[RoomId], limit: Int = 200): IO[List[GameRecordRow]] = {
    val base =
      fr"""SELECT id, room_id, seats_json, seed, wall_json, status, outcome_json, started_at, finished_at
           FROM game_records""" ++
        roomId.fold(Fragment.empty)(r => fr"WHERE room_id = $r") ++
        fr"ORDER BY started_at DESC LIMIT $limit"
    base.query[(String, String, String, Option[Long], String, String, Option[String], Instant, Option[Instant])]
      .to[List]
      .transact(xa)
      .map(_.flatMap(rowToGame))
  }

  def eventsFor(gameId: String): IO[List[GameEventRow]] =
    sql"""
      SELECT game_id, seq, event_type, seat, source_seat, tile, chow_position, ts
      FROM game_events WHERE game_id = $gameId ORDER BY seq
    """.query[GameEventRow].to[List].transact(xa)

  /** Deletes the game row; events cascade. */
  def deleteGame(id: String): IO[Unit] =
    sql"""DELETE FROM game_records WHERE id = $id""".update.run.transact(xa).void

  private def rowToGame(
    row: (String, String, String, Option[Long], String, String, Option[String], Instant, Option[Instant])
  ): Option[GameRecordRow] = {
    val (id, roomId, seatsJson, seed, wallJson, status, outcomeJson, startedAt, finishedAt) = row
    for {
      seats   <- decode[List[Seat]](seatsJson).toOption
      wall    <- decode[List[String]](wallJson).toOption
      outcome <- outcomeJson match {
                   case None    => Some(None)
                   case Some(j) => decode[GameOutcome](j).toOption.map(Some(_))
                 }
    } yield GameRecordRow(id, roomId, seats, seed, wall, status, outcome, startedAt, finishedAt)
  }
}
