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
  dealerSeat: Int,                   // seat that takes the first turn (rows written before this column: 0)
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

/** A human prompt that expired, so the engine played a default instead of the
  * player (issue #42).
  *
  * `eventSeq` is the seq the *next* event will carry, which is exactly the
  * event cursor [[GameReplayer]] reports at that decision point — that is what
  * lets a review tell "the player passed" apart from "nobody answered".
  * Decisions that produce no event of their own (a declined claim) are still
  * pinned by (eventSeq, seat, kind), since a seat is offered each claim kind
  * at most once per discarded tile.
  */
case class DecisionTimeoutRow(
  gameId:   String,
  eventSeq: Int,
  seat:     Int,
  kind:     String,                   // discard | win | self_win | kong | self_kong | pong | chow
  ts:       Instant
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
    // Added after the first records existed: rows written before it started at
    // seat 0, which is what the default backfills.
    val dealerCol = sql"""
      ALTER TABLE game_records ADD COLUMN IF NOT EXISTS dealer_seat INT NOT NULL DEFAULT 0
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
    val timeouts = sql"""
      CREATE TABLE IF NOT EXISTS game_decision_timeouts (
        game_id   TEXT NOT NULL REFERENCES game_records(id) ON DELETE CASCADE,
        event_seq INT NOT NULL,
        seat      INT NOT NULL,
        kind      TEXT NOT NULL,
        ts        TIMESTAMPTZ NOT NULL,
        PRIMARY KEY (game_id, event_seq, seat, kind)
      )
    """.update.run
    (games *> gamesIdx *> dealerCol *> events *> timeouts).transact(xa).void
  }

  def insertGame(
    id:        String,
    roomId:    RoomId,
    seats:     List[Seat],
    seed:       Option[Long],
    wall:       List[String],
    dealerSeat: Int,
    startedAt:  Instant
  ): IO[Unit] = {
    val seatsJson = seats.asJson.noSpaces
    val wallJson  = wall.asJson.noSpaces
    val status    = GameRecordStatus.InProgress
    sql"""
      INSERT INTO game_records (id, room_id, seats_json, seed, wall_json, dealer_seat, status, started_at)
      VALUES ($id, $roomId, $seatsJson, $seed, $wallJson, $dealerSeat, $status, $startedAt)
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

  /** Total games recorded. Doubles as a live DB reachability probe for /api/health. */
  def countGames: IO[Long] =
    sql"""SELECT count(*) FROM game_records""".query[Long].unique.transact(xa)

  def getGame(id: String): IO[Option[GameRecordRow]] =
    sql"""
      SELECT id, room_id, seats_json, seed, wall_json, dealer_seat, status, outcome_json, started_at, finished_at
      FROM game_records WHERE id = $id
    """.query[(String, String, String, Option[Long], String, Int, String, Option[String], Instant, Option[Instant])]
      .option
      .transact(xa)
      .map(_.flatMap(rowToGame))

  def listGames(roomId: Option[RoomId], limit: Int = 200): IO[List[GameRecordRow]] = {
    val base =
      fr"""SELECT id, room_id, seats_json, seed, wall_json, dealer_seat, status, outcome_json, started_at, finished_at
           FROM game_records""" ++
        roomId.fold(Fragment.empty)(r => fr"WHERE room_id = $r") ++
        fr"ORDER BY started_at DESC LIMIT $limit"
    base.query[(String, String, String, Option[Long], String, Int, String, Option[String], Instant, Option[Instant])]
      .to[List]
      .transact(xa)
      .map(_.flatMap(rowToGame))
  }

  /** Records that a human prompt expired. Idempotent: a retry of the same
    * decision must not fail the game thread. */
  def insertTimeout(t: DecisionTimeoutRow): IO[Unit] =
    sql"""
      INSERT INTO game_decision_timeouts (game_id, event_seq, seat, kind, ts)
      VALUES (${t.gameId}, ${t.eventSeq}, ${t.seat}, ${t.kind}, ${t.ts})
      ON CONFLICT DO NOTHING
    """.update.run.transact(xa).void

  def timeoutsFor(gameId: String): IO[List[DecisionTimeoutRow]] =
    sql"""
      SELECT game_id, event_seq, seat, kind, ts
      FROM game_decision_timeouts WHERE game_id = $gameId ORDER BY event_seq
    """.query[DecisionTimeoutRow].to[List].transact(xa)

  /** Finished games, newest first, optionally only those whose seats mention
    * `player`. The name match is a JSON substring test used to bound the scan;
    * callers still verify the seat exactly, since a name could appear in
    * another field. */
  def listFinished(player: Option[String], limit: Int): IO[List[GameRecordRow]] = {
    val finished = GameRecordStatus.Finished
    val base =
      fr"""SELECT id, room_id, seats_json, seed, wall_json, dealer_seat, status, outcome_json, started_at, finished_at
           FROM game_records WHERE status = $finished""" ++
        player.fold(Fragment.empty) { p =>
          // strpos, not LIKE: the name is arbitrary user input and would
          // otherwise need wildcard escaping to avoid matching the wrong rows
          val needle = "\"name\":\"" + p + "\""
          fr"AND strpos(seats_json, $needle) > 0"
        } ++
        fr"ORDER BY started_at DESC LIMIT $limit"
    base.query[(String, String, String, Option[Long], String, Int, String, Option[String], Instant, Option[Instant])]
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
    row: (String, String, String, Option[Long], String, Int, String, Option[String], Instant, Option[Instant])
  ): Option[GameRecordRow] = {
    val (id, roomId, seatsJson, seed, wallJson, dealerSeat, status, outcomeJson, startedAt, finishedAt) = row
    for {
      seats   <- decode[List[Seat]](seatsJson).toOption
      wall    <- decode[List[String]](wallJson).toOption
      outcome <- outcomeJson match {
                   case None    => Some(None)
                   case Some(j) => decode[GameOutcome](j).toOption.map(Some(_))
                 }
    } yield GameRecordRow(id, roomId, seats, seed, wall, dealerSeat, status, outcome, startedAt, finishedAt)
  }
}
