package io.fele.mahjong.server

import cats.effect.IO
import cats.syntax.all._
import doobie._
import doobie.implicits._
import doobie.postgres.implicits._
import io.circe.parser.decode
import io.circe.syntax._
import io.fele.mahjong.server.Models._

import java.time.Instant

/** Postgres-backed persistence of room/seat configuration. The active in-memory
  * game runners are not stored — only the room layout is durable so users can
  * reconnect after a process restart. */
class RoomRepo(xa: Transactor[IO]) {

  private type Row = (String, String, String, String, String, Instant, String, String, Int)

  private val columns = fr"id, name, host_id, seats_json, status, created_at, code, balances_json, games_played"

  def init: IO[Unit] = {
    val table = sql"""
      CREATE TABLE IF NOT EXISTS rooms (
        id          TEXT PRIMARY KEY,
        name        TEXT NOT NULL,
        host_id     TEXT NOT NULL,
        seats_json  TEXT NOT NULL,
        status      TEXT NOT NULL,
        created_at  TIMESTAMPTZ NOT NULL DEFAULT now()
      )
    """.update.run
    // Added once the room's identity and running money mattered (#54). Rooms
    // written before this have no code and no history: the defaults say so
    // rather than inventing a code that was never read out to anybody.
    val code     = sql"ALTER TABLE rooms ADD COLUMN IF NOT EXISTS code TEXT NOT NULL DEFAULT ''".update.run
    val balances = sql"ALTER TABLE rooms ADD COLUMN IF NOT EXISTS balances_json TEXT NOT NULL DEFAULT '[0,0,0,0]'".update.run
    val played   = sql"ALTER TABLE rooms ADD COLUMN IF NOT EXISTS games_played INT NOT NULL DEFAULT 0".update.run
    (table *> code *> balances *> played).transact(xa).void
  }

  def upsert(room: Room): IO[Unit] = {
    val seatsJson    = room.seats.asJson.noSpaces
    val balancesJson = room.balances.asJson.noSpaces
    val status       = RoomStatus.toWire(room.status)
    sql"""
      INSERT INTO rooms (id, name, host_id, seats_json, status, created_at, code, balances_json, games_played)
      VALUES (${room.id}, ${room.name}, ${room.hostId}, $seatsJson, $status, ${room.createdAt},
              ${room.code}, $balancesJson, ${room.gamesPlayed})
      ON CONFLICT (id) DO UPDATE
      SET name          = EXCLUDED.name,
          host_id       = EXCLUDED.host_id,
          seats_json    = EXCLUDED.seats_json,
          status        = EXCLUDED.status,
          code          = EXCLUDED.code,
          balances_json = EXCLUDED.balances_json,
          games_played  = EXCLUDED.games_played
    """.update.run.transact(xa).void
  }

  def get(id: RoomId): IO[Option[Room]] =
    (fr"SELECT" ++ columns ++ fr"FROM rooms WHERE id = $id")
      .query[Row]
      .option
      .transact(xa)
      .map(_.flatMap(rowToRoom))

  def list: IO[List[Room]] =
    (fr"SELECT" ++ columns ++ fr"FROM rooms ORDER BY created_at DESC LIMIT 200")
      .query[Row]
      .to[List]
      .transact(xa)
      .map(_.flatMap(rowToRoom))

  def delete(id: RoomId): IO[Unit] =
    sql"""DELETE FROM rooms WHERE id = $id""".update.run.transact(xa).void

  private def rowToRoom(row: Row): Option[Room] = {
    val (id, name, host, seatsJson, status, createdAt, code, balancesJson, gamesPlayed) = row
    for {
      seats <- decode[List[Seat]](seatsJson).toOption
      st    <- RoomStatus.fromString(status)
    } yield Room(id, name, host, seats, st, createdAt, code,
      // a room whose balances row is unreadable still opens, at zero
      decode[List[Int]](balancesJson).getOrElse(List(0, 0, 0, 0)), gamesPlayed)
  }
}
