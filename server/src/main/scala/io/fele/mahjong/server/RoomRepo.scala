package io.fele.mahjong.server

import cats.effect.IO
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

  def init: IO[Unit] = sql"""
    CREATE TABLE IF NOT EXISTS rooms (
      id          TEXT PRIMARY KEY,
      name        TEXT NOT NULL,
      host_id     TEXT NOT NULL,
      seats_json  TEXT NOT NULL,
      status      TEXT NOT NULL,
      created_at  TIMESTAMPTZ NOT NULL DEFAULT now()
    )
  """.update.run.transact(xa).void

  def upsert(room: Room): IO[Unit] = {
    val seatsJson = room.seats.asJson.noSpaces
    val status    = RoomStatus.toWire(room.status)
    sql"""
      INSERT INTO rooms (id, name, host_id, seats_json, status, created_at)
      VALUES (${room.id}, ${room.name}, ${room.hostId}, $seatsJson, $status, ${room.createdAt})
      ON CONFLICT (id) DO UPDATE
      SET name       = EXCLUDED.name,
          host_id    = EXCLUDED.host_id,
          seats_json = EXCLUDED.seats_json,
          status     = EXCLUDED.status
    """.update.run.transact(xa).void
  }

  def get(id: RoomId): IO[Option[Room]] =
    sql"""SELECT id, name, host_id, seats_json, status, created_at FROM rooms WHERE id = $id"""
      .query[(String, String, String, String, String, Instant)]
      .option
      .transact(xa)
      .map(_.flatMap(rowToRoom))

  def list: IO[List[Room]] =
    sql"""SELECT id, name, host_id, seats_json, status, created_at FROM rooms ORDER BY created_at DESC LIMIT 200"""
      .query[(String, String, String, String, String, Instant)]
      .to[List]
      .transact(xa)
      .map(_.flatMap(rowToRoom))

  def delete(id: RoomId): IO[Unit] =
    sql"""DELETE FROM rooms WHERE id = $id""".update.run.transact(xa).void

  private def rowToRoom(row: (String, String, String, String, String, Instant)): Option[Room] = {
    val (id, name, host, seatsJson, status, createdAt) = row
    for {
      seats <- decode[List[Seat]](seatsJson).toOption
      st    <- RoomStatus.fromString(status)
    } yield Room(id, name, host, seats, st, createdAt)
  }
}
