package io.fele.mahjong.server

import cats.effect.IO
import cats.effect.std.Dispatcher
import io.fele.app.mahjong._
import io.fele.mahjong.server.Models._

import java.time.Instant
import java.util.UUID

/** Persists one game's complete event stream via [[GameRecordRepo]].
  *
  * All calls happen on the dedicated game thread (the engine is synchronous),
  * so writes are ordered and each event is durable the moment the decision is
  * made — an abandoned game still keeps every decision up to that point.
  * A DB failure disables recording for the rest of the game instead of
  * crashing it.
  */
class GameRecorder(
  repo:       GameRecordRepo,
  dispatcher: Dispatcher[IO],
  roomId:     RoomId,
  seats:      List[Seat],
  seed:       Option[Long],
  wall:       Seq[Tile]
) {
  val gameId: String = UUID.randomUUID().toString

  private var seq = 0
  private var closed = false
  @volatile private var enabled = true

  private def run(io: IO[Unit], what: String): Unit =
    if (enabled) {
      try dispatcher.unsafeRunSync(io)
      catch {
        case t: Throwable =>
          enabled = false
          println(s"WARN: game recording disabled for game $gameId ($what failed: ${t.getMessage})")
      }
    }

  /** Insert the game row. Must be called before any event is logged. */
  def begin(): Unit =
    run(repo.insertGame(gameId, roomId, seats, seed, wall.map(Models.tileToWire).toList, Instant.now()), "insert game")

  private def event(
    eventType:  String,
    seat:       Option[Int]    = None,
    sourceSeat: Option[Int]    = None,
    tile:       Option[Tile]   = None,
    chowPos:    Option[String] = None
  ): Unit = {
    val row = GameEventRow(gameId, seq, eventType, seat, sourceSeat, tile.map(Models.tileToWire), chowPos, Instant.now())
    seq += 1
    run(repo.insertEvent(row), s"insert event $eventType")
  }

  /** The engine-facing hook; tee it with the snapshot-publishing logger. */
  val logger: GameLogger = new GameLogger {
    override def start():  Unit = event("start")
    override def resume(): Unit = event("resume")
    override def draw(e: DrawEvent):       Unit = event("draw",    Some(e.playerId), tile = Some(e.tile))
    override def discard(e: DiscardEvent): Unit = event("discard", Some(e.playerId), tile = Some(e.tile))
    override def kong(e: KongEvent):       Unit = event("kong",    Some(e.playerId), Some(e.sourcePlayerId), Some(e.tile))
    override def pong(e: PongEvent):       Unit = event("pong",    Some(e.playerId), Some(e.sourcePlayerId), Some(e.tile))
    override def chow(e: ChowEvent):       Unit =
      event("chow", Some(e.playerId), Some(e.sourcePlayerId), Some(e.tile), Some(e.position.toString))
    override def end(e: EndEvent): Unit = {
      event("end")
      closed = true
      run(repo.finishGame(gameId, GameOutcome.from(e.winnersInfo), Instant.now()), "finish game")
    }
  }

  /** Mark the record aborted if the engine died before emitting `end`. */
  def close(): Unit = if (!closed) {
    closed = true
    run(repo.abortGame(gameId, Instant.now()), "abort game")
  }
}

/** Forwards every engine callback to both loggers (persistence + live snapshots). */
class TeeGameLogger(a: GameLogger, b: GameLogger) extends GameLogger {
  override def start():  Unit = { a.start();  b.start()  }
  override def resume(): Unit = { a.resume(); b.resume() }
  override def discard(e: DiscardEvent): Unit = { a.discard(e); b.discard(e) }
  override def kong(e: KongEvent):       Unit = { a.kong(e);    b.kong(e)    }
  override def pong(e: PongEvent):       Unit = { a.pong(e);    b.pong(e)    }
  override def chow(e: ChowEvent):       Unit = { a.chow(e);    b.chow(e)    }
  override def end(e: EndEvent):         Unit = { a.end(e);     b.end(e)     }
  override def draw(e: DrawEvent):       Unit = { a.draw(e);    b.draw(e)    }
}
