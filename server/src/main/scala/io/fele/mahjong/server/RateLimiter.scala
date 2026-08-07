package io.fele.mahjong.server

import cats.effect.{IO, Ref}

import scala.concurrent.duration.FiniteDuration

/** A sliding-window limiter, keyed by caller (issue #53).
  *
  * Room creation is the one unauthenticated write this server exposes, and
  * every room it makes is held in memory, so without a limit one script can
  * occupy the whole room cap in a second.
  *
  * State is in-process and resets on restart. That is the right scope here —
  * the rooms it protects are in-process too — but it does mean a proxy in
  * front of the server makes every caller share one bucket unless the deploy
  * passes the real client address through. */
final class RateLimiter private (
  max:     Int,
  window:  FiniteDuration,
  maxKeys: Int,
  now:     IO[Long],
  state:   Ref[IO, Map[String, Vector[Long]]]
) {

  /** Records the hit and reports whether it is within the caller's budget.
    *
    * The common path touches only the caller's own bucket. Sweeping the whole
    * map on every call would be O(callers) per request — and `Ref.modify`
    * re-runs the function on CAS contention, so a burst would pay it
    * repeatedly. The sweep happens only when the map is at its key ceiling,
    * which also bounds memory: without a ceiling, one host cycling through an
    * IPv6 /64 would make the limiter itself the memory-exhaustion vector it
    * was added to prevent. */
  def allow(key: String): IO[Boolean] = now.flatMap { t =>
    val cutoff = t - window.toMillis
    state.modify { m =>
      val mine = m.getOrElse(key, Vector.empty).filter(_ > cutoff)
      if (mine.size >= max) (m.updated(key, mine), false)
      else if (mine.isEmpty && !m.contains(key) && m.size >= maxKeys) {
        val swept = m.map { case (k, ts) => k -> ts.filter(_ > cutoff) }.filter(_._2.nonEmpty)
        // still full of live callers: refuse the new one rather than grow.
        // Callers already tracked keep their budget, so an address flood
        // degrades new arrivals instead of everybody.
        if (swept.size >= maxKeys) (swept, false)
        else (swept.updated(key, Vector(t)), true)
      }
      else (m.updated(key, mine :+ t), true)
    }
  }
}

object RateLimiter {
  /** Distinct callers tracked at once before the limiter starts refusing new
    * ones. Far above any real audience for this server, far below anything
    * that troubles the heap. */
  val DefaultMaxKeys = 8192

  def create(max: Int, window: FiniteDuration,
             // monotonic, not wall-clock: an NTP step backwards would park
             // future-dated timestamps in the window and lock callers out for
             // the size of the jump
             now: IO[Long] = IO.monotonic.map(_.toMillis),
             maxKeys: Int = DefaultMaxKeys): IO[RateLimiter] =
    Ref.of[IO, Map[String, Vector[Long]]](Map.empty).map(new RateLimiter(max, window, maxKeys, now, _))
}
