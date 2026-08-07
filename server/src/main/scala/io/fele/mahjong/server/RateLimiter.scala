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
  max:    Int,
  window: FiniteDuration,
  now:    IO[Long],
  state:  Ref[IO, Map[String, Vector[Long]]]
) {

  /** Records the hit and reports whether it is within the caller's budget. */
  def allow(key: String): IO[Boolean] = now.flatMap { t =>
    val cutoff = t - window.toMillis
    state.modify { m =>
      // prune every bucket, not just this caller's: otherwise a wide spread of
      // one-shot callers grows the map forever, which is the leak the limiter
      // is supposed to prevent
      val pruned = m.map { case (k, ts) => k -> ts.filter(_ > cutoff) }.filter(_._2.nonEmpty)
      val mine   = pruned.getOrElse(key, Vector.empty)
      if (mine.size >= max) (pruned, false)
      else (pruned.updated(key, mine :+ t), true)
    }
  }
}

object RateLimiter {
  def create(max: Int, window: FiniteDuration,
             now: IO[Long] = IO.realTime.map(_.toMillis)): IO[RateLimiter] =
    Ref.of[IO, Map[String, Vector[Long]]](Map.empty).map(new RateLimiter(max, window, now, _))
}
