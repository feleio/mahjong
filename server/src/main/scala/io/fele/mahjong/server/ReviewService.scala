package io.fele.mahjong.server

import cats.effect.IO
import io.circe.Encoder
import io.circe.generic.semiauto._
import io.fele.app.mahjong.{Config => EngineConfig, CurState}
import io.fele.app.mahjong.rl.PolicyOut
import io.fele.mahjong.server.Models._

import java.time.Instant
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable.ListBuffer

/** Post-game review: replay a recorded game and diff every decision of one
  * seat against the champion (issues #40/#41).
  *
  * Reviews are pure functions of an immutable finished record, so results are
  * cached in memory for the life of the process. Replay + ~100 champion
  * queries takes a few hundred ms — always run inside IO.blocking.
  */
object ReviewService {

  case class ReviewDecision(
    seq:         Int,               // recorded-event cursor at decision time (stable ordering key)
    kind:        String,            // discard | win | self_win | kong | self_kong | pong | chow
    contextTile: Option[String],    // offered/claimable tile for the non-discard kinds
    hand:        List[String],      // your concealed tiles at the decision, sorted
    chosen:      String,            // what you played (action key)
    chosenProb:  Double,            // champion's probability of your action
    best:        String,            // champion's preferred action
    bestProb:    Double,
    gap:         Double,            // bestProb - chosenProb (0 when you agreed)
    value:       Double,            // champion value estimate at the decision
    agree:       Boolean,
    timedOut:    Boolean,           // engine played a default; not a choice you made (#42)
    why:         Option[String],    // grounded reason the champion differed (#44)
    bucket:      Option[String]     // shape | tempo | safety | accept | other
  )
  object ReviewDecision { implicit val enc: Encoder[ReviewDecision] = deriveEncoder }

  /** Counts cover the decisions the player actually made; `timedOut` turns are
    * reported separately and excluded, because scoring moves nobody chose
    * would make the agreement rate meaningless (issue #42). */
  case class ReviewSummary(decisions: Int, agreements: Int, agreementRate: Double, meanGap: Double, timedOut: Int)
  object ReviewSummary { implicit val enc: Encoder[ReviewSummary] = deriveEncoder }

  case class GameReview(
    gameId:     String,
    seat:       Int,
    playerName: String,
    startedAt:  Instant,
    outcome:    Option[GameOutcome],
    seatMoney:  Int,
    summary:    ReviewSummary,
    decisions:  List[ReviewDecision]   // disagreements first, biggest gap first
  )
  object GameReview { implicit val enc: Encoder[GameReview] = deriveEncoder }

  sealed trait ReviewError { def message: String }
  case object GameNotFound        extends ReviewError { val message = "game not found" }
  case object GameNotFinished     extends ReviewError { val message = "game is not finished" }
  case class  BadSeat(msg: String) extends ReviewError { val message = msg }
  case class  ChampionDown(msg: String) extends ReviewError { val message = msg }
  case class  ReplayFailed(msg: String) extends ReviewError { val message = msg }

  case class PlayerGameAgreement(gameId: String, startedAt: Instant, agreementRate: Double,
                                 decisions: Int, timedOut: Int)
  object PlayerGameAgreement { implicit val enc: Encoder[PlayerGameAgreement] = deriveEncoder }

  case class PlayerStats(
    name:            String,
    games:           Int,
    totalMoney:      Int,
    moneyPerGame:    Double,
    winRate:         Double,
    selfWinRate:     Double,
    dealInRate:      Double,
    drawRate:        Double,
    reviewedGames:   Int,
    agreementRate:   Option[Double],          // pooled over reviewed games, timed-out turns excluded (#42)
    timedOutTurns:   Int,                     // turns the engine played for you across reviewed games
    agreementByGame: List[PlayerGameAgreement] // newest first — the "over time" series
  )
  object PlayerStats { implicit val enc: Encoder[PlayerStats] = deriveEncoder }

  /** Engine payout for one seat, mirroring WinnersInfo.winnersBalance (and
    * rl/human_eval.py seat_money). */
  def seatMoney(outcome: GameOutcome, seat: Int)(implicit cfg: EngineConfig): Int = {
    def amount(score: Int): Int = cfg.scoreMap(math.min(math.max(score, cfg.minScore), cfg.maxScore).toString)
    if (outcome.drawn || outcome.winners.isEmpty) 0
    else if (outcome.isSelfWin) {
      val w = outcome.winners.head
      if (w.seat == seat) amount(w.score) * 3 / 2 else -amount(w.score) / 2
    } else outcome.winners.find(_.seat == seat) match {
      case Some(w)                                   => amount(w.score)
      case None if outcome.loserSeat.contains(seat)  => -outcome.winners.map(w => amount(w.score)).sum
      case None                                      => 0
    }
  }
}

class ReviewService(repo: GameRecordRepo)(implicit engineConfig: EngineConfig) {
  import ReviewService._

  private val cache = new ConcurrentHashMap[(String, Int), GameReview]()

  def review(gameId: String, seat: Int): IO[Either[ReviewError, GameReview]] = {
    val key = (gameId, seat)
    Option(cache.get(key)) match {
      case Some(r) => IO.pure(Right(r))
      case None =>
        (for {
          _   <- check(seat >= 0 && seat <= 3, BadSeat(s"seat must be 0..3, got $seat"))
          _   <- checkChampion
          g   <- loadFinished(gameId).flatMap(e => IO.fromEither(e.left.map(Bail)))
          evs <- repo.eventsFor(gameId)
          tos <- repo.timeoutsFor(gameId)
          rev <- runReplay(g, evs, tos, seat).flatMap(e => IO.fromEither(e.left.map(Bail)))
          _   <- IO(cache.put(key, rev)).void
        } yield rev).attempt.map {
          case Right(r)        => Right(r)
          case Left(Bail(err)) => Left(err)
          case Left(t)         => Left(ReplayFailed(s"${t.getClass.getSimpleName}: ${t.getMessage}"))
        }
    }
  }

  private case class Bail(err: ReviewError) extends RuntimeException(err.message)
  private def check(cond: Boolean, err: => ReviewError): IO[Unit] =
    if (cond) IO.unit else IO.raiseError(Bail(err))
  private def checkChampion: IO[Unit] =
    ChampionService.unavailableReason.fold(IO.unit)(e => IO.raiseError(Bail(ChampionDown(e))))

  private def loadFinished(gameId: String): IO[Either[ReviewError, GameRecordRow]] =
    repo.getGame(gameId).map {
      case None                                                  => Left(GameNotFound)
      case Some(g) if g.status != GameRecordStatus.Finished      => Left(GameNotFinished)
      case Some(g)                                               => Right(g)
    }

  private def runReplay(g: GameRecordRow, events: List[GameEventRow],
                        timeouts: List[DecisionTimeoutRow], seat: Int): IO[Either[ReviewError, GameReview]] =
    IO.blocking {
      val outcome = g.outcome.getOrElse(GameOutcome(drawn = true, isSelfWin = false, None, None, Nil))
      // (event cursor, decision kind) of every prompt this seat let expire
      val timedOutAt: Set[(Int, String)] =
        timeouts.filter(_.seat == seat).map(t => (t.eventSeq, t.kind)).toSet
      val buf = ListBuffer.empty[ReviewDecision]
      val observer = new GameReplayer.DecisionObserver {
        override def onDecision(eventPos: Int, kind: String, cs: CurState, contextTile: Option[Int],
                                head: PolicyOut => Array[Float], keys: List[(String, Int)], chosen: String): Unit = {
          CoachService.hint(seat, cs, contextTile, head, keys).foreach { h =>
            val chosenProb = h.probs.getOrElse(chosen, 0.0)
            val why = scala.util.Try(DecisionExplainer.explain(kind, cs, chosen, h.top, h)).toOption.flatten
            buf += ReviewDecision(
              seq         = eventPos,
              kind        = kind,
              contextTile = contextTile.map(CoachService.tileWire),
              hand        = cs.myInfo.tiles.map(_.toTileValue).sorted.map(CoachService.tileWire),
              chosen      = chosen,
              chosenProb  = chosenProb,
              best        = h.top,
              bestProb    = h.probs.getOrElse(h.top, 0.0),
              gap         = math.max(0.0, h.probs.getOrElse(h.top, 0.0) - chosenProb),
              value       = h.value,
              agree       = chosen == h.top,
              timedOut    = timedOutAt.contains((eventPos, kind)),
              why         = why.map(_.text),
              bucket      = why.map(_.bucket)
            )
          }
        }
      }
      try {
        GameReplayer.replay(g.wall, events, outcome, Some(seat), observer)
        val ds = buf.toList
        val played     = ds.filterNot(_.timedOut)
        val agreements = played.count(_.agree)
        val summary = ReviewSummary(
          decisions     = played.size,
          agreements    = agreements,
          agreementRate = if (played.isEmpty) 1.0 else agreements.toDouble / played.size,
          meanGap       = if (played.isEmpty) 0.0 else played.map(_.gap).sum / played.size,
          timedOut      = ds.size - played.size
        )
        // timed-out turns sink to the bottom: they are context, not study material
        val sorted = ds.sortBy(d => (d.timedOut, d.agree, -d.gap, d.seq))
        Right(GameReview(g.id, seat, g.seats.lift(seat).map(_.name).getOrElse(s"seat $seat"),
          g.startedAt, g.outcome, seatMoney(outcome, seat), summary, sorted))
      } catch {
        case m: GameReplayer.ReplayMismatchException => Left(ReplayFailed(m.getMessage))
      }
    }

  /** Finished games with a human seat named `player` (newest first). */
  def gamesFor(player: Option[String], limit: Int): IO[List[(GameRecordRow, Option[Int])]] =
    repo.listGames(None, math.min(math.max(limit, 1), 500)).map { rows =>
      rows.filter(_.status == GameRecordStatus.Finished).flatMap { g =>
        player match {
          case None => List((g, None))
          case Some(name) =>
            g.seats.find(s => s.kind == SeatKind.Human && s.name == name)
              .map(s => (g, Some(s.index))).toList
        }
      }
    }

  def playerStats(name: String, maxReviewGames: Int = 20): IO[PlayerStats] =
    gamesFor(Some(name), 500).flatMap { games =>
      val outcomes = games.collect { case (g, Some(seat)) => (g, seat, g.outcome) }
      val n = outcomes.size
      val money   = outcomes.map { case (_, seat, o) => o.fold(0)(seatMoney(_, seat)) }
      val wins    = outcomes.count { case (_, seat, o) => o.exists(_.winners.exists(_.seat == seat)) }
      val selfW   = outcomes.count { case (_, seat, o) => o.exists(x => x.isSelfWin && x.winners.exists(_.seat == seat)) }
      val dealIns = outcomes.count { case (_, seat, o) => o.exists(_.loserSeat.contains(seat)) }
      val draws   = outcomes.count { case (_, _, o) => o.forall(_.drawn) }

      // agreement over the most recent games only — each review replays the
      // game with ~100 champion queries, so bound the work per request
      import cats.syntax.traverse._
      outcomes.take(maxReviewGames).traverse { case (g, seat, _) =>
        review(g.id, seat).map(_.toOption.map(r =>
          PlayerGameAgreement(g.id, g.startedAt, r.summary.agreementRate, r.summary.decisions, r.summary.timedOut)))
      }.map(_.flatten).map { agr =>
        val totalDecisions = agr.map(_.decisions).sum
        val pooled =
          if (totalDecisions == 0) None
          else Some(agr.map(a => a.agreementRate * a.decisions).sum / totalDecisions)
        PlayerStats(
          name            = name,
          games           = n,
          totalMoney      = money.sum,
          moneyPerGame    = if (n == 0) 0.0 else money.sum.toDouble / n,
          winRate         = rate(wins, n),
          selfWinRate     = rate(selfW, n),
          dealInRate      = rate(dealIns, n),
          drawRate        = rate(draws, n),
          reviewedGames   = agr.size,
          agreementRate   = pooled,
          timedOutTurns   = agr.map(_.timedOut).sum,
          agreementByGame = agr
        )
      }
    }

  private def rate(k: Int, n: Int): Double = if (n == 0) 0.0 else k.toDouble / n
}
