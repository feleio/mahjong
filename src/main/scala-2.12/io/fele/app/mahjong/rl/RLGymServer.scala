package io.fele.app.mahjong.rl

import io.fele.app.mahjong._
import io.fele.app.mahjong.player.{Chicken, FirstFelix, Player}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

import scala.util.{Failure, Success, Try}

/**
  * RLGymServer — Main entry point for the RL training environment.
  *
  * Exposes a simple JSON-over-stdin/stdout protocol so that a Python RL agent
  * can drive the simulator like an OpenAI Gym environment:
  *
  *   Python → Scala  (stdin):   {"cmd": "reset"} | {"cmd": "reset", "seed": 42}
  *   Scala  → Python (stdout):  first observation JSON (see RLPlayer)
  *
  *   …multiple observation / action exchanges during the game…
  *
  *   Scala  → Python (stdout):  {"type": "game_over", "reward": <float>,
  *                                "winner_ids": [...], "loser_id": <int|null>,
  *                                "is_self_win": <bool>,
  *                                "state": {...}}
  *
  * The RL agent is always player 0.  Players 1–3 are Chicken bots.
  *
  * Reward shaping
  * ──────────────
  *   Win  (self-drawn)  : +scoreMap[score] * 1.5   (matches actual payout)
  *   Win  (on discard)  : +scoreMap[score]
  *   Lose (responsible) : sum of –scoreMap[winner_score] for each winner
  *   Draw               : 0
  */
object RLGymServer extends App {

  implicit val formats: Formats = Serialization.formats(NoTypeHints)
  implicit val config: Config = new Config()

  // ── Helpers ────────────────────────────────────────────────────────────────

  private def readCommand(): Map[String, Any] = {
    val line = scala.io.StdIn.readLine()
    if (line == null) sys.exit(0)
    parse(line).values.asInstanceOf[Map[String, Any]]
  }

  private def sendGameOver(result: GameResult,
                            selfPlay: Boolean,
                            rlPlayerId: Int,
                            lastState: Option[Map[String, Any]]): Unit = {
    val winnerIds: List[Int] = result.winnersInfo
      .map(_.winners.map(_.id).toList).getOrElse(List.empty)
    val loserId: Option[Int]  = result.winnersInfo.flatMap(_.loserId)
    val isSelfWin: Boolean    = result.winnersInfo.exists(_.isSelfWin)

    if (selfPlay) {
      // Send per-seat rewards for all 4 players.
      // With -Drl.scorebonus=true, add a quality bonus for high-scoring wins
      // (5pt +20%, 7pt +50%, 8pt +100%); default is the true zero-sum payout.
      val scoreBonus = System.getProperty("rl.scorebonus", "false").toLowerCase == "true"
      val balMap: Map[Int, Double] = result.winnersInfo match {
        case None       => Map.empty
        case Some(info) => info.winnersBalance.map(b => b.id -> b.amount.toDouble).toMap
      }
      val winnerScores: Map[Int, Int] = result.winnersInfo match {
        case None       => Map.empty
        case Some(info) => info.winners.map(w => w.id -> w.score).toMap
      }
      val allRewards = (0 to 3).map { i =>
        val base  = balMap.getOrElse(i, 0.0)
        val bonus = if (scoreBonus && base > 0) winnerScores.get(i).map { s =>
          if (s >= 8)      base * 1.0   // 8pt → 2× payout
          else if (s >= 7) base * 0.5   // 7pt → 1.5×
          else if (s >= 5) base * 0.2   // 5pt → 1.2×
          else             0.0
        }.getOrElse(0.0) else 0.0
        i.toString -> (base + bonus)
      }.toMap
      val msg = Map(
        "type"        -> "game_over",
        "rewards"     -> allRewards,
        "winner_ids"  -> winnerIds,
        "loser_id"    -> loserId.map(_.toString).orNull,
        "is_self_win" -> isSelfWin
      )
      println(write(msg))
      System.out.flush()
    } else {
      val reward: Double = result.winnersInfo match {
        case None => 0.0
        case Some(info) =>
          info.winnersBalance.find(_.id == rlPlayerId).map(_.amount.toDouble).getOrElse(0.0)
      }
      val agentScore: Option[Int] = result.winnersInfo.flatMap(
        _.winners.find(_.id == rlPlayerId).map(_.score)
      )
      val msg = Map(
        "type"        -> "game_over",
        "reward"      -> reward,
        "winner_ids"  -> winnerIds,
        "loser_id"    -> loserId.map(_.toString).orNull,
        "is_self_win" -> isSelfWin,
        "agent_score" -> agentScore.map(_.asInstanceOf[Any]).orNull,
        "state"       -> lastState.orNull
      )
      println(write(msg))
      System.out.flush()
    }
  }

  // ── Main loop ──────────────────────────────────────────────────────────────

  val selfPlay     = System.getProperty("rl.selfplay",  "false").toLowerCase == "true"
  val opponentType = System.getProperty("rl.opponent",  "chicken").toLowerCase
  // Append ground-truth opponent danger labels (DangerLabels) to every
  // observation's state map — datagen for the #21 auxiliary heads. Off by
  // default: 3×34 canWin calls per observation, and labels leak hidden
  // state, so they must never reach a play-time consumer.
  val dangerLabels = System.getProperty("rl.dangerlabels", "false").toLowerCase == "true"
  val rng          = new scala.util.Random()

  var gameCount = 0

  while (true) {
    val cmd = readCommand()
    if (cmd.getOrElse("cmd", "") != "reset") {
      // Silently ignore unexpected commands; wait for reset
    } else {
      gameCount += 1
      val seed: Option[Long] = cmd.get("seed").flatMap {
        case v: BigInt => Some(v.toLong)
        case v: Long   => Some(v)
        case v: Int    => Some(v.toLong)
        case _         => None
      }

      // Build game
      val drawer: TileDrawer = new RandomTileDrawer(seed.orElse(Some(gameCount.toLong)))

      // Probes close over this var so RLPlayers can be constructed before the
      // full table exists; it is assigned before the first decision is made.
      var table: List[Player] = null
      def probe(pid: Int): Option[() => Map[String, Any]] =
        if (dangerLabels) Some(() => DangerLabels.compute(table, pid)) else None

      val players: List[Player] = if (selfPlay) {
        // True self-play: all 4 seats are RL agents sharing the same Python model
        (0 to 3).map(i => new RLPlayer(i, drawer.popHand(), probe(i))).toList
      } else {
        // Single RL agent (seat 0) vs Chicken / FirstFelix opponents
        val useFirstFelix = opponentType == "firstfelix" ||
          (opponentType == "mixed" && rng.nextBoolean())
        val rlPlayer  = new RLPlayer(0, drawer.popHand(), probe(0))
        val opponents = (1 to 3).map(i =>
          if (useFirstFelix) new FirstFelix(i, drawer.popHand(), 5)
          else new Chicken(i, drawer.popHand())
        ).toList
        rlPlayer :: opponents
      }
      table = players

      val initPlayerId = seed.map(s => (s % 4).toInt).getOrElse(gameCount % 4)
      val state = GameState(players, None, Nil, initPlayerId.toInt, drawer)

      implicit val gameLogger: GameLogger = new DummyGameLogger()
      val flow = new FlowImpl(state)

      val result = Try(flow.start()) match {
        case Success(r) => r
        case Failure(ex) =>
          System.err.println(s"Game $gameCount crashed: ${ex.getMessage}")
          GameResult(None)
      }

      sendGameOver(result, selfPlay = selfPlay, rlPlayerId = 0, lastState = None)
    }
  }
}
