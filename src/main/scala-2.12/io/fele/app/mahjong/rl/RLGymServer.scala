package io.fele.app.mahjong.rl

import io.fele.app.mahjong._
import io.fele.app.mahjong.player.Chicken
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
                            rlPlayerId: Int,
                            lastState: Option[Map[String, Any]]): Unit = {
    val reward: Double = result.winnersInfo match {
      case None => 0.0
      case Some(info) =>
        val balance = info.winnersBalance
        balance.find(_.id == rlPlayerId).map(_.amount.toDouble).getOrElse(0.0)
    }

    val winnerIds: List[Int] = result.winnersInfo
      .map(_.winners.map(_.id).toList).getOrElse(List.empty)
    val loserId: Option[Int] = result.winnersInfo.flatMap(_.loserId)
    val isSelfWin: Boolean   = result.winnersInfo.exists(_.isSelfWin)

    val msg = Map(
      "type"        -> "game_over",
      "reward"      -> reward,
      "winner_ids"  -> winnerIds,
      "loser_id"    -> loserId.map(_.toString).orNull,
      "is_self_win" -> isSelfWin,
      "state"       -> lastState.orNull
    )
    println(write(msg))
    System.out.flush()
  }

  // ── Main loop ──────────────────────────────────────────────────────────────

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
      val rlPlayer  = new RLPlayer(0, drawer.popHand())
      val opponents = (1 to 3).map(i => new Chicken(i, drawer.popHand())).toList
      val players   = rlPlayer :: opponents

      val initPlayerId = seed.map(s => (s % 4).toInt).getOrElse(gameCount % 4)
      val state = GameState(players, None, Nil, initPlayerId.toInt, drawer)

      implicit val gameLogger: GameLogger = new DummyGameLogger()
      val flow = new FlowImpl(state)

      val result = Try(flow.start()) match {
        case Success(r) => r
        case Failure(ex) =>
          // Surface unexpected errors; Python can handle a game_over with null state
          System.err.println(s"Game $gameCount crashed: ${ex.getMessage}")
          GameResult(None)
      }

      sendGameOver(result, rlPlayerId = 0, lastState = None)
    }
  }
}
