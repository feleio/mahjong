package io.fele.app.mahjong.rl

import io.fele.app.mahjong._
import io.fele.app.mahjong.ChowPosition._
import io.fele.app.mahjong.player.{FirstFelix, Player}
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

import scala.util.{Failure, Success, Try}

/**
  * FFDataServer — Runs FirstFelix vs FirstFelix games and outputs
  * every decision made by seat 0 as RL-format observations + teacher actions.
  *
  * Usage:
  *   java -cp <jar> io.fele.app.mahjong.rl.FFDataServer
  *
  * Protocol (stdout, one JSON per line):
  *   {"type":"decision", "decision":"discard", "state":{...}, "context":{...}, "action": 5}
  *   {"type":"decision", "decision":"pong",    "state":{...}, "context":{...}, "action": true}
  *   ...
  *   {"type":"game_over", "reward": -8.0, "winner_ids":[2], "is_self_win":false}
  *
  * Stdin: reads {"cmd":"collect","n_games":1000,"seed":0} commands.
  */
object FFDataServer extends App {

  implicit val formats: Formats = Serialization.formats(NoTypeHints)
  implicit val config: Config = new Config()

  // ── State serialization (same as RLPlayer) ──────────────────────────────

  private def tileGroupsToMap(groups: List[TileGroup]): Map[String, Any] = {
    val pongTiles = groups.collect { case PongGroup(t) => t.toTileValue }
    val kongTiles = groups.collect { case KongGroup(t) => t.toTileValue }
    val chowTiles = groups.collect {
      case ChowGroup(ts) => ts.map(_.toTileValue).toList.sorted
    }
    Map("pongs" -> pongTiles, "kongs" -> kongTiles, "chows" -> chowTiles)
  }

  private def stateToMap(playerId: Int, curState: CurState): Map[String, Any] = {
    val handCounts = Array.fill[Int](34)(0)
    curState.myInfo.tiles.foreach(t => handCounts(t.toTileValue) += 1)

    val discardCounts = Array.fill[Int](34)(0)
    curState.discards.foreach(d => discardCounts(d.tile.toTileValue) += 1)

    val perPlayerDiscards = Array.fill[Array[Int]](4)(Array.fill[Int](34)(0))
    curState.discards.foreach(d => perPlayerDiscards(d.playerId)(d.tile.toTileValue) += 1)

    Map(
      "hand"                -> handCounts.toList,
      "my_groups"           -> tileGroupsToMap(curState.myInfo.tileGroups),
      "opp_groups"          -> curState.otherInfos.map(info => tileGroupsToMap(info.tileGroups)),
      "discarded"           -> discardCounts.toList,
      "discarded_by_player" -> perPlayerDiscards.map(_.toList).toList,
      "remaining"           -> curState.remainTileNum,
      "my_id"               -> playerId,
      "cur_player_id"       -> curState.curPlayerId
    ) ++ RLFeatures.featureMap(
      handCounts,
      curState.myInfo.tileGroups,
      curState.otherInfos.map(_.tileGroups),
      discardCounts
    )
  }

  // ── Recording player ────────────────────────────────────────────────────

  class RecordingFF(id: Int, tiles: List[Tile], minScore: Int)
      extends FirstFelix(id, tiles, minScore) {

    private def emit(decision: String, curState: CurState,
                     context: Map[String, Any], action: Any): Unit = {
      val msg = Map(
        "type"     -> "decision",
        "decision" -> decision,
        "state"    -> stateToMap(id, curState),
        "context"  -> context,
        "action"   -> action
      )
      println(write(msg))
      System.out.flush()
    }

    override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean = {
      val result = super.decideSelfWin(tile, score, curState)
      emit("self_win", curState,
        Map("tile_id" -> tile.toTileValue, "score" -> score),
        result)
      result
    }

    override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean = {
      val result = super.decideWin(tile, score, curState)
      emit("win", curState,
        Map("tile_id" -> tile.toTileValue, "score" -> score,
            "discard_player_id" -> curState.curPlayerId),
        result)
      result
    }

    override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = {
      val result = super.decideSelfKong(selfKongTiles, curState)
      // Map to action index: None->0, Some(tile)->tile_id+1
      val actionIdx = result.map(_.toTileValue + 1).getOrElse(0)
      emit("self_kong", curState,
        Map("valid_tiles" -> selfKongTiles.map(_.toTileValue).toList.sorted),
        actionIdx)
      result
    }

    override def decideKong(tile: Tile, curState: CurState): Boolean = {
      val result = super.decideKong(tile, curState)
      emit("kong", curState,
        Map("tile_id" -> tile.toTileValue,
            "discard_player_id" -> curState.curPlayerId),
        result)
      result
    }

    override def decidePong(tile: Tile, curState: CurState): Boolean = {
      val result = super.decidePong(tile, curState)
      emit("pong", curState,
        Map("tile_id" -> tile.toTileValue,
            "discard_player_id" -> curState.curPlayerId),
        result)
      result
    }

    override def decideChow(tile: Tile, positions: Set[ChowPosition],
                             curState: CurState): Option[ChowPosition] = {
      val result = super.decideChow(tile, positions, curState)
      // Map to action index: None->0, LEFT(0)->1, MIDDLE(1)->2, RIGHT(2)->3
      val actionIdx = result.map(_.id + 1).getOrElse(0)
      emit("chow", curState,
        Map("tile_id" -> tile.toTileValue,
            "positions" -> positions.map(_.id).toList.sorted,
            "discard_player_id" -> curState.curPlayerId),
        actionIdx)
      result
    }

    override def decideDiscard(curState: CurState): Tile = {
      val result = super.decideDiscard(curState)
      val validTiles = hand.dynamicTiles.map(_.toTileValue).distinct.sorted
      emit("discard", curState,
        Map("valid_tiles" -> validTiles),
        result.toTileValue)
      result
    }

    override def name: String = "RecordingFF"
  }

  // ── Main loop ───────────────────────────────────────────────────────────

  import org.json4s.native.JsonMethods.parse

  while (true) {
    val line = scala.io.StdIn.readLine()
    if (line == null) sys.exit(0)
    val cmd = parse(line).values.asInstanceOf[Map[String, Any]]

    if (cmd.getOrElse("cmd", "") == "collect") {
      val nGames = cmd.get("n_games").map {
        case v: BigInt => v.toInt; case v: Int => v; case v: Long => v.toInt
      }.getOrElse(100)
      val baseSeed = cmd.get("seed").map {
        case v: BigInt => v.toLong; case v: Long => v; case v: Int => v.toLong
      }.getOrElse(0L)

      for (g <- 0 until nGames) {
        val drawer: TileDrawer = new RandomTileDrawer(Some(baseSeed + g))
        val recordingPlayer = new RecordingFF(0, drawer.popHand(), 5)
        val opponents = (1 to 3).map(i => new FirstFelix(i, drawer.popHand(), 5): Player).toList
        val players = recordingPlayer :: opponents

        val initPlayerId = ((baseSeed + g) % 4).toInt
        val state = GameState(players, None, Nil, initPlayerId, drawer)
        implicit val gameLogger: GameLogger = new DummyGameLogger()
        val flow = new FlowImpl(state)

        val result = Try(flow.start()) match {
          case Success(r) => r
          case Failure(ex) =>
            System.err.println(s"Game ${g} crashed: ${ex.getMessage}")
            GameResult(None)
        }

        // Output game_over with reward for seat 0
        val reward: Double = result.winnersInfo match {
          case None => 0.0
          case Some(info) =>
            info.winnersBalance.find(_.id == 0).map(_.amount.toDouble).getOrElse(0.0)
        }
        val winnerIds = result.winnersInfo.map(_.winners.map(_.id).toList).getOrElse(List.empty)
        val isSelfWin = result.winnersInfo.exists(_.isSelfWin)

        val msg = Map(
          "type"        -> "game_over",
          "reward"      -> reward,
          "winner_ids"  -> winnerIds,
          "is_self_win" -> isSelfWin,
          "game_idx"    -> g
        )
        println(write(msg))
        System.out.flush()
      }

      // Signal batch complete
      val doneMsg = Map("type" -> "batch_done", "n_games" -> nGames)
      println(write(doneMsg))
      System.out.flush()
    }
  }
}
