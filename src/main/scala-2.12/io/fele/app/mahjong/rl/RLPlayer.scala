package io.fele.app.mahjong.rl

import io.fele.app.mahjong._
import io.fele.app.mahjong.ChowPosition._
import io.fele.app.mahjong.player.Player
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

/**
  * DangerLabels: ground-truth per-opponent danger labels for the auxiliary
  * tenpai/wait heads (issue #21). Computed from the TRUE hidden hands the
  * simulator holds — labels only, never observation input.
  *
  *   opp_waits [3][34] — 1 if that opponent wins on that tile right now
  *                       (Hand.canWin, so config.minScore is respected)
  *   opp_tenpai [3]    — 1 if that opponent has any winning tile
  *
  * Opponents are in seat-relative order (myId+1 .. myId+3) — the same order
  * as CurState.otherInfos and the obs opp_groups planes.
  */
object DangerLabels {
  def compute(players: List[Player], myId: Int): Map[String, Any] = {
    val waits = (1 to 3).map { off =>
      val p = players((myId + off) % 4)
      (0 until 34).map(v => if (p.canWin(Tile.fromValue(v)).canWin) 1 else 0).toList
    }.toList
    Map(
      "opp_waits"  -> waits,
      "opp_tenpai" -> waits.map(w => if (w.sum > 0) 1 else 0)
    )
  }
}

/**
  * RLPlayer: A mahjong player that delegates all decisions to an external
  * reinforcement learning agent via JSON messages over stdin/stdout.
  *
  * Protocol:
  *   Scala → Python: JSON observation line written to stdout
  *   Python → Scala: JSON action line read from stdin
  *
  * @param dangerProbe when set (RLGymServer -Drl.dangerlabels=true), called at
  *                    every observation to append ground-truth opponent danger
  *                    labels (DangerLabels) to the state map.
  */
class RLPlayer(id: Int, tiles: List[Tile],
               dangerProbe: Option[() => Map[String, Any]] = None)(implicit config: Config)
    extends Player(id, tiles) {

  implicit val formats: Formats = Serialization.formats(NoTypeHints)

  // Reward sent on the NEXT observation after the agent discards to tenpai.
  // 2.0 = 25% of the minimum win payout ($8), a modest shaping signal.
  private val TENPAI_REWARD = 2.0
  private var pendingTenpaiReward: Double = 0.0

  /** True if the current 13-tile hand is tenpai (any tile would win). */
  private def checkTenpai(): Boolean =
    (0 until 34).exists(v => hand.canWin(Tile.fromValue(v)).canWin)

  // ---------------------------------------------------------------------------
  // State serialization helpers
  // ---------------------------------------------------------------------------

  private def tileGroupsToMap(groups: List[TileGroup]): Map[String, Any] = {
    val pongTiles  = groups.collect { case PongGroup(t) => t.toTileValue }
    val kongTiles  = groups.collect { case KongGroup(t) => t.toTileValue }
    val chowTiles  = groups.collect {
      case ChowGroup(ts) => ts.map(_.toTileValue).toList.sorted
    }
    Map("pongs" -> pongTiles, "kongs" -> kongTiles, "chows" -> chowTiles)
  }

  /** Encode CurState into a plain Map that json4s can serialise. */
  private def stateToMap(curState: CurState): Map[String, Any] = {
    // Count of each tile type in hand (index 0-33)
    val handCounts = Array.fill[Int](34)(0)
    curState.myInfo.tiles.foreach(t => handCounts(t.toTileValue) += 1)

    // Count of each tile type in discard pile (total, for backward compat)
    val discardCounts = Array.fill[Int](34)(0)
    curState.discards.foreach(d => discardCounts(d.tile.toTileValue) += 1)

    // Per-player discard counts (4 players × 34 tile types)
    val perPlayerDiscards = Array.fill[Array[Int]](4)(Array.fill[Int](34)(0))
    curState.discards.foreach(d => perPlayerDiscards(d.playerId)(d.tile.toTileValue) += 1)

    Map(
      "hand"                -> handCounts.toList,
      "my_groups"           -> tileGroupsToMap(curState.myInfo.tileGroups),
      "opp_groups"          -> curState.otherInfos.map(info => tileGroupsToMap(info.tileGroups)),
      "discarded"           -> discardCounts.toList,
      "discarded_by_player" -> perPlayerDiscards.map(_.toList).toList,
      // chronological [playerId, tileValue] pairs, oldest first (curState.discards
      // is most-recent-first) — the discard-ORDER signal for obs v4 (issue #21)
      "discard_seq"         -> curState.discards.reverse.map(d => List(d.playerId, d.tile.toTileValue)),
      "remaining"           -> curState.remainTileNum,
      "my_id"               -> id,
      "cur_player_id"       -> curState.curPlayerId
    ) ++ dangerProbe.map(_()).getOrElse(Map.empty) ++ RLFeatures.featureMap(
      handCounts,
      curState.myInfo.tileGroups,
      curState.otherInfos.map(_.tileGroups),
      discardCounts
    )
  }

  // ---------------------------------------------------------------------------
  // Communication primitives
  // ---------------------------------------------------------------------------

  /** Write an observation message to stdout (Python reads it).
   *  Includes any pending tenpai reward and resets it. */
  private def sendObservation(decision: String,
                               curState: CurState,
                               context: Map[String, Any]): Unit = {
    val msg = Map(
      "type"          -> "observation",
      "seat_id"       -> id,
      "decision"      -> decision,
      "state"         -> stateToMap(curState),
      "context"       -> context,
      "tenpai_reward" -> pendingTenpaiReward
    )
    pendingTenpaiReward = 0.0
    println(write(msg))
    System.out.flush()
  }

  /** Read one JSON line from stdin (Python writes it). */
  private def readAction(): Map[String, Any] = {
    val line = scala.io.StdIn.readLine()
    if (line == null) throw new RuntimeException("RL agent closed stdin unexpectedly")
    parse(line).values.asInstanceOf[Map[String, Any]]
  }

  /** Convenience: read a boolean action field. */
  private def readBoolAction(): Boolean = readAction()("action").asInstanceOf[Boolean]

  /** Convenience: read an optional int action field (null → None). */
  private def readOptIntAction(): Option[Int] = readAction()("action") match {
    case null       => None
    case v: BigInt  => Some(v.toInt)
    case v: Int     => Some(v)
    case v: Long    => Some(v.toInt)
  }

  /** Convenience: read a mandatory int action field. */
  private def readIntAction(): Int = readAction()("action") match {
    case v: BigInt => v.toInt
    case v: Int    => v
    case v: Long   => v.toInt
  }

  // ---------------------------------------------------------------------------
  // Decision methods (called by the game engine)
  // ---------------------------------------------------------------------------

  override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean = {
    sendObservation("self_win", curState,
      Map("tile_id" -> tile.toTileValue, "score" -> score))
    readBoolAction()
  }

  override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean = {
    sendObservation("win", curState,
      Map("tile_id" -> tile.toTileValue, "score" -> score,
          "discard_player_id" -> curState.curPlayerId))
    readBoolAction()
  }

  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = {
    sendObservation("self_kong", curState,
      Map("valid_tiles" -> selfKongTiles.map(_.toTileValue).toList.sorted))
    readOptIntAction().map(Tile.fromValue)
  }

  override def decideKong(tile: Tile, curState: CurState): Boolean = {
    sendObservation("kong", curState,
      Map("tile_id" -> tile.toTileValue,
          "discard_player_id" -> curState.curPlayerId))
    readBoolAction()
  }

  override def decidePong(tile: Tile, curState: CurState): Boolean = {
    sendObservation("pong", curState,
      Map("tile_id" -> tile.toTileValue,
          "discard_player_id" -> curState.curPlayerId))
    readBoolAction()
  }

  override def decideChow(tile: Tile,
                           positions: Set[ChowPosition],
                           curState: CurState): Option[ChowPosition] = {
    sendObservation("chow", curState,
      Map("tile_id"           -> tile.toTileValue,
          "positions"         -> positions.map(_.id).toList.sorted,
          "discard_player_id" -> curState.curPlayerId))
    readOptIntAction().map(ChowPosition(_))
  }

  override def decideDiscard(curState: CurState): Tile = {
    val validTiles = hand.dynamicTiles.map(_.toTileValue).distinct.sorted
    sendObservation("discard", curState,
      Map("valid_tiles" -> validTiles))
    val tile = Tile.fromValue(readIntAction())
    // Simulate post-discard hand to check tenpai, then restore.
    // Engine does the actual discard after we return.
    hand.discard(tile)
    if (checkTenpai()) pendingTenpaiReward = TENPAI_REWARD
    hand.add(tile)
    tile
  }

  override def name: String = "RLAgent"
}
