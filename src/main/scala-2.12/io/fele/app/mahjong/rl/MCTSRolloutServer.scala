package io.fele.app.mahjong.rl

import io.fele.app.mahjong._
import io.fele.app.mahjong.player.{Chicken, FirstFelix, Player}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

import scala.util.{Random, Try}

/**
 * MCTSRolloutServer — Monte Carlo rollout evaluator for imperfect-information MCTS.
 *
 * Receives game-state snapshots from Python, determinizes the hidden information
 * (opponent hands + draw pile), and runs complete game rollouts to estimate the
 * value of each candidate discard tile.
 *
 * Protocol (newline-delimited JSON over stdin/stdout):
 *
 *   Python → Scala:
 *     { "cmd":          "evaluate",
 *       "hand":         [34 ints],        // our hand counts (14 tiles at discard time)
 *       "my_groups":    { "pongs": [tileId, ...],
 *                         "kongs": [tileId, ...],
 *                         "chows": [[tileId, tileId, tileId], ...] },
 *       "opp_groups":   [ <same struct>, <same struct>, <same struct> ],
 *       "discards":     [34 ints],        // formal discard pile counts
 *       "remaining":    N,               // draw pile size at decision time
 *       "discard_tile": T,               // candidate tile to evaluate (0-33)
 *       "n_rollouts":   K,               // number of Monte Carlo rollouts
 *       "rollout_opp":  "chicken"|"firstfelix"  // optional, default chicken
 *     }
 *
 *   Scala → Python:
 *     { "mean_reward": float, "std_reward": float, "rewards": [float, ...] }
 */
object MCTSRolloutServer extends App {

  implicit val formats: Formats   = Serialization.formats(NoTypeHints)
  implicit val config:  Config    = new Config()
  implicit val gameLogger: GameLogger = new DummyGameLogger()

  private val rng = new Random()

  // NN rollout policy (loaded lazily from -Drl.nnmodel=<path.onnx>)
  private lazy val nnService: OnnxPolicyService = {
    val path = System.getProperty("rl.nnmodel")
    require(path != null, "self_policy/rollout_opp 'nn' needs -Drl.nnmodel=<path.onnx>")
    new OnnxPolicyService(path)
  }

  // Parallel world evaluation (-Drl.rolloutThreads=N)
  private lazy val rolloutPool = java.util.concurrent.Executors.newFixedThreadPool(
    Integer.getInteger("rl.rolloutThreads", 1))

  // ── Custom TileDrawer that operates over a pre-set tile sequence ─────────────

  private class FixedDrawer(tiles: Seq[Tile]) extends TileDrawer {
    private var pos = 0
    override def pop(): Option[Tile] =
      if (pos < tiles.size) { val t = tiles(pos); pos += 1; Some(t) } else None
    override def popHand(): List[Tile] =
      throw new UnsupportedOperationException("FixedDrawer.popHand not used")
    override def remainingTiles: Seq[Tile] = tiles.slice(pos, tiles.size)
    override def drawerState: DrawerState  = DrawerState(tiles, pos)
  }

  // ── JSON helpers ─────────────────────────────────────────────────────────────

  private def readCommand(): Map[String, Any] = {
    val line = scala.io.StdIn.readLine()
    if (line == null) sys.exit(0)
    parse(line).values.asInstanceOf[Map[String, Any]]
  }

  private def asInt(v: Any): Int = v match {
    case n: BigInt => n.toInt
    case n: Int    => n
    case n: Long   => n.toInt
    case n: Double => n.toInt
  }

  private def asList(v: Any): List[Any] = v.asInstanceOf[List[Any]]

  // ── TileGroup reconstruction ─────────────────────────────────────────────────

  private def parseGroups(g: Map[String, Any]): List[TileGroup] = {
    val pongs = asList(g.getOrElse("pongs", List.empty[Any]))
      .map(x => PongGroup(Tile.fromValue(asInt(x))): TileGroup)
    val kongs = asList(g.getOrElse("kongs", List.empty[Any]))
      .map(x => KongGroup(Tile.fromValue(asInt(x))): TileGroup)
    val chows = asList(g.getOrElse("chows", List.empty[Any])).map { row =>
      val ids = asList(row).map(asInt)
      ChowGroup(ids.map(Tile.fromValue).toSet): TileGroup
    }
    pongs ++ kongs ++ chows
  }

  /** Physical tiles locked inside a set of fixed groups (actual tile count). */
  private def groupPhysicalTiles(groups: List[TileGroup]): List[Tile] = groups.flatMap {
    case PongGroup(t)  => List.fill(3)(t)
    case KongGroup(t)  => List.fill(4)(t)
    case ChowGroup(ts) => ts.toList
  }

  // ── Single rollout ───────────────────────────────────────────────────────────

  private def makePlayer(policy: String, seat: Int, dyn: List[Tile],
                         grps: List[TileGroup]): Player = policy match {
    case "nn"         => new NNPlayer(seat, dyn, grps, nnService)
    case "firstfelix" => new FirstFelix(seat, dyn, 5, grps)
    case _            => new Chicken(seat, dyn, grps)
  }

  private def runRollout(
    myDynamic:   List[Tile],
    myGroups:    List[TileGroup],
    oppGroups:   List[List[TileGroup]],  // 3 opponents (relative seats 1-3)
    discardTile: Tile,
    unknown:     Seq[Tile],              // shuffled pool to split
    remaining:   Int,                   // tiles to assign to draw pile
    oppPolicy:   String,                // "chicken" | "firstfelix" | "nn"
    selfPolicy:  String,                // "chicken" | "firstfelix" | "nn"
    preDiscards: List[DiscardInfo] = Nil // real discard history (NN policies read it)
  ): Double = {

    // --- Distribute unknown tiles ---
    // First `remaining` tiles → draw pile; rest → opponents' dynamic hands
    val drawPile  = unknown.take(remaining)
    var oppHidden = unknown.drop(remaining).toList

    val oppDynamic: List[List[Tile]] = oppGroups.map { grps =>
      val nDynamic = Math.max(0, 13 - 3 * grps.size)
      val chunk    = oppHidden.take(nDynamic)
      oppHidden    = oppHidden.drop(nDynamic)
      chunk
    }

    // Verify hand sizes are valid; skip rollout if not
    val handsOk = (0 until 3).forall { i =>
      val nDynamic = oppDynamic(i).size
      val nGroups  = oppGroups(i).size
      nDynamic + nGroups * 3 == 13
    } && myDynamic.size + myGroups.size * 3 == 13

    if (!handsOk) return 0.0

    // --- Build players ---
    val us: Player = makePlayer(selfPolicy, 0, myDynamic, myGroups)
    val opps: List[Player] = (0 until 3)
      .map(i => makePlayer(oppPolicy, i + 1, oppDynamic(i), oppGroups(i))).toList
    val players: List[Player] = us :: opps

    val drawer = new FixedDrawer(drawPile)
    val state  = GameState(players, None, preDiscards, 0, drawer)
    val flow   = new FlowImpl(state)

    Try(flow.resume(Some(discardTile))).toOption match {
      case None         => 0.0
      case Some(result) =>
        result.winnersInfo match {
          case None       => 0.0
          case Some(info) =>
            info.winnersBalance.find(_.id == 0).map(_.amount.toDouble).getOrElse(0.0)
        }
    }
  }

  // ── Shared state parsing ─────────────────────────────────────────────────────

  private case class ParsedState(
    handCounts: List[Int],           // full 14-tile hand counts (before discard)
    myGroups:   List[TileGroup],
    oppGroups:  List[List[TileGroup]],
    unknownBase: Seq[Tile],
    actualRemaining: Int
  ) {
    /** Dynamic hand after removing one copy of `discardTileId`. */
    def dynamicAfterDiscard(discardTileId: Int): List[Tile] = {
      val rawHand = handCounts.zipWithIndex.flatMap { case (cnt, id) =>
        List.fill(cnt)(Tile.fromValue(id))
      }
      var discardRemoved = false
      rawHand.filter { t =>
        if (!discardRemoved && t.toTileValue == discardTileId) { discardRemoved = true; false }
        else true
      }
    }
  }

  private def parseState(cmd: Map[String, Any]): ParsedState = {
    val handCounts    = asList(cmd("hand")).map(asInt)
    val myGroupsRaw   = cmd("my_groups").asInstanceOf[Map[String, Any]]
    val oppGroupsRaw  = asList(cmd("opp_groups")).map(_.asInstanceOf[Map[String, Any]])
    val discardCounts = asList(cmd("discards")).map(asInt)

    val myGroups  = parseGroups(myGroupsRaw)
    val oppGroups = oppGroupsRaw.map(parseGroups)

    // Compute known physical tiles (everything we can observe).
    // The full 14-tile hand is known; the candidate discard stays in our hand
    // for the purpose of tile accounting (we discard it, it becomes visible).
    val knownMultiset = Array.fill[Int](34)(0)
    handCounts.zipWithIndex.foreach { case (cnt, id) => knownMultiset(id) += cnt }
    groupPhysicalTiles(myGroups).foreach(t  => knownMultiset(t.toTileValue) += 1)
    oppGroups.flatMap(groupPhysicalTiles).foreach(t => knownMultiset(t.toTileValue) += 1)
    discardCounts.zipWithIndex.foreach { case (cnt, id) => knownMultiset(id) += cnt }

    // Unknown tiles = all 136 minus known
    val unknownMultiset = Array.fill[Int](34)(4)
    (0 until 34).foreach(id => unknownMultiset(id) -= knownMultiset(id))
    (0 until 34).foreach(id => if (unknownMultiset(id) < 0) unknownMultiset(id) = 0)

    val unknownBase: Seq[Tile] = unknownMultiset.zipWithIndex.flatMap { case (cnt, id) =>
      List.fill(cnt)(Tile.fromValue(id))
    }.toSeq

    val totalOppNeeded  = oppGroups.map(grps => Math.max(0, 13 - 3 * grps.size)).sum
    val actualRemaining = unknownBase.size - totalOppNeeded

    ParsedState(handCounts, myGroups, oppGroups, unknownBase, actualRemaining)
  }

  /** Optional real discard history, 4×34 counts in rollout seat order
    * (0 = our seat, 1-3 = opponents in opp_groups order). */
  private def parsePreDiscards(cmd: Map[String, Any]): List[DiscardInfo] =
    cmd.get("discards_by_player") match {
      case None => Nil
      case Some(raw) =>
        asList(raw).map(row => asList(row).map(asInt)).zipWithIndex.flatMap {
          case (counts, seat) =>
            counts.zipWithIndex.flatMap { case (cnt, tileId) =>
              List.fill(cnt)(DiscardInfo(seat, Tile.fromValue(tileId)))
            }
        }
    }

  // ── Main server loop ─────────────────────────────────────────────────────────

  while (true) {
    val cmd = readCommand()

    cmd.getOrElse("cmd", "") match {

      // ── Legacy: one candidate tile, independent determinizations ────────────
      case "evaluate" =>
        val st            = parseState(cmd)
        val discardTileId = asInt(cmd("discard_tile"))
        val nRollouts     = asInt(cmd("n_rollouts"))
        val oppPolicy     = cmd.getOrElse("rollout_opp", "chicken").toString
        val selfPolicy    = cmd.getOrElse("self_policy", "chicken").toString
        val preDiscards   = parsePreDiscards(cmd)

        val discardTile = Tile.fromValue(discardTileId)
        val myDynamic   = st.dynamicAfterDiscard(discardTileId)

        val rewards = (0 until nRollouts).map { _ =>
          val shuffled = rng.shuffle(st.unknownBase)
          runRollout(myDynamic, st.myGroups, st.oppGroups, discardTile, shuffled,
                     st.actualRemaining, oppPolicy, selfPolicy, preDiscards)
        }.toList

        val n    = rewards.size.toDouble
        val mean = rewards.sum / n
        val variance = rewards.map(r => (r - mean) * (r - mean)).sum / n
        val std  = math.sqrt(variance)

        println(write(Map(
          "mean_reward" -> mean,
          "std_reward"  -> std,
          "rewards"     -> rewards
        )))
        System.out.flush()

      // ── Paired batch: all candidates evaluated on the SAME K worlds ─────────
      // Common-random-numbers massively reduces the variance of Q-value
      // *differences* between candidate tiles, which is what ranking needs.
      case "evaluate_batch" =>
        val st         = parseState(cmd)
        val candidates = asList(cmd("candidate_tiles")).map(asInt)
        val nWorlds    = asInt(cmd("n_worlds"))
        val oppPolicy  = cmd.getOrElse("rollout_opp", "chicken").toString
        val selfPolicy = cmd.getOrElse("self_policy", "firstfelix").toString
        val preDiscards = parsePreDiscards(cmd)

        val candTiles   = candidates.map(Tile.fromValue)
        val candDynamic = candidates.map(st.dynamicAfterDiscard)

        // rewards(k)(c) = reward of candidate c in world k.
        // Worlds run in parallel (-Drl.rolloutThreads=N); the shuffles are
        // drawn sequentially up front so results don't depend on scheduling.
        val worldSeeds = (0 until nWorlds).map(_ => rng.nextLong())
        val tasks = new java.util.ArrayList[java.util.concurrent.Callable[List[Double]]]()
        worldSeeds.foreach { seed =>
          tasks.add(new java.util.concurrent.Callable[List[Double]] {
            override def call(): List[Double] = {
              val shuffled = new Random(seed).shuffle(st.unknownBase)
              candTiles.zip(candDynamic).map { case (tile, dyn) =>
                runRollout(dyn, st.myGroups, st.oppGroups, tile, shuffled,
                           st.actualRemaining, oppPolicy, selfPolicy, preDiscards)
              }
            }
          })
        }
        val futures = rolloutPool.invokeAll(tasks)
        val rewards: List[List[Double]] =
          (0 until nWorlds).map(k => futures.get(k).get()).toList

        val nW = nWorlds.toDouble
        val meanRewards = candidates.indices.map { c =>
          rewards.map(_(c)).sum / nW
        }.toList

        println(write(Map(
          "tiles"        -> candidates,
          "mean_rewards" -> meanRewards,
          "rewards"      -> rewards
        )))
        System.out.flush()

      // ── Parity test: encode a state dict exactly like Python encode_state ───
      case "encode_v3" =>
        val handCounts = asList(cmd("hand")).map(asInt).toArray
        val myGroups   = parseGroups(cmd("my_groups").asInstanceOf[Map[String, Any]])
        val oppGroups  = asList(cmd("opp_groups"))
          .map(g => parseGroups(g.asInstanceOf[Map[String, Any]]))
        val discByPlayer = asList(cmd("discarded_by_player"))
          .map(row => asList(row).map(asInt).toArray).toArray
        val contextTile = cmd.get("context_tile").map(asInt)
        val obs = V3Obs.encode(
          handCounts, myGroups, oppGroups, discByPlayer,
          asInt(cmd("remaining")), asInt(cmd("my_id")),
          asInt(cmd("cur_player_id")), contextTile)
        println(write(Map("obs" -> obs.toList)))
        System.out.flush()

      case _ => // ignore unknown commands
    }
  }
}
