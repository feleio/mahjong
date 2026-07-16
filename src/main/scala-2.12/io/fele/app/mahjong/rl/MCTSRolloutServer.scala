package io.fele.app.mahjong.rl

import io.fele.app.mahjong._
import io.fele.app.mahjong.player.{Chicken, FirstFelix, Player}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

import scala.util.Random

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

  // Dedicated value net for IS-MCTS leaf bootstrap (opt-in via
  // -Drl.valuemodel=<path.onnx>). Its value head is outcome-trained; the policy
  // net's own value head is untrained, so leaves fall back to it only if unset.
  private val valuePath = System.getProperty("rl.valuemodel")
  private lazy val valueService: OnnxPolicyService = new OnnxPolicyService(valuePath)
  private def valueNet: OnnxPolicyService = if (valuePath != null) valueService else null

  // Opponent hand-belief model (opt-in via -Drl.belief=<path.onnx>). When set,
  // opponents' hidden hands are determinized by belief-weighted sampling from
  // the unseen pool instead of a uniform shuffle. Uniform stays the fallback.
  private val beliefPath = System.getProperty("rl.belief")
  private lazy val beliefService: BeliefService = new BeliefService(beliefPath)
  private def beliefEnabled: Boolean = beliefPath != null

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

  /** Uniform determinization: split a shuffled unseen pool into draw pile +
    * opponents' hidden hands (the search's default behaviour). */
  private def determinizeUniform(
    st: ParsedState, remaining: Int, rng: Random
  ): (List[List[Tile]], Seq[Tile]) = {
    val shuffled  = rng.shuffle(st.unknownBase)
    val drawPile  = shuffled.take(remaining)
    var oppHidden = shuffled.drop(remaining).toList
    val oppDynamic = st.oppGroups.map { grps =>
      val nDynamic = Math.max(0, 13 - 3 * grps.size)
      val chunk    = oppHidden.take(nDynamic)
      oppHidden    = oppHidden.drop(nDynamic)
      chunk
    }
    (oppDynamic, drawPile)
  }

  /** Belief-weighted determinization: for each opponent, sample their hidden
    * tiles from the unseen pool WITHOUT replacement, weighting each remaining
    * tile type by beliefProbs(i)(type) × copies-left. Leftover pool → draw pile. */
  private def determinizeBelief(
    st: ParsedState, remaining: Int, beliefProbs: Array[Array[Float]], rng: Random
  ): (List[List[Tile]], Seq[Tile]) = {
    val pool = Array.fill[Int](34)(0)
    st.unknownBase.foreach(t => pool(t.toTileValue) += 1)

    val oppDynamic = st.oppGroups.zipWithIndex.map { case (grps, i) =>
      val nDynamic = Math.max(0, 13 - 3 * grps.size)
      val probs    = beliefProbs(i)
      val buf      = new scala.collection.mutable.ListBuffer[Tile]
      var drawn    = 0
      while (drawn < nDynamic) {
        var total = 0.0
        var t = 0
        val w = new Array[Double](34)
        while (t < 34) {
          w(t)   = pool(t).toDouble * math.max(1e-6, probs(t).toDouble)
          total += w(t)
          t += 1
        }
        if (total <= 0.0) drawn = nDynamic          // pool exhausted; bail out
        else {
          var r = rng.nextDouble() * total
          var chosen = -1
          t = 0
          while (t < 34 && chosen < 0) {
            r -= w(t)
            if (r <= 0.0 && pool(t) > 0) chosen = t
            t += 1
          }
          if (chosen < 0) {                           // numerical guard: last non-empty
            t = 33; while (t >= 0 && chosen < 0) { if (pool(t) > 0) chosen = t; t -= 1 }
          }
          pool(chosen) -= 1
          buf += Tile.fromValue(chosen)
          drawn += 1
        }
      }
      buf.toList
    }

    val leftover = (0 until 34).flatMap(id => List.fill(pool(id))(Tile.fromValue(id)))
    (oppDynamic, rng.shuffle(leftover))
  }

  private def runRollout(
    myDynamic:   List[Tile],
    myGroups:    List[TileGroup],
    oppGroups:   List[List[TileGroup]],  // 3 opponents (relative seats 1-3)
    discardTile: Tile,
    oppDynamic:  List[List[Tile]],       // determinized opponent hidden hands
    drawPile:    Seq[Tile],              // determinized draw pile
    oppPolicy:   String,                // "chicken" | "firstfelix" | "nn"
    selfPolicy:  String,                // "chicken" | "firstfelix" | "nn"
    preDiscards: List[DiscardInfo] = Nil, // real discard history (NN policies read it)
    valueLeafPlies: Int = 0             // > 0 ⇒ value-net bootstrap at our Nth decision
  ): Double = {

    // Verify hand sizes are valid; skip rollout if not
    val handsOk = (0 until 3).forall { i =>
      val nDynamic = oppDynamic(i).size
      val nGroups  = oppGroups(i).size
      nDynamic + nGroups * 3 == 13
    } && myDynamic.size + myGroups.size * 3 == 13

    if (!handsOk) return 0.0

    // --- Build players ---
    // With valueLeafPlies > 0 our seat is the value-leaf player (NN policy up to
    // the leaf, then LeafReached with the value net's estimate); selfPolicy is
    // effectively "nn" for the few moves before the bootstrap.
    val us: Player =
      if (valueLeafPlies > 0)
        new ValueLeafPlayer(0, myDynamic, myGroups, nnService, valueNet, valueLeafPlies)
      else makePlayer(selfPolicy, 0, myDynamic, myGroups)
    val opps: List[Player] = (0 until 3)
      .map(i => makePlayer(oppPolicy, i + 1, oppDynamic(i), oppGroups(i))).toList
    val players: List[Player] = us :: opps

    val drawer = new FixedDrawer(drawPile)
    val state  = GameState(players, None, preDiscards, 0, drawer)
    val flow   = new FlowImpl(state)

    try seat0Balance(flow.resume(Some(discardTile)))
    catch {
      case LeafReached(v) => v.toDouble
      case _: Throwable   => 0.0
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

  /** Raw 4×34 per-player discard counts in rollout seat order (0 = us). */
  private def parseDiscByPlayer(cmd: Map[String, Any]): Array[Array[Int]] =
    cmd.get("discards_by_player") match {
      case Some(raw) => asList(raw).map(row => asList(row).map(asInt).toArray).toArray
      case None      => Array.fill(4)(new Array[Int](34))
    }

  /** 0/1 meld planes for one opponent's fixed groups (matches BeliefDataGen). */
  private def meldPlanes(groups: List[TileGroup]): (Array[Float], Array[Float], Array[Float]) = {
    val pong = new Array[Float](34); val kong = new Array[Float](34); val chow = new Array[Float](34)
    groups.foreach {
      case PongGroup(t)  => pong(t.toTileValue) = 1f
      case KongGroup(t)  => kong(t.toTileValue) = 1f
      case ChowGroup(ts) => ts.foreach(t => chow(t.toTileValue) = 1f)
    }
    (pong, kong, chow)
  }

  /** Per-opponent belief distribution over held tile types, computed once for a
    * decision (the public trail is fixed across worlds). Features/masking mirror
    * belief_train.load exactly: feat = concat(disc/4, pong, kong, chow,
    * [hand_size/13, remaining/136]); avail = clip(4 - disc - 3*pong - 4*kong - chow, 0, 4).
    * `discByPlayer` is 4×34 in rollout seat order (0 = us, 1-3 = opp_groups order). */
  private def beliefProbsPerOpp(
    st: ParsedState, discByPlayer: Array[Array[Int]], remaining: Int
  ): Array[Array[Float]] =
    st.oppGroups.zipWithIndex.map { case (grps, i) =>
      val disc = if (i + 1 < discByPlayer.length) discByPlayer(i + 1) else new Array[Int](34)
      val (pong, kong, chow) = meldPlanes(grps)
      val handSize = Math.max(0, 13 - 3 * grps.size)

      val feat = new Array[Float](138)
      var t = 0
      while (t < 34) {
        feat(t)        = disc(t) / 4f
        feat(34 + t)   = pong(t)
        feat(68 + t)   = kong(t)
        feat(102 + t)  = chow(t)
        t += 1
      }
      feat(136) = handSize / 13f
      feat(137) = remaining / 136f

      val avail = new Array[Float](34)
      t = 0
      while (t < 34) {
        val a = 4f - disc(t) - 3f * pong(t) - 4f * kong(t) - chow(t)
        avail(t) = math.max(0f, math.min(4f, a))
        t += 1
      }
      beliefService.query(feat, avail)
    }.toArray

  // ── Determinized information-set MCTS (the "search" command) ─────────────────

  private def softmax34(logits: Array[Float]): Array[Float] = {
    val mx  = logits.max
    val exp = logits.map(l => math.exp(l - mx))
    val sum = exp.sum
    exp.map(e => (e / sum).toFloat)
  }

  /** Seat-0 terminal balance of a finished game (0.0 for a draw/no winner). */
  private def seat0Balance(result: GameResult): Double =
    result.winnersInfo
      .flatMap(_.winnersBalance.find(_.id == 0).map(_.amount.toDouble))
      .getOrElse(0.0)

  private def backup(path: Seq[(SearchNode, Int)], value: Double, stats: MinMaxStats): Unit =
    path.foreach { case (node, a) =>
      node.synchronized {
        val e = node.edgeOf(a)
        e.n += 1
        e.w += value
        e.vloss = math.max(0, e.vloss - 1)
        node.totalN += 1
        stats.update(e.w / e.n)
      }
    }

  /** Run one simulation and return (path, value) for backup by the caller. */
  private def simulate(
    st: ParsedState, root: SearchNode, candidates: List[Int],
    beliefProbs: Array[Array[Float]], preDiscards: List[DiscardInfo],
    oppPolicy: String, cPuct: Double, stats: MinMaxStats,
    maxDepth: Int, rolloutTailZero: Boolean, seed: Long
  ): (Seq[(SearchNode, Int)], Double) = {
    val wr = new Random(seed)
    val (oppDynamic, drawPile) =
      if (beliefEnabled) determinizeBelief(st, st.actualRemaining, beliefProbs, wr)
      else               determinizeUniform(st, st.actualRemaining, wr)

    val rootTile = root.synchronized {
      val t = root.puctSelect(candidates, cPuct, stats)
      root.edgeOf(t).vloss += 1
      t
    }

    val myDynamic = st.dynamicAfterDiscard(rootTile)
    val handsOk = myDynamic.size + st.myGroups.size * 3 == 13 &&
      (0 until 3).forall(i => oppDynamic(i).size + st.oppGroups(i).size * 3 == 13)

    if (!handsOk) {
      root.synchronized { val e = root.edgeOf(rootTile); e.vloss = math.max(0, e.vloss - 1) }
      return (Nil, 0.0)
    }

    val us = new TreePlayer(0, myDynamic, st.myGroups, nnService, valueNet, root, rootTile,
      cPuct, stats, maxDepth, rolloutTailZero)
    val opps = (0 until 3)
      .map(i => makePlayer(oppPolicy, i + 1, oppDynamic(i), st.oppGroups(i))).toList
    val drawer = new FixedDrawer(drawPile)
    val state  = GameState(us :: opps, None, preDiscards, 0, drawer)
    val flow   = new FlowImpl(state)

    val value: Double =
      try seat0Balance(flow.resume(Some(Tile.fromValue(rootTile))))
      catch {
        case LeafReached(v) => v.toDouble
        case _: Throwable   => 0.0
      }
    (us.path.toList, value)
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

        val beliefProbs =
          if (beliefEnabled) beliefProbsPerOpp(st, parseDiscByPlayer(cmd), st.actualRemaining)
          else null
        val rewards = (0 until nRollouts).map { _ =>
          val (oppDynamic, drawPile) =
            if (beliefEnabled) determinizeBelief(st, st.actualRemaining, beliefProbs, rng)
            else determinizeUniform(st, st.actualRemaining, rng)
          runRollout(myDynamic, st.myGroups, st.oppGroups, discardTile, oppDynamic, drawPile,
                     oppPolicy, selfPolicy, preDiscards)
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
      // value_leaf_plies > 0 (issue #20) keeps the CRN structure but stops each
      // rollout at our Nth discard decision after the root and bootstraps the
      // value net there instead of playing to game end (needs -Drl.valuemodel).
      case "evaluate_batch" =>
        val st         = parseState(cmd)
        val candidates = asList(cmd("candidate_tiles")).map(asInt)
        val nWorlds    = asInt(cmd("n_worlds"))
        val oppPolicy  = cmd.getOrElse("rollout_opp", "chicken").toString
        val selfPolicy = cmd.getOrElse("self_policy", "firstfelix").toString
        val preDiscards = parsePreDiscards(cmd)
        val valueLeafPlies = asInt(cmd.getOrElse("value_leaf_plies", 0))
        require(valueLeafPlies == 0 || valuePath != null,
          "value_leaf_plies > 0 needs -Drl.valuemodel=<path.onnx>")

        val candTiles   = candidates.map(Tile.fromValue)
        val candDynamic = candidates.map(st.dynamicAfterDiscard)

        // rewards(k)(c) = reward of candidate c in world k.
        // Worlds run in parallel (-Drl.rolloutThreads=N); the shuffles are
        // drawn sequentially up front so results don't depend on scheduling.
        // Belief distributions are fixed for this decision (public trail doesn't
        // vary across worlds) → compute once, reuse in every world's sampling.
        val beliefProbs =
          if (beliefEnabled) beliefProbsPerOpp(st, parseDiscByPlayer(cmd), st.actualRemaining)
          else null

        val worldSeeds = (0 until nWorlds).map(_ => rng.nextLong())
        val tasks = new java.util.ArrayList[java.util.concurrent.Callable[List[Double]]]()
        worldSeeds.foreach { seed =>
          tasks.add(new java.util.concurrent.Callable[List[Double]] {
            override def call(): List[Double] = {
              // One determinization per world, shared by all candidates (CRN).
              val wr = new Random(seed)
              val (oppDynamic, drawPile) =
                if (beliefEnabled) determinizeBelief(st, st.actualRemaining, beliefProbs, wr)
                else determinizeUniform(st, st.actualRemaining, wr)
              candTiles.zip(candDynamic).map { case (tile, dyn) =>
                runRollout(dyn, st.myGroups, st.oppGroups, tile, oppDynamic, drawPile,
                           oppPolicy, selfPolicy, preDiscards, valueLeafPlies)
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

      // ── What would FirstFelix discard from this exact seat? ─────────────────
      // Champion-vs-FF analysis (rl/compare_vs_ff.py): a fresh FirstFelix is
      // dropped into our seat — it re-decides its target from this hand + the
      // discard history, then picks its discard. Deterministic.
      case "ff_decide" =>
        val st = parseState(cmd)
        val preDiscards = parsePreDiscards(cmd)
        val handTiles = st.handCounts.zipWithIndex.flatMap { case (cnt, id) =>
          List.fill(cnt)(Tile.fromValue(id))
        }.toList
        // Player's ctor wants a 13-tile hand (incl. groups); the 14th (drawn)
        // tile is added afterwards, mirroring runRollout's construction.
        val ff = new FirstFelix(0, handTiles.tail, 5, st.myGroups)
        ff.hand.add(handTiles.head)
        val cs = CurState(ff.privateInfo, st.oppGroups.map(PublicState),
          preDiscards, 0, st.actualRemaining)
        println(write(Map("tile" -> ff.decideDiscard(cs).toTileValue)))
        System.out.flush()

      // ── Parity test: v4 = v3 + discard-order planes (issue #21) ─────────────
      case "encode_v4" =>
        val handCounts = asList(cmd("hand")).map(asInt).toArray
        val myGroups   = parseGroups(cmd("my_groups").asInstanceOf[Map[String, Any]])
        val oppGroups  = asList(cmd("opp_groups"))
          .map(g => parseGroups(g.asInstanceOf[Map[String, Any]]))
        val discByPlayer = asList(cmd("discarded_by_player"))
          .map(row => asList(row).map(asInt).toArray).toArray
        val contextTile = cmd.get("context_tile").map(asInt)
        val discardSeq = asList(cmd.getOrElse("discard_seq", List.empty[Any])).map { row =>
          val pair = asList(row).map(asInt)
          (pair(0), pair(1))
        }
        val obs = V4Obs.encode(
          handCounts, myGroups, oppGroups, discByPlayer,
          asInt(cmd("remaining")), asInt(cmd("my_id")),
          asInt(cmd("cur_player_id")), contextTile, discardSeq)
        println(write(Map("obs" -> obs.toList)))
        System.out.flush()

      // ── Information-set MCTS: a real PUCT tree over our discard sequence ─────
      case "search" =>
        val st          = parseState(cmd)
        val preDiscards = parsePreDiscards(cmd)
        val discByPlayer = parseDiscByPlayer(cmd)
        val remaining    = asInt(cmd.getOrElse("remaining", st.actualRemaining))

        // Root candidate discards: an explicit list, or all distinct tiles held.
        val candidates: List[Int] = cmd.get("candidate_tiles") match {
          case Some(raw) => asList(raw).map(asInt)
          case None      => st.handCounts.zipWithIndex.filter(_._1 > 0).map(_._2)
        }

        val sims       = asInt(cmd.getOrElse("sims", 256))
        val cPuct      = cmd.get("c_puct").map(_.toString.toDouble).getOrElse(1.5)
        val tau        = cmd.get("temperature").map(_.toString.toDouble).getOrElse(1.0)
        val maxDepth   = asInt(cmd.getOrElse("max_depth", 64))
        val nParallel  = math.max(1, asInt(cmd.getOrElse("n_parallel", 1)))
        val oppPolicy  = cmd.getOrElse("rollout_opp", "firstfelix").toString
        val rolloutTailZero = cmd.getOrElse("rollout_tail", "inf").toString == "zero"
        val dirFrac    = cmd.get("root_dirichlet_frac").map(_.toString.toDouble).getOrElse(0.0)
        val dirAlpha   = cmd.get("root_dirichlet_alpha").map(_.toString.toDouble).getOrElse(0.3)

        val beliefProbs =
          if (beliefEnabled) beliefProbsPerOpp(st, discByPlayer, st.actualRemaining) else null

        // Expand the root once from the pre-discard (14-tile) observation.
        val root = new SearchNode()
        val rootObs = V3Obs.encode(st.handCounts.toArray, st.myGroups, st.oppGroups,
          discByPlayer, remaining, 0, 0, None)
        val rootOut = nnService.query(rootObs)
        root.expand(softmax34(rootOut.discard), rootOut.value)

        // Override root priors with the decision net's prior over candidates (the
        // student prior loaded here is only for the rollout tail / deeper nodes).
        cmd.get("candidate_priors").foreach { raw =>
          val cp = asList(raw).map(_.toString.toDouble)
          candidates.zip(cp).foreach { case (t, p) => root.priors(t) = p.toFloat }
        }

        val baseRng = cmd.get("seed").map(v => new Random(asInt(v).toLong)).getOrElse(rng)

        // Optional AlphaZero root exploration noise (datagen only).
        if (dirFrac > 0.0 && candidates.nonEmpty) {
          val noise = Dirichlet.sample(candidates.size, dirAlpha, baseRng)
          candidates.zipWithIndex.foreach { case (t, i) =>
            root.priors(t) = ((1.0 - dirFrac) * root.priors(t) + dirFrac * noise(i)).toFloat
          }
        }

        val seeds   = (0 until sims).map(_ => baseRng.nextLong())
        val stats   = new MinMaxStats()

        // Run sims in batches of nParallel on the shared rollout pool; virtual
        // loss decorrelates concurrent descents. Backup is serialized per node.
        seeds.grouped(nParallel).foreach { batch =>
          val tasks = new java.util.ArrayList[java.util.concurrent.Callable[(Seq[(SearchNode, Int)], Double)]]()
          batch.foreach { s =>
            tasks.add(new java.util.concurrent.Callable[(Seq[(SearchNode, Int)], Double)] {
              override def call(): (Seq[(SearchNode, Int)], Double) =
                simulate(st, root, candidates, beliefProbs, preDiscards, oppPolicy,
                  cPuct, stats, maxDepth, rolloutTailZero, s)
            })
          }
          val futures = rolloutPool.invokeAll(tasks)
          (0 until batch.size).foreach { i =>
            val (path, value) = futures.get(i).get()
            backup(path, value, stats)
          }
        }

        // Read the root edges over the candidate set for the response.
        val visits = candidates.map(t => root.children.get(t).map(_.n).getOrElse(0))
        val qs     = candidates.map { t =>
          root.children.get(t) match {
            case Some(e) if e.n > 0 => e.w / e.n
            case _                  => 0.0
          }
        }
        val priors = candidates.map(t => root.priors(t).toDouble)
        val powed  = visits.map(v => math.pow(v.toDouble, 1.0 / math.max(1e-6, tau)))
        val pSum   = powed.sum
        val policy = if (pSum > 0) powed.map(_ / pSum) else visits.map(_ => 1.0 / candidates.size)

        println(write(Map(
          "tiles"  -> candidates,
          "visits" -> visits,
          "q"      -> qs,
          "prior"  -> priors,
          "value"  -> root.value.toDouble,
          "policy" -> policy
        )))
        System.out.flush()

      case _ => // ignore unknown commands
    }
  }
}
