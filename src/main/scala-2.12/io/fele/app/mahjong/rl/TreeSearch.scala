package io.fele.app.mahjong.rl

import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong._
import io.fele.app.mahjong.player.Player

import scala.collection.mutable

/**
 * TreeSearch — determinized information-set MCTS (PIMC-UCT) support.
 *
 * A single search tree is shared across many determinized playouts ("root
 * sampling" / single-observer IS-MCTS). Each simulation samples a fresh world,
 * descends the tree by PUCT over OUR discard decisions, expands one leaf, and
 * plays the rest of the game out with the default (NN) policy; the terminal
 * seat-0 balance (rolloutTail = inf) — or the net value head at the leaf
 * (rolloutTail = zero) — is backed up along the path.
 *
 * The tree only branches on OUR (seat 0) discards; reactive win/pong/kong/chow
 * decisions use the network argmax inherited from NNPlayer, keeping the
 * branching factor small.
 *
 *   SearchNode — one of our discard decision points; holds a 34-way prior and a
 *                lazily-grown map of child edges keyed by tile value.
 *   Edge       — an action (discard tile) out of a node: visit count n, value
 *                sum w, virtual-loss counter, and the child node it leads to.
 *   TreePlayer — the seat-0 Player driving a single simulation; consults the
 *                shared tree in decideDiscard and records the descended path.
 *
 * See rl/mcts.py (SearchPolicy) and MCTSRolloutServer's "search" command.
 */

/** Thrown from a TreePlayer to abort a playout early when rolloutTail = zero,
  * carrying the network value-head estimate at the expanded leaf. It propagates
  * out of FlowImpl.resume (which wraps nothing) and is caught by the driver. */
case class LeafReached(value: Float) extends RuntimeException {
  override def fillInStackTrace(): Throwable = this   // control-flow, not an error
}

/**
 * Running min/max of backed-up Q values, shared across a whole search. Q here is
 * raw money (±tens of dollars), far outside PUCT's assumed [-1,1] range, so we
 * normalize Q into [0,1] before adding the exploration bonus (MuZero-style).
 * Without this the exploration term never competes with a large raw Q and PUCT
 * collapses onto the net's prior-preferred move.
 */
final class MinMaxStats {
  private var lo = Double.PositiveInfinity
  private var hi = Double.NegativeInfinity
  def update(v: Double): Unit = synchronized {
    if (v < lo) lo = v
    if (v > hi) hi = v
  }
  def normalize(v: Double): Double = synchronized {
    if (hi > lo) (v - lo) / (hi - lo) else 0.5
  }
}

/**
 * Root exploration noise. A confident (possibly wrong) prior makes PUCT collapse
 * onto the net's top move and barely search alternatives — exactly the moves the
 * ExIt loop needs the search to discover. AlphaZero mixes symmetric Dirichlet
 * noise into the root priors to force exploration; enable it for datagen (frac
 * ~0.25, alpha ~0.3) and disable it for deterministic evaluation.
 */
object Dirichlet {
  /** Gamma(alpha, 1) via Marsaglia–Tsang, with the alpha<1 boosting trick. */
  private def gamma(alpha: Double, rng: scala.util.Random): Double = {
    if (alpha < 1.0)
      return gamma(alpha + 1.0, rng) * math.pow(rng.nextDouble(), 1.0 / alpha)
    val d = alpha - 1.0 / 3.0
    val c = 1.0 / math.sqrt(9.0 * d)
    while (true) {
      var x = 0.0; var v = 0.0
      do { x = rng.nextGaussian(); v = 1.0 + c * x } while (v <= 0.0)
      v = v * v * v
      val u = rng.nextDouble()
      if (u < 1.0 - 0.0331 * x * x * x * x) return d * v
      if (math.log(u) < 0.5 * x * x + d * (1.0 - v + math.log(v))) return d * v
    }
    d   // unreachable
  }

  /** A length-n symmetric-Dirichlet(alpha) sample. */
  def sample(n: Int, alpha: Double, rng: scala.util.Random): Array[Double] = {
    val g   = Array.fill(n)(gamma(alpha, rng))
    val sum = g.sum
    if (sum <= 0.0) Array.fill(n)(1.0 / n) else g.map(_ / sum)
  }
}

/** An action out of a SearchNode. `prior` is fixed at creation; the rest mutate
  * under the parent node's monitor. */
final class Edge(val prior: Float) {
  var n: Int    = 0
  var w: Double = 0.0
  var vloss: Int = 0
  var child: SearchNode = null
}

/** One of our discard decision points. Guard all reads/writes of the mutable
  * fields with `this.synchronized` — many simulation threads share the tree. */
final class SearchNode {
  var expanded: Boolean = false
  var priors: Array[Float] = null       // softmax over all 34 discard logits
  var value:  Float = 0f                 // net value head at this info set
  var totalN: Int = 0                    // visits that have passed through here
  val children = mutable.Map[Int, Edge]()

  /** Record the net's prediction the first time this node is reached. */
  def expand(priors34: Array[Float], v: Float): Unit = {
    priors = priors34
    value  = v
    expanded = true
  }

  /** Edge for tile value `t`, created lazily with the stored prior. Synchronized
    * because concurrent simulations share the tree and mutate `children` (a plain
    * map is unsafe under concurrent structural change even for distinct keys). The
    * monitor is reentrant, so callers already holding it are unaffected. */
  def edgeOf(t: Int): Edge = synchronized {
    children.getOrElseUpdate(t, new Edge(if (priors != null) priors(t) else 1f / 34f))
  }

  /** Child node for tile `t`, created lazily. */
  def childOf(t: Int): SearchNode = synchronized {
    val e = edgeOf(t)
    if (e.child == null) e.child = new SearchNode()
    e.child
  }

  /**
   * PUCT selection restricted to the tiles legal in the current world.
   *   score(a) = Qnorm(a) + cPuct * P(a) * sqrt(totalN) / (1 + n(a) + vloss(a))
   * Q is normalized into [0,1] by the shared MinMaxStats so the exploration
   * bonus is comparable to it. A virtual loss inflates the visit denominator so
   * concurrent descents diverge. Unvisited edges take the neutral Qnorm = 0.
   * Returns the chosen tile value.
   */
  def puctSelect(valid: Seq[Int], cPuct: Double, stats: MinMaxStats): Int = {
    val sqrtN = math.sqrt(math.max(1, totalN).toDouble)
    var bestTile  = valid.head
    var bestScore = Double.NegativeInfinity
    var i = 0
    while (i < valid.length) {
      val t = valid(i)
      val e = edgeOf(t)
      val nEff = e.n + e.vloss
      val q = if (e.n == 0) 0.0 else stats.normalize(e.w / e.n)
      val u = cPuct * e.prior * sqrtN / (1.0 + nEff)
      val score = q + u
      if (score > bestScore) { bestScore = score; bestTile = t }
      i += 1
    }
    bestTile
  }
}

/**
 * Seat-0 player for the CRN-preserving flat + value-leaf hybrid (issue #20).
 *
 * Plays the default NN policy, but at its `plies`-th own discard decision
 * (counting from just after the root forced discard) it aborts the playout
 * with the dedicated value net's estimate of the current information set,
 * via LeafReached. plies = 1 ⇒ bootstrap at our first decision after the
 * root discard, so the rollout sees the opponents' responses and our draw
 * (a genuine 1-ply lookahead) but never lets a deep playout compound value
 * -net error. If the game ends before the leaf (someone wins, wall empty),
 * the real terminal balance is used instead — LeafReached is never thrown.
 *
 * Used by evaluate_batch (value_leaf_plies > 0): all candidates share the
 * same determinized world, and the value net is deterministic, so paired
 * Q-differences keep evaluate_batch's low-variance CRN structure.
 */
final class ValueLeafPlayer(
  id: Int,
  tiles: List[Tile],
  tileGroups: List[TileGroup],
  svc: OnnxPolicyService,
  valueSvc: OnnxPolicyService,
  plies: Int
)(implicit c: Config) extends NNPlayer(id, tiles, tileGroups, svc)(c) {

  private var decisions = 0

  override def decideDiscard(cs: CurState): Tile = {
    decisions += 1
    if (decisions >= plies)
      throw LeafReached(valueSvc.query(V3Obs.fromCurState(id, cs, None)).value)
    super.decideDiscard(cs)
  }
}

/**
 * Seat-0 player for a single simulation. Constructed fresh per playout (like
 * the players in runRollout) with references to the shared tree; carries the
 * per-simulation descended `path` used for backup.
 *
 * @param root           shared root node (already expanded by the driver)
 * @param rootTile       the root discard the driver PUCT-selected for this sim
 * @param cPuct          PUCT exploration constant
 * @param stats          shared Q min/max normalizer for this search
 * @param maxDepth       max tree plies (our discards chosen by PUCT, incl. the
 *                       root). maxDepth = 1 ⇒ only the root branches (reproduces
 *                       the flat evaluate_batch behaviour) and every subsequent
 *                       discard is the default NN policy.
 * @param rolloutTailZero true ⇒ bootstrap the leaf with the net value head
 *                        (throws LeafReached); false ⇒ play out to game end.
 */
final class TreePlayer(
  id: Int,
  tiles: List[Tile],
  tileGroups: List[TileGroup],
  svc: OnnxPolicyService,
  valueSvc: OnnxPolicyService,   // dedicated value net for leaf bootstrap (may be null)
  root: SearchNode,
  rootTile: Int,
  cPuct: Double,
  stats: MinMaxStats,
  maxDepth: Int,
  rolloutTailZero: Boolean
)(implicit c: Config) extends NNPlayer(id, tiles, tileGroups, svc)(c) {

  // The path already includes the driver-selected root edge.
  val path: mutable.ListBuffer[(SearchNode, Int)] =
    mutable.ListBuffer((root, rootTile))
  private var currentNode: SearchNode = root.childOf(rootTile)
  private var inRolloutTail: Boolean = false

  override def decideDiscard(cs: CurState): Tile = {
    // Past the tree frontier or depth cap: play the default (NN argmax) policy.
    if (inRolloutTail || path.length >= maxDepth)
      return super.decideDiscard(cs)

    val valid = hand.dynamicTiles.map(_.toTileValue).distinct
    if (valid.isEmpty) return super.decideDiscard(cs)   // shouldn't happen

    val node = currentNode
    node.synchronized {
      if (!node.expanded) {
        val obs = V3Obs.fromCurState(id, cs, None)
        val out = svc.query(obs)
        // Leaf value: the dedicated value net if provided, else the policy net's
        // (untrained) value head. Priors always come from the policy net.
        val leafV = if (valueSvc != null) valueSvc.query(obs).value else out.value
        node.expand(softmax(out.discard), leafV)
        if (rolloutTailZero) throw LeafReached(leafV)
        // Expand-one-node-per-simulation: select once here, then roll out.
        inRolloutTail = true
      }
      val a = node.puctSelect(valid, cPuct, stats)
      val e = node.edgeOf(a)
      e.vloss += 1
      path += ((node, a))
      currentNode = node.childOf(a)
      Tile.fromValue(a)
    }
  }

  private def softmax(logits: Array[Float]): Array[Float] = {
    var mx = Float.NegativeInfinity
    var i = 0
    while (i < logits.length) { if (logits(i) > mx) mx = logits(i); i += 1 }
    val out = new Array[Float](logits.length)
    var sum = 0.0
    i = 0
    while (i < logits.length) { val e = math.exp(logits(i) - mx); out(i) = e.toFloat; sum += e; i += 1 }
    i = 0
    while (i < logits.length) { out(i) = (out(i) / sum).toFloat; i += 1 }
    out
  }
}
