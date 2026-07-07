package io.fele.app.mahjong.rl

import java.util.concurrent.{CompletableFuture, LinkedBlockingQueue}

import ai.onnxruntime.{OnnxTensor, OrtEnvironment, OrtSession}
import io.fele.app.mahjong._
import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.player.Player

/**
 * NN-guided rollout policy support:
 *
 *   V3Obs             — Scala port of Python env.encode_state(version=3);
 *                       MUST stay byte-for-byte consistent (see encode_v3
 *                       parity command in MCTSRolloutServer / test_v3obs.py).
 *   OnnxPolicyService — batched ONNX Runtime inference shared by all rollout
 *                       threads (single inference thread drains a queue).
 *   NNPlayer          — Player whose decisions are greedy masked argmax of
 *                       the network heads.
 */

object V3Obs {
  val DIM = 759

  private def groupPhysicalTiles(groups: List[TileGroup]): List[Tile] =
    groups.flatMap {
      case PongGroup(t)  => List.fill(3)(t)
      case KongGroup(t)  => List.fill(4)(t)
      case ChowGroup(ts) => ts.toList
    }

  /**
   * Mirror of encode_state(state, version=3, context).
   *
   * handCounts   : 34 counts of dynamic hand tiles
   * myGroups     : our fixed groups
   * oppGroups    : 3 opponents' fixed groups, seating order relative to obs ids
   * discByPlayer : 4×34 discard counts indexed by the same ids as myId/curId
   * contextTile  : offered/drawn tile for win/pong/kong/chow/self_win decisions
   */
  def encode(handCounts: Array[Int],
             myGroups: List[TileGroup],
             oppGroups: List[List[TileGroup]],
             discByPlayer: Array[Array[Int]],
             remaining: Int,
             myId: Int,
             curPlayerId: Int,
             contextTile: Option[Int]): Array[Float] = {
    val obs = new Array[Float](DIM)
    var ptr = 0

    def writeGroupPlanes(groups: List[TileGroup]): Unit = {
      val pong = new Array[Float](34)
      val kong = new Array[Float](34)
      val chow = new Array[Float](34)
      groups.foreach {
        case PongGroup(t)  => pong(t.toTileValue) = 1f
        case KongGroup(t)  => kong(t.toTileValue) = 1f
        case ChowGroup(ts) => ts.foreach(t => chow(t.toTileValue) = 1f)
      }
      Array(pong, kong, chow).foreach { plane =>
        System.arraycopy(plane, 0, obs, ptr, 34); ptr += 34
      }
    }

    // hand counts / 4
    (0 until 34).foreach(i => obs(ptr + i) = handCounts(i) / 4f)
    ptr += 34

    writeGroupPlanes(myGroups)
    (0 until 3).foreach(j => writeGroupPlanes(
      if (j < oppGroups.size) oppGroups(j) else List.empty))

    // per-player discard counts / 4
    (0 until 4).foreach { p =>
      (0 until 34).foreach(i => obs(ptr + i) = discByPlayer(p)(i) / 4f)
      ptr += 34
    }

    obs(ptr) = remaining / 136f; ptr += 1
    obs(ptr + myId) = 1f; ptr += 4
    obs(ptr + curPlayerId) = 1f; ptr += 4

    // ── v3 feature planes ────────────────────────────────────────────────────
    val discardTotals = Array.tabulate(34)(i =>
      (0 until 4).map(p => discByPlayer(p)(i)).sum)

    val seen = Array.tabulate(34)(i => handCounts(i) + discardTotals(i))
    groupPhysicalTiles(myGroups).foreach(t => seen(t.toTileValue) += 1)
    oppGroups.foreach(gs =>
      groupPhysicalTiles(gs).foreach(t => seen(t.toTileValue) += 1))
    val unseen = Array.tabulate(34)(i => math.max(0, 4 - seen(i)))

    val fixed = myGroups.size
    val dyn = handCounts.sum
    val (curSh, shAfter, ukAfter, improve) =
      if (dyn % 3 == 2) {
        val (sh, uk) = Shanten.discardFeatures(handCounts.clone(), fixed, unseen)
        (sh.min, sh, uk, Array.fill(34)(0))
      } else {
        val (sh, imp) = Shanten.improveTiles(handCounts.clone(), fixed, unseen)
        (sh, Array.fill(34)(9), Array.fill(34)(0), imp)
      }

    (0 until 34).foreach(i => obs(ptr + i) = (9f - shAfter(i)) / 10f)
    ptr += 34
    (0 until 34).foreach(i =>
      obs(ptr + i) = math.min(1f, math.max(0f, ukAfter(i) / 60f)))
    ptr += 34
    (0 until 34).foreach(i =>
      obs(ptr + i) = math.min(1f, math.max(0f, improve(i) / 4f)))
    ptr += 34
    (0 until 34).foreach(i =>
      obs(ptr + i) = math.min(1f, math.max(0f, (4f - seen(i)) / 4f)))
    ptr += 34

    contextTile.foreach(t => obs(ptr + t) = 1f)
    ptr += 34

    obs(ptr) = (9f - curSh) / 10f
    obs(ptr + 1) = if (dyn % 3 == 2) 1f else 0f
    ptr += 2

    require(ptr == DIM, s"V3Obs dim mismatch: $ptr != $DIM")
    obs
  }

  /** Build the observation from a live CurState (rollout-side). */
  def fromCurState(myId: Int, cs: CurState, contextTile: Option[Int]): Array[Float] = {
    val handCounts = new Array[Int](34)
    cs.myInfo.tiles.foreach(t => handCounts(t.toTileValue) += 1)
    val discByPlayer = Array.fill(4)(new Array[Int](34))
    cs.discards.foreach(d => discByPlayer(d.playerId)(d.tile.toTileValue) += 1)
    encode(handCounts, cs.myInfo.tileGroups, cs.otherInfos.map(_.tileGroups),
           discByPlayer, cs.remainTileNum, myId, cs.curPlayerId, contextTile)
  }
}

case class PolicyOut(discard: Array[Float], win: Array[Float],
                     selfWin: Array[Float], pong: Array[Float],
                     kong: Array[Float], chow: Array[Float],
                     selfKong: Array[Float], value: Float)

/**
 * Batched ONNX inference: rollout threads enqueue observations and block;
 * one daemon thread drains the queue and runs a single batched forward.
 */
class OnnxPolicyService(modelPath: String, maxBatch: Int = 256) {
  private val env = OrtEnvironment.getEnvironment
  private val opts = new OrtSession.SessionOptions()
  opts.setIntraOpNumThreads(Integer.getInteger("rl.onnxthreads", 2))
  private val session = env.createSession(modelPath, opts)

  private case class Req(obs: Array[Float], promise: CompletableFuture[PolicyOut])
  private val queue = new LinkedBlockingQueue[Req]()

  private val worker = new Thread(new Runnable {
    override def run(): Unit = loop()
  }, "onnx-batcher")
  worker.setDaemon(true)
  worker.start()

  def query(obs: Array[Float]): PolicyOut = {
    val p = new CompletableFuture[PolicyOut]()
    queue.put(Req(obs, p))
    p.get()
  }

  private def loop(): Unit = while (true) {
    val first = queue.take()
    val reqs = new java.util.ArrayList[Req]()
    reqs.add(first)
    queue.drainTo(reqs, maxBatch - 1)
    val b = reqs.size
    try {
      val flat = new Array[Float](b * V3Obs.DIM)
      (0 until b).foreach(i =>
        System.arraycopy(reqs.get(i).obs, 0, flat, i * V3Obs.DIM, V3Obs.DIM))
      val tensor = OnnxTensor.createTensor(
        env, java.nio.FloatBuffer.wrap(flat), Array(b.toLong, V3Obs.DIM.toLong))
      val out = session.run(java.util.Collections.singletonMap("obs", tensor))
      try {
        def mat(i: Int): Array[Array[Float]] =
          out.get(i).getValue.asInstanceOf[Array[Array[Float]]]
        val (d, w, sw, po, ko, ch, sk) =
          (mat(0), mat(1), mat(2), mat(3), mat(4), mat(5), mat(6))
        val v = out.get(7).getValue.asInstanceOf[Array[Float]]
        (0 until b).foreach { i =>
          reqs.get(i).promise.complete(
            PolicyOut(d(i), w(i), sw(i), po(i), ko(i), ch(i), sk(i), v(i)))
        }
      } finally {
        out.close()
        tensor.close()
      }
    } catch {
      case e: Throwable =>
        (0 until b).foreach(i => reqs.get(i).promise.completeExceptionally(e))
    }
  }
}

/** Rollout player driven by greedy masked argmax over the network heads. */
class NNPlayer(id: Int, tiles: List[Tile], tileGroups: List[TileGroup],
               svc: OnnxPolicyService)(implicit c: Config)
    extends Player(id, tiles, tileGroups)(c) {

  private def out(cs: CurState, contextTile: Option[Int]): PolicyOut =
    svc.query(V3Obs.fromCurState(id, cs, contextTile))

  private def maskedArgmax(logits: Array[Float], valid: Iterable[Int]): Int =
    valid.maxBy(logits(_))

  override def decideDiscard(cs: CurState): Tile = {
    val logits = out(cs, None).discard
    val valid = hand.dynamicTiles.map(_.toTileValue).distinct
    Tile.fromValue(maskedArgmax(logits, valid))
  }

  override def decideWin(tile: Tile, score: Int, cs: CurState): Boolean =
    maskedArgmax(out(cs, Some(tile.toTileValue)).win, List(0, 1)) == 1

  override def decideSelfWin(tile: Tile, score: Int, cs: CurState): Boolean =
    maskedArgmax(out(cs, Some(tile.toTileValue)).selfWin, List(0, 1)) == 1

  override def decidePong(tile: Tile, cs: CurState): Boolean =
    maskedArgmax(out(cs, Some(tile.toTileValue)).pong, List(0, 1)) == 1

  override def decideKong(tile: Tile, cs: CurState): Boolean =
    maskedArgmax(out(cs, Some(tile.toTileValue)).kong, List(0, 1)) == 1

  override def decideChow(tile: Tile, positions: Set[ChowPosition],
                          cs: CurState): Option[ChowPosition] = {
    val logits = out(cs, Some(tile.toTileValue)).chow
    val valid = 0 :: positions.map(_.id + 1).toList
    maskedArgmax(logits, valid) match {
      case 0 => None
      case i => Some(ChowPosition(i - 1))
    }
  }

  override def decideSelfKong(kongSet: Set[Tile], cs: CurState): Option[Tile] = {
    val logits = out(cs, None).selfKong
    val valid = 0 :: kongSet.map(_.toTileValue + 1).toList
    maskedArgmax(logits, valid) match {
      case 0 => None
      case i => Some(Tile.fromValue(i - 1))
    }
  }

  override def name: String = "NNPlayer"
}
