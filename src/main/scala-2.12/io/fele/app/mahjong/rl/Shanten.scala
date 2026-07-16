package io.fele.app.mahjong.rl

import io.fele.app.mahjong.{ChowGroup, KongGroup, PongGroup, TileGroup}

/**
 * Shanten — standard-hand shanten number and ukeire (tile acceptance) calculator.
 *
 * Operates on 34-length tile-count arrays (indices 0-8 = suit A 1-9,
 * 9-17 = suit B, 18-26 = suit C, 27-33 = honors), matching Tile.toTileValue.
 *
 * A winning hand is 4 sets + 1 pair, where already-melded groups (pong/kong/chow)
 * count toward the 4 sets. Shanten is the minimum number of tile exchanges to
 * reach tenpai + 1; i.e. tenpai = 0, win = -1. Special hands (thirteen orphans)
 * are ignored — they are irrelevant for feature purposes.
 *
 * Formula for a candidate decomposition with S dynamic sets, P partial sets
 * (two tiles needing one more) and an optional reserved pair:
 *
 *     shanten = 2·(4 − F − S) − P − pairFlag        (F = fixed melded groups)
 *
 * with P ≤ 4 − F − S. The minimum over all decompositions is the shanten number.
 */
object Shanten {

  private val MAX_SHANTEN = 8

  /** Shanten of a dynamic-tile configuration with F fixed groups.
   *  Works for both 13-tile (3·(4−F)+1) and 14-tile configurations. */
  def shanten(counts: Array[Int], fixedGroups: Int): Int = {
    val need = 4 - fixedGroups
    val c    = counts.clone()
    var best = MAX_SHANTEN

    def formula(sets: Int, partials: Int, hasPair: Boolean): Int = {
      val p = math.min(partials, math.max(0, need - sets))
      2 * math.max(0, need - sets) - p - (if (hasPair) 1 else 0)
    }

    def isNumber(i: Int): Boolean = i < 27
    def suitPos(i: Int): Int      = i % 9

    def rec(i: Int, sets: Int, partials: Int, hasPair: Boolean): Unit = {
      if (best <= -1) return  // already found a winning decomposition
      if (sets >= need && hasPair) { best = math.min(best, formula(sets, partials, hasPair)); return }
      if (i >= 34) { best = math.min(best, formula(sets, partials, hasPair)); return }
      if (c(i) == 0) { rec(i + 1, sets, partials, hasPair); return }

      // 1. Extract pong
      if (c(i) >= 3) {
        c(i) -= 3
        rec(i, sets + 1, partials, hasPair)
        c(i) += 3
      }
      // 2. Extract chow
      if (isNumber(i) && suitPos(i) <= 6 && c(i + 1) > 0 && c(i + 2) > 0) {
        c(i) -= 1; c(i + 1) -= 1; c(i + 2) -= 1
        rec(i, sets + 1, partials, hasPair)
        c(i) += 1; c(i + 1) += 1; c(i + 2) += 1
      }
      // 3. Reserve pair (eyes)
      if (c(i) >= 2 && !hasPair) {
        c(i) -= 2
        rec(i, sets, partials, hasPair = true)
        c(i) += 2
      }
      // Partials only useful while blocks are still needed
      if (sets + partials < need) {
        // 4. Proto-pong partial
        if (c(i) >= 2) {
          c(i) -= 2
          rec(i, sets, partials + 1, hasPair)
          c(i) += 2
        }
        // 5. Two-tile chow partials (i,i+1) and (i,i+2)
        if (isNumber(i) && suitPos(i) <= 7 && c(i + 1) > 0) {
          c(i) -= 1; c(i + 1) -= 1
          rec(i, sets, partials + 1, hasPair)
          c(i) += 1; c(i + 1) += 1
        }
        if (isNumber(i) && suitPos(i) <= 6 && c(i + 2) > 0) {
          c(i) -= 1; c(i + 2) -= 1
          rec(i, sets, partials + 1, hasPair)
          c(i) += 1; c(i + 2) += 1
        }
      }
      // 6. Leave remaining copies of tile i unused
      rec(i + 1, sets, partials, hasPair)
    }

    rec(0, 0, 0, hasPair = false)
    best
  }

  /** Tile ids that could possibly reduce shanten when drawn: any tile within
   *  suit-distance 2 of a held tile, or matching a held honor. */
  private def candidateImproves(counts: Array[Int]): IndexedSeq[Int] =
    (0 until 34).filter { t =>
      if (t >= 27) counts(t) > 0
      else {
        val lo = t - (t % 9)          // suit start
        val hi = lo + 8
        (math.max(lo, t - 2) to math.min(hi, t + 2)).exists(counts(_) > 0)
      }
    }

  /** Ukeire of a 13-tile configuration: per-tile unseen-weighted acceptance.
   *  result(t) = unseen(t) if drawing t reduces shanten, else 0. */
  def improveTiles(counts13: Array[Int], fixedGroups: Int,
                   unseen: Array[Int]): (Int, Array[Int]) = {
    val base = shanten(counts13, fixedGroups)
    val out  = Array.fill(34)(0)
    if (base <= -1) return (base, out)
    candidateImproves(counts13).foreach { t =>
      if (unseen(t) > 0 && counts13(t) < 4) {
        counts13(t) += 1
        if (shanten(counts13, fixedGroups) < base) out(t) = unseen(t)
        counts13(t) -= 1
      }
    }
    (base, out)
  }

  /**
   * Discard features for a 14-tile configuration:
   *   shantenAfter(t) = shanten of the 13-tile hand after discarding t (or 9 if t not in hand)
   *   ukeireAfter(t)  = sum of unseen counts of tiles that would then reduce shanten
   */
  def discardFeatures(counts14: Array[Int], fixedGroups: Int,
                      unseen: Array[Int]): (Array[Int], Array[Int]) = {
    val shAfter = Array.fill(34)(9)
    val ukAfter = Array.fill(34)(0)
    (0 until 34).foreach { t =>
      if (counts14(t) > 0) {
        counts14(t) -= 1
        val (sh, imp) = improveTiles(counts14, fixedGroups, unseen)
        shAfter(t) = sh
        ukAfter(t) = imp.sum
        counts14(t) += 1
      }
    }
    (shAfter, ukAfter)
  }
}

/**
 * RLFeatures — shanten/ukeire feature block for observation messages.
 * Shared by RLPlayer (live gym) and FFDataServer (imitation data) so both
 * emit identical feature semantics.
 */
object RLFeatures {

  private def groupPhysicalTiles(groups: List[TileGroup]): List[io.fele.app.mahjong.Tile] =
    groups.flatMap {
      case PongGroup(t)  => List.fill(3)(t)
      case KongGroup(t)  => List.fill(4)(t)
      case ChowGroup(ts) => ts.toList
    }

  /**
   * Compute the feature entries to merge into a state map.
   *
   * handCounts    : 34 counts of our dynamic tiles (13-3F or 14-3F tiles)
   * myGroups      : our melded groups
   * oppGroupsList : melded groups of the 3 opponents
   * discardCounts : 34 counts of all formal discards
   *
   * Emits:
   *   cur_shanten   : shanten of the current configuration (min over discards
   *                   for 14-tile states); win = -1, tenpai = 0, junk ≤ 8
   *   shanten_after : 34 ints — shanten after discarding tile t (9 = not in hand);
   *                   all 9s for 13-tile states
   *   ukeire_after  : 34 ints — unseen-weighted acceptance after discarding t
   *   improve_tiles : 34 ints — for 13-tile states, unseen count of t if drawing
   *                   t reduces shanten, else 0; zeros for 14-tile states
   */
  def featureMap(handCounts: Array[Int],
                 myGroups: List[TileGroup],
                 oppGroupsList: List[List[TileGroup]],
                 discardCounts: Array[Int]): Map[String, Any] = {
    val fixed = myGroups.size

    val seen = Array.fill(34)(0)
    (0 until 34).foreach(i => seen(i) += handCounts(i) + discardCounts(i))
    groupPhysicalTiles(myGroups).foreach(t => seen(t.toTileValue) += 1)
    oppGroupsList.foreach(gs =>
      groupPhysicalTiles(gs).foreach(t => seen(t.toTileValue) += 1))
    val unseen = (0 until 34).map(i => math.max(0, 4 - seen(i))).toArray

    val dyn = handCounts.sum
    if (dyn % 3 == 2) {
      // Post-draw configuration: per-discard features
      val (shAfter, ukAfter) = Shanten.discardFeatures(handCounts.clone(), fixed, unseen)
      Map(
        "cur_shanten"   -> shAfter.min,
        "shanten_after" -> shAfter.toList,
        "ukeire_after"  -> ukAfter.toList,
        "improve_tiles" -> List.fill(34)(0)
      )
    } else {
      // Waiting configuration: acceptance features
      val (sh, imp) = Shanten.improveTiles(handCounts.clone(), fixed, unseen)
      Map(
        "cur_shanten"   -> sh,
        "shanten_after" -> List.fill(34)(9),
        "ukeire_after"  -> List.fill(34)(0),
        "improve_tiles" -> imp.toList
      )
    }
  }
}
