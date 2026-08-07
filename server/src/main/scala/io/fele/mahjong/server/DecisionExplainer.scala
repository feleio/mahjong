package io.fele.mahjong.server

import io.fele.app.mahjong._
import io.fele.app.mahjong.rl.Shanten
import io.fele.mahjong.server.Models.CoachHint

/** Human-legible reasons for a champion disagreement (issue #44).
  *
  * The net cannot explain itself, and asking it to would invent a rationale.
  * Instead we compute the two things that actually decide a HK mahjong discard
  * from the engine itself — how close the hand stays to a win (shanten and
  * ukeire, the tiles that would improve it) and how dangerous the tile is
  * (from the v4 danger heads already attached to the hint) — and say which of
  * them explains the difference. Anything we cannot ground in a number is left
  * unsaid.
  */
object DecisionExplainer {

  /** One bucket per lesson, mirroring the taxonomy rl/mine_overrides.py uses
    * for search-vs-net disagreements. */
  sealed abstract class Bucket(val key: String)
  object Bucket {
    case object Shape  extends Bucket("shape")   // keeps the hand closer to a win
    case object Tempo  extends Bucket("tempo")   // same shape, more tiles to improve on
    case object Safety extends Bucket("safety")  // avoids feeding an opponent
    case object Accept extends Bucket("accept")  // claim / decline a discarded tile
    case object Other  extends Bucket("other")
  }

  case class Explanation(bucket: String, text: String)

  private def counts(tiles: List[Tile]): Array[Int] = {
    val c = new Array[Int](34)
    tiles.foreach(t => c(t.toTileValue) += 1)
    c
  }

  private def groupTiles(groups: List[TileGroup]): List[Tile] = groups.flatMap {
    case PongGroup(t)  => List.fill(3)(t)
    case KongGroup(t)  => List.fill(4)(t)
    case ChowGroup(ts) => ts.toList
  }

  /** Tiles of each kind nobody can see yet, from this seat's point of view. */
  private def unseenFrom(cs: CurState): Array[Int] = {
    val seen = new Array[Int](34)
    cs.myInfo.tiles.foreach(t => seen(t.toTileValue) += 1)
    groupTiles(cs.myInfo.tileGroups).foreach(t => seen(t.toTileValue) += 1)
    cs.otherInfos.foreach(o => groupTiles(o.tileGroups).foreach(t => seen(t.toTileValue) += 1))
    cs.discards.foreach(d => seen(d.tile.toTileValue) += 1)
    Array.tabulate(34)(i => math.max(0, 4 - seen(i)))
  }

  private def shantenWord(n: Int): String = n match {
    case x if x <= 0 => "ready to win"
    case 1           => "1 away from ready"
    case x           => s"$x away from ready"
  }

  /** Wire codes like "D5"/"HW_E" mean nothing to a player reading a sentence;
    * the tile images carry them everywhere else in the UI, so spell them out
    * here. Mirrors tileLabel() in web/src/lib/tiles.ts. */
  def tileName(wire: String): String = wire match {
    case "HW_E" => "East wind"
    case "HW_S" => "South wind"
    case "HW_W" => "West wind"
    case "HW_N" => "North wind"
    case "HD_R" => "Red dragon"
    case "HD_G" => "Green dragon"
    case "HD_B" => "White dragon"
    case s if s.length == 2 && s(1).isDigit =>
      val n   = s(1)
      val one = n == '1'
      s(0) match {
        case 'D' => if (one) "1 dot" else s"$n dots"
        case 'B' => s"$n bamboo"
        case 'C' => if (one) "1 character" else s"$n characters"
        case _   => s
      }
    case s => s
  }

  /** Explain why the champion preferred `best` over `chosen`.
    * Returns None when nothing can be said with confidence. */
  def explain(kind: String, cs: CurState, chosen: String, best: String, hint: CoachHint): Option[Explanation] =
    if (chosen == best) None
    else kind match {
      case "discard" => explainDiscard(cs, chosen, best, hint)
      case _         => explainClaim(kind, chosen, best)
    }

  private def explainDiscard(cs: CurState, chosen: String, best: String, hint: CoachHint): Option[Explanation] =
    for {
      mine  <- Models.tileFromWire(chosen).toOption
      theirs <- Models.tileFromWire(best).toOption
      hand   = counts(cs.myInfo.tiles)
      if hand(mine.toTileValue) > 0 && hand(theirs.toTileValue) > 0
      fixed  = cs.myInfo.tileGroups.size
      (shAfter, ukAfter) = Shanten.discardFeatures(hand.clone(), fixed, unseenFrom(cs))
      expl <- {
        val (myShanten, theirShanten) = (shAfter(mine.toTileValue), shAfter(theirs.toTileValue))
        val (myUkeire,  theirUkeire)  = (ukAfter(mine.toTileValue), ukAfter(theirs.toTileValue))
        val danger = hint.dangerByTile
        val myDanger    = danger.flatMap(_.get(chosen))
        val theirDanger = danger.flatMap(_.get(best))

        if (theirShanten < myShanten)
          Some(Explanation(Bucket.Shape.key,
            s"Keeping ${tileName(best)} would have left you ${shantenWord(theirShanten)}; discarding ${tileName(chosen)} leaves you ${shantenWord(myShanten)}."))
        else if (theirShanten == myShanten && theirUkeire > myUkeire * 6 / 5 && theirUkeire - myUkeire >= 2)
          Some(Explanation(Bucket.Tempo.key,
            s"Both keep you ${shantenWord(myShanten)}, but the champion's discard leaves $theirUkeire tiles that improve the hand against your $myUkeire."))
        else (myDanger, theirDanger) match {
          case (Some(md), Some(td)) if md > td * 1.5 && md > 0.02 =>
            Some(Explanation(Bucket.Safety.key,
              f"Similar shape either way, but ${tileName(chosen)} is the more dangerous discard right now (${md * 100}%.0f%% vs ${td * 100}%.0f%% relative deal-in risk)."))
          case _ if theirShanten == myShanten && theirUkeire == myUkeire => None
          case _ =>
            Some(Explanation(Bucket.Other.key,
              s"The champion preferred ${tileName(best)}; shape and acceptance are close, so this is a judgement call it makes differently."))
        }
      }
    } yield expl

  private def explainClaim(kind: String, chosen: String, best: String): Option[Explanation] = {
    val what = kind match {
      case "pong"      => "pong"
      case "kong"      => "kong"
      case "chow"      => "chow"
      case "self_kong" => "self-kong"
      case "win"       => "win"
      case "self_win"  => "self-draw win"
      case other       => other
    }
    if (best == "pass")
      Some(Explanation(Bucket.Accept.key,
        s"The champion would have declined this $what — claiming it costs concealment and flexibility more than the shape it gains."))
    else if (chosen == "pass")
      Some(Explanation(Bucket.Accept.key,
        s"The champion would have taken this $what rather than passing."))
    else
      Some(Explanation(Bucket.Accept.key, s"The champion would have played $best here instead of $chosen."))
  }
}
