package io.fele.app.mahjong.player

import io.fele.app.mahjong.ChowPosition._
import io.fele.app.mahjong.TileType._
import io.fele.app.mahjong.{ChowPosition => _, _}

object TargetType extends Enumeration {
  type TargetType = Value
  val SameSuit, DiffSuitAllPong = Value
}

import TargetType._

case class TargetDecision(`type`: TargetType, tiletype: TileType)

class FirstFelix(id: Int, tiles: List[Tile], sameSuitNum: Int, tileGroups: List[TileGroup] = List.empty[TileGroup])(implicit c: Config) extends Player(id, tiles, tileGroups)(c) {
  var target: Option[TargetDecision] = None
  val sameSuitDecisionNum = sameSuitNum

  override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean = true

  override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean = true

  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = target match {
    case Some(t) if t.`type` == DiffSuitAllPong => selfKongTiles.headOption
    case _ => None
  }

  override def decideKong(tile: Tile, curState: CurState): Boolean =
    target.exists(_.`type` == DiffSuitAllPong) || tile.`type` == HONOR

  override def decidePong(tile: Tile, curState: CurState): Boolean = target match {
    case Some(t) if t.`type` == DiffSuitAllPong => true
    case Some(t) if t.`type` == SameSuit        => tile.`type` == t.tiletype
    case _                                       => false
  }

  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = target match {
    case Some(t) if t.tiletype == tile.`type` => positions.headOption
    case _                                    => None
  }

  private def previousPlayerMainSuit(discards: List[DiscardInfo]): Set[TileType] = {
    val previousPlayerId = (id + 3) % 4
    val discardedTypes = discards.filter(x => x.playerId == previousPlayerId && x.tile.`type` != HONOR).map(_.tile.`type`)
    val discardedTypesCounts = discardedTypes.groupBy(identity).mapValues(_.size)
    discardedTypesCounts.keySet.size match {
      case 3 => Set(discardedTypesCounts.minBy(_._2)._1)
      case t if t < 3 => Set(DOT, BAMBOO, CHARACTER) diff discardedTypesCounts.keySet
      case _ => Set.empty
    }
  }

  private def makeTargetDecision(discards: List[DiscardInfo]): Unit = {
    val tileTypeStat: Map[TileType, Int] = hand.dynamicTiles.groupBy(_.`type`).mapValues(_.size)
    val nonHonorType = tileTypeStat.keySet.filter(_ != HONOR)

    val manySuit: Option[TileType] = nonHonorType.find(tileType => tileTypeStat(tileType) > sameSuitDecisionNum)
    if (manySuit.isDefined) {
      target = Some(TargetDecision(SameSuit, manySuit.get))
    } else {
      val pairTileValues = hand.dynamicTileStats.zipWithIndex
        .filter { case (tileCount, _) => tileCount >= 2 }
        .map(v => Tile.fromValue(v._2))
      val pairTileStat: Map[TileType, Int] = pairTileValues.groupBy(_.`type`).mapValues(_.size)
      val maxPairSuit: Option[TileType] = pairTileStat.keySet.filter(_ != HONOR) match {
        case ks if ks.nonEmpty => Some(ks.maxBy(k => pairTileStat(k)))
        case _                 => None
      }
      val isDecideDiffSuitAllPong = maxPairSuit.isDefined &&
        (pairTileStat(maxPairSuit.get) >= 3 ||
          (pairTileStat.contains(HONOR) && pairTileStat(HONOR) >= 1 && pairTileStat(maxPairSuit.get) >= 2))

      if (isDecideDiffSuitAllPong) {
        target = Some(TargetDecision(DiffSuitAllPong, maxPairSuit.get))
      } else {
        // Default: SameSuit targeting the suit with most tiles, avoiding the previous player's main suit
        val maxSuit: TileType = if (nonHonorType.nonEmpty) {
          nonHonorType.diff(previousPlayerMainSuit(discards)) match {
            case types if types.nonEmpty => types.maxBy(tileType => tileTypeStat(tileType))
            case _                       => nonHonorType.maxBy(tileType => tileTypeStat(tileType))
          }
        } else DOT
        target = Some(TargetDecision(SameSuit, maxSuit))
      }
    }
  }

  override def decideDiscard(curState: CurState): Tile = {
    if (target.isEmpty) makeTargetDecision(curState.discards)
    target.get.`type` match {
      case SameSuit =>
        // Discard tiles of the wrong suit first, then isolated same-suit tiles
        hand.dynamicTiles.find(_.`type` != target.get.tiletype).getOrElse(
          hand.dynamicTiles.find(tile => tile.`type` == HONOR || hand.dynamicTileStats(tile.toTileValue) == 1).getOrElse(
            hand.dynamicTiles.find(tile => tile.`type` != HONOR && hand.dynamicTileStats(tile.toTileValue) < 2 && !isContinues(tile, hand.dynamicTiles)).getOrElse(
              hand.dynamicTiles.head
            )
          )
        )
      case DiffSuitAllPong =>
        // Discard singletons (prefer non-honor singletons to preserve pong potential)
        hand.dynamicTiles.find(tile => hand.dynamicTileStats(tile.toTileValue) == 1 && tile.`type` != HONOR).getOrElse(
          hand.dynamicTiles.find(tile => hand.dynamicTileStats(tile.toTileValue) == 1).getOrElse(
            hand.dynamicTiles.head
          )
        )
    }
  }

  override def name: String = "FirstFelix"
}
