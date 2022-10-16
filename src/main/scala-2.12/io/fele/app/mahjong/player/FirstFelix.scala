package io.fele.app.mahjong.player

import io.fele.app.mahjong.ChowPosition._
import io.fele.app.mahjong.TileType._
import io.fele.app.mahjong.{ChowPosition => _, _}

/**
  * Created by felix.ling on 15/01/2017.
  */

object TargetType extends Enumeration {
  type TargetType = Value
  val SameSuit, DiffSuitAllPong, DiffSuit = Value
}

import TargetType._

case class TargetDecision(`type`: TargetType, tiletype: TileType)

class FirstFelix(id: Int, tiles: List[Tile], sameSuitNum: Int, tileGroups: List[TileGroup] = List.empty[TileGroup])(implicit c: Config) extends Player(id, tiles, tileGroups)(c) {
  var target: Option[TargetDecision] = None
  var decisionDeadline: Int = 5
  val sameSuitDecisionNum = sameSuitNum

  override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean = {
    true
  }

  override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean ={
    true
  }

  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = target match {
    case Some(DiffSuitAllPong) => selfKongTiles.headOption
    case _ => None
  }

  override def decideKong(tile: Tile, curState: CurState): Boolean = {
    target.exists(_.`type` == DiffSuitAllPong) || tile.`type` == HONOR
  }

  override def decidePong(tile: Tile, curState: CurState): Boolean = {
    ((target.exists(_.`type` == DiffSuit) && (!isContinues(tile, hand.dynamicTiles) && tile.`type` != HONOR || tile.`type` == HONOR) )|| target.exists(_.`type` == DiffSuitAllPong)) &&
      (tile.`type` == HONOR || target.exists(_.`type` == tile.`type`))
  }

  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = {
    target match {
      case Some(t) if t.tiletype == tile.`type` => positions.headOption
      case _ => None
    }
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

  private def makeTargetDecision(tileTypeStat: Map[TileType, Int], discards: List[DiscardInfo]): Unit = {
    val tileTypeStat: Map[TileType, Int] = hand.dynamicTiles.groupBy(_.`type`).mapValues(_.size)

    val manySuit: Option[TileType] = tileTypeStat.keySet.filter(_ != HONOR).find(tileType => tileTypeStat(tileType) > sameSuitDecisionNum)
    val nonHonorType = tileTypeStat.keySet.filter(_ != HONOR)

    val maxSuit: TileType = nonHonorType.diff(previousPlayerMainSuit(discards)) match {
      case types if types.nonEmpty => types.maxBy(tileType => tileTypeStat(tileType))
      case _ => DOT
    }

    var maxPairSuit: Option[TileType] = None
    val isDecideDiffSuitAllPong = {
      val pairTileValues: Seq[Tile] = hand.dynamicTileStats.zipWithIndex
        .filter{case (tileCount, _) => tileCount >= 2}
        .map(v => Tile.fromValue(v._2))
      val pairTileStat: Map[TileType, Int] = pairTileValues.groupBy(_.`type`).mapValues(_.size)
      maxPairSuit = pairTileStat.keySet.filter(_ != HONOR) match {
        case ks if ks.nonEmpty => Some(ks.maxBy(k => pairTileStat(k)))
        case _ => None
      }
      maxPairSuit.isDefined &&
        (pairTileStat(maxPairSuit.get) >= 3 || (pairTileStat.contains(HONOR) && pairTileStat(HONOR) >= 1 && pairTileStat(maxPairSuit.get) >= 2))
    }

    if (manySuit.isDefined){
      target = Some(TargetDecision(SameSuit, manySuit.get))
    } else if(isDecideDiffSuitAllPong) {
      target = Some(TargetDecision(DiffSuitAllPong, maxPairSuit.get))
    } else {
      if (decisionDeadline > 0)
        decisionDeadline -= 1
      else{
        // can't decide target when deadline reached
        target = Some(TargetDecision(DiffSuit, maxSuit))
      }
    }
  }

  override def decideDiscard(curState: CurState): Tile = {
    if(target.isEmpty) {
      // decision not yet made
      val tileTypeStat: Map[TileType, Int] = hand.dynamicTiles.groupBy(_.`type`).mapValues(_.size)
      val nonHonorType = tileTypeStat.keySet.filter(_ != HONOR)
      val discardTile = if (nonHonorType.nonEmpty){
        val minSuit: TileType = nonHonorType.minBy(tileType => tileTypeStat(tileType))
        hand.dynamicTiles.find(_.`type` == minSuit).getOrElse(
          hand.dynamicTiles.find(_.`type` == HONOR).get
        )
      } else hand.dynamicTiles.find(_.`type` == HONOR).get

      makeTargetDecision(tileTypeStat, curState.discards)
      discardTile
    } else {
      // decision already made
      hand.dynamicTiles.find(_.`type` != target.get.`type`).getOrElse(
        hand.dynamicTiles.find(tile => tile.`type` == HONOR || hand.dynamicTileStats(tile.toTileValue) == 1).getOrElse(
          hand.dynamicTiles.find(tile => target.exists(_.`type` == SameSuit) && tile.`type` == HONOR).getOrElse(
            hand.dynamicTiles.find(tile => tile.`type` != HONOR && hand.dynamicTileStats(tile.toTileValue) < 2 && !isContinues(tile, hand.dynamicTiles)).getOrElse(
              hand.dynamicTiles.head
            )
          )
        )
      )
    }
  }

  override def name: String = "FirstFelix"

  def printDebug: Unit = {
    println(s"first felix target: $target")
  }
}
