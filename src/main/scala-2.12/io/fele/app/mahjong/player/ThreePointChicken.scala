package io.fele.app.mahjong.player

import io.fele.app.mahjong.ChowPosition._
import io.fele.app.mahjong.TileType._
import io.fele.app.mahjong.{ChowPosition => _, _}

/**
  * Created by felix.ling on 15/01/2017.
  */

case class ChickenTargetDecision(tiletype: TileType)

class ThreePointChicken(id: Int, tiles: List[Tile], tileGroups: List[TileGroup] = List.empty[TileGroup])(implicit c: Config) extends Player(id, tiles, tileGroups)(c) {
  var target: Option[TileType] = None
  var decisionDeadline: Int = 5

  override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean = {
    true
  }

  override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean ={
    true
  }

  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = None

  override def decideKong(tile: Tile, curState: CurState): Boolean = {
    tile.`type` == HONOR
  }

  override def decidePong(tile: Tile, curState: CurState): Boolean = {
    tile.`type` == HONOR || (target.contains(tile.`type`) && !isContinues(tile, hand.dynamicTiles))
  }

  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = {
    target match {
      case Some(t) if t == tile.`type` => positions.headOption
      case _ => None
    }
  }

  private def makeTargetDecision(tileTypeStat: Map[TileType, Int]): Unit = {
    val tileTypeStat: Map[TileType, Int] = hand.dynamicTiles.groupBy(_.`type`).mapValues(_.size)

    val manySuit: Option[TileType] = tileTypeStat.keySet.filter(_ != HONOR).find(tileType => tileTypeStat(tileType) > 6)
    val maxSuit: TileType = tileTypeStat.keySet.filter(_ != HONOR).maxBy(tileType => tileTypeStat(tileType))

    if(manySuit.isDefined){
      target = Some(manySuit.get)
    } else {
      if(decisionDeadline > 0)
        decisionDeadline -= 1
      else{
        // can't decide target when deadline reached
        target = Some(maxSuit)
      }
    }
  }

  override def decideDiscard(curState: CurState): Tile = {
    if(target.isEmpty) {
      // decision not yet made
      val tileTypeStat: Map[TileType, Int] = hand.dynamicTiles.groupBy(_.`type`).mapValues(_.size)
      val minSuit: TileType = tileTypeStat.keySet.filter(_ != HONOR).minBy(tileType => tileTypeStat(tileType))
      val discardTile = hand.dynamicTiles.find(_.`type` == minSuit).getOrElse(
        hand.dynamicTiles.find(_.`type` == HONOR).get
      )
      makeTargetDecision(tileTypeStat)
      discardTile
    } else {
      // decision already made
      hand.dynamicTiles.find(_.`type` != target.get).getOrElse(
        hand.dynamicTiles.find(tile => tile.`type` == HONOR || hand.dynamicTileStats(tile.value.id) == 1).getOrElse(
          hand.dynamicTiles.find(tile => tile.`type` != HONOR && hand.dynamicTileStats(tile.value.id) < 2 && !isContinues(tile, hand.dynamicTiles)).getOrElse(
            hand.dynamicTiles.head
          )
        )
      )
    }
  }

  override def name: String = "ThreePointChicken"

  private def isContinues(tile: Tile, tiles: List[Tile]) =
    tile.`type` != TileType.HONOR && (
      tile.num > 1 && tiles.contains(tile-1) || tile.num < 9 && tiles.contains(tile+1)
      )
}
