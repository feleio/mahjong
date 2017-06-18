package io.fele.app.mahjong

import io.fele.app.mahjong.Tile._
import io.fele.app.mahjong.TileType._
import io.fele.app.mahjong.TileValue._
import io.fele.app.mahjong.ChowPosition._

import scala.collection.mutable

/**
  * Created by felix.ling on 04/12/2016.
  */

trait TileGroup {
  def getCount: Int
  def toString: String
}

case class KongGroup(tile: Tile) extends TileGroup{
  override def getCount: Int = 4
  override def toString: String = s"Kong(${List.fill(getCount)(tile).mkString(", ")})"
}

case class PongGroup(tile: Tile) extends TileGroup{
  override def getCount: Int = 3
  override def toString: String = s"Pong(${List.fill(getCount)(tile).mkString(", ")})"
}

case class ChowGroup(tiles: Set[Tile]) extends TileGroup{
  override def getCount: Int = 3
  override def toString: String = s"Chow(${tiles.toList.sortBy(t => t.value.id).mkString(", ")})"
}

class Hand(ts: List[Tile], gs: List[TileGroup] = List.empty[TileGroup]) {
  if (ts.length + (gs.length * 3) != 13) throw new IllegalArgumentException("invalid number of tiles.")

  var dynamicTiles: List[Tile] = ts.sortBy(_.value.id)
  var fixedTileGroups: List[TileGroup] = gs
  var dynamicTileStats = mutable.ArraySeq.fill[Int](34)(0)

  dynamicTiles.foreach(x => {
    dynamicTileStats(x.value) += 1
  })

  // check if a tile exist in this hand. Tiles in TileGroups are not counted
  private def isExist(tileValue: TileValue): Boolean = dynamicTileStats(tileValue) >= 1

  // recursively check if the hand is winning with a sorted tile List
  private def validate(sortedTiles: List[Tile]): Boolean = sortedTiles match {
    case t if t.isEmpty => true
    case t if t.head == t(1) && t(1) == t(2) => validate(sortedTiles.drop(3))
    case t if t.head.`type` != HONOR && t.head.num <= 7 && t.contains(t.head+1) && t.contains(t.head+2) => validate(sortedTiles diff List(t.head, t.head+1, t.head+2))
    case _ => false
  }

  // check if this hand can win with the specific tile
  def canWin(tile: Tile): Boolean = {
    // filter the eyes tile value id, check there exists winning hands with these eyes
    dynamicTileStats.zipWithIndex
      .filter{case (tileCount, i) => tileCount >= 2 || (tileCount == 1 && i == tile.value.id )}
      .map(_._2)
      .exists(eyeTileId => validate((dynamicTiles + tile) diff List.fill[Tile](2)(TileValue(eyeTileId))))
  }

  // find which tile can be kong in current hand. It can be tile count == 4 OR pong group + count == 1
  def canSelfKong(): Set[Tile] = {
    val kongableTiles = dynamicTileStats.zipWithIndex.collect{case (count, tileId) if count >= 4 => Tile(TileValue(tileId))}.toSet
    val kongablePongGroupTiles = fixedTileGroups.collect{case PongGroup(tile) if dynamicTileStats(tile.value) >= 1 => tile}.toSet
    kongableTiles | kongablePongGroupTiles
  }

  def canKong(tile: Tile): Boolean = dynamicTileStats(tile.value) >= 3
  def canPong(tile: Tile): Boolean = dynamicTileStats(tile.value) >= 2
  def canChow(tile: Tile): Set[ChowPosition] = {
    if (tile.`type` != HONOR){
      Set(
        (tile.num > 2 && isExist((tile - 2).value) && isExist((tile - 1).value)) -> RIGHT,
        (tile.num > 1 && tile.num < 9 && isExist((tile -1).value) && isExist((tile + 1).value)) -> MIDDLE,
        (tile.num < 8 && isExist((tile + 1).value) && isExist((tile + 2).value)) -> LEFT
      ).collect{case x if x._1 => x._2}
    } else Set.empty[ChowPosition]
  }

  def kong(tile: Tile): Unit = {
    dynamicTileStats(tile.value) -= 3
    (1 to 3).foreach(_ => dynamicTiles = dynamicTiles - tile)
    fixedTileGroups = KongGroup(tile) :: fixedTileGroups
  }

  def selfKong(tile: Tile): Unit = {
    dynamicTileStats(tile.value) match {
      case 4 => {
        dynamicTileStats(tile.value) -= 4
        (1 to 4).foreach(_ => dynamicTiles = dynamicTiles - tile)
        fixedTileGroups = KongGroup(tile) :: fixedTileGroups
      }
      case 1 => {
        dynamicTileStats(tile.value) -= 1
        dynamicTiles = dynamicTiles - tile
        fixedTileGroups = fixedTileGroups diff List(PongGroup(tile))
        fixedTileGroups = KongGroup(tile) :: fixedTileGroups
      }
    }
  }

  def pong(tile: Tile): Unit = {
    dynamicTileStats(tile.value) -= 2
    (1 to 2).foreach(_ => dynamicTiles = dynamicTiles - tile)
    fixedTileGroups = PongGroup(tile) :: fixedTileGroups
  }

  def chow(tile: Tile, position: ChowPosition): Unit = {
    val existTiles: List[Tile] = position match {
      case LEFT => List[Tile](tile + 1, tile + 2)
      case MIDDLE => List[Tile](tile - 1, tile + 1)
      case RIGHT => List[Tile](tile - 2, tile - 1)
    }

    existTiles.foreach(x => {
      dynamicTileStats(x.value) -= 1
      dynamicTiles -= x
    })
    fixedTileGroups = ChowGroup(existTiles.toSet + tile) :: fixedTileGroups
  }

  def add(tile: Tile): Unit = {
    dynamicTileStats(tile.value) += 1
    dynamicTiles = dynamicTiles + tile
  }

  def discard(tile: Tile): Unit = {
    dynamicTileStats(tile.value) -= 1
    dynamicTiles = dynamicTiles - tile
  }

  override def toString: String = {
    s"fixed: ${fixedTileGroups.mkString(" ")}\ntiles: ${dynamicTiles.sortBy(t => t.value.id).mkString(" ")}\n"
  }
}
