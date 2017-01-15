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
  override def toString: String = s"Kong(${List.fill(getCount)(tile).sortBy(t => t.value.id).mkString(", ")})"
}

case class PongGroup(tile: Tile) extends TileGroup{
  override def getCount: Int = 3
  override def toString: String = s"Pong(${List.fill(getCount)(tile).sortBy(t => t.value.id).mkString(", ")})"
}

case class ChowGroup(tiles: Set[Tile]) extends TileGroup{
  override def getCount: Int = 3
  override def toString: String = s"Chow(${tiles.toList.sortBy(t => t.value.id).mkString(", ")})"
}

class Hand(ts: List[Tile], gs: List[TileGroup] = List.empty[TileGroup]) {
  if (ts.length + (gs.length * 3) != 13) throw new IllegalArgumentException("invalid number of tiles.")

  var tiles: List[Tile] = ts.sortBy(_.value.id)
  var fixedTileGroups: List[TileGroup] = gs
  var tileStats = mutable.ArraySeq.fill[Int](34)(0)

  tiles.foreach(x => {
    tileStats(x.value) += 1
  })

  private def isExist(tileValue: TileValue): Boolean = tileStats(tileValue) >= 1
  private def getExistingChowTiles(tile: Tile, position: ChowPosition): List[Tile] = position match {
    case LEFT => List[Tile](tile + 1, tile + 2)
    case MIDDLE => List[Tile](tile - 1, tile + 1)
    case RIGHT => List[Tile](tile - 2, tile - 1)
  }

  private def validate(tiles: List[Tile]): Boolean = tiles match {
    case t if t.isEmpty => true
    case t if t.head == t(1) && t(1) == t(2) => validate(tiles.drop(3))
    case t if t.head.`type` != HONOR && t.head.num <= 7 && t.contains(t.head+1) && t.contains(t.head+2) => validate(tiles diff List(t.head, t.head+1, t.head+2))
    case _ => false
  }

  def isToDiscard: Boolean = tiles.size % 3 == 2

  def canWin(tile: Tile): Boolean = {
    tileStats.zipWithIndex
      .filter{case (tileCount, i) => tileCount >= 2 || (i == tile.value.id && tileCount >= 1)}
      .map(_._2)
      .exists(eyeTileId => validate((tiles + tile) diff List[Tile](TileValue(eyeTileId), TileValue(eyeTileId))))
  }

  def canKong(tile: Tile): Boolean = tileStats(tile.value) >= 3
  def selfKongableTiles(): Set[Tile] = {
    val kongableTiles = tileStats.zipWithIndex.collect{case (count, tileId) if count >= 4 => Tile(TileValue(tileId))}.toSet
    val kongablePongGroupTiles = fixedTileGroups.collect{case PongGroup(tile) if tileStats(tile.value) >= 1 => tile}.toSet
    kongableTiles | kongablePongGroupTiles
  }
  def canPong(tile: Tile): Boolean = tileStats(tile.value) >= 2
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
    tileStats(tile.value) -= 3
    (1 to 3).foreach(_ => tiles = tiles - tile)
    fixedTileGroups = KongGroup(tile) :: fixedTileGroups
  }

  def selfKong(tile: Tile): Unit = {
    tileStats(tile.value) match {
      case 4 => {
        tileStats(tile.value) -= 4
        (1 to 4).foreach(_ => tiles = tiles - tile)
        fixedTileGroups = KongGroup(tile) :: fixedTileGroups
      }
      case 1 => {
        tileStats(tile.value) -= 1
        tiles = tiles - tile
        fixedTileGroups = fixedTileGroups diff List(PongGroup(tile))
        fixedTileGroups = KongGroup(tile) :: fixedTileGroups
      }
    }
  }

  def pong(tile: Tile): Unit = {
    tileStats(tile.value) -= 2
    (1 to 2).foreach(_ => tiles = tiles - tile)
    fixedTileGroups = PongGroup(tile) :: fixedTileGroups
  }

  def chow(tile: Tile, position: ChowPosition): Unit = {
    val existTiles = getExistingChowTiles(tile, position)
    existTiles.foreach(x => {
      tileStats(x.value) -= 1
      tiles -= x
    })
    fixedTileGroups = ChowGroup(existTiles.toSet + tile) :: fixedTileGroups
  }

  def add(tile: Tile): Unit = {
    tileStats(tile.value) += 1
    tiles = tiles + tile
  }

  def discard(tile: Tile): Unit = {
    tileStats(tile.value) -= 1
    tiles = tiles - tile
  }

  override def toString: String = {
    s"fixed: ${fixedTileGroups.mkString(" ")}\ntiles: ${tiles.sortBy(t => t.value.id).mkString(" ")}\n"
  }
}
