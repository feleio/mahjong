package io.fele.app.mahjong

import io.fele.app.mahjong.TileType._
import io.fele.app.mahjong.TileValue._

import scala.collection.mutable

/**
  * Created by felix.ling on 04/12/2016.
  */

trait TileGroup {
  def getCount: Int
  def getTiles: List[Tile]
}

case class KongGroup(tile: Tile) extends TileGroup{
  override def getCount: Int = 4
  override def getTiles: List[Tile] = List.fill(getCount)(tile)
}

case class PongGroup(tile: Tile) extends TileGroup{
  override def getCount: Int = 3
  override def getTiles: List[Tile] = List.fill(getCount)(tile)
}

case class ChowGroup(tiles: List[Tile]) extends TileGroup{
  override def getCount: Int = 3
  override def getTiles: List[Tile] = tiles
}

class Hand(var tiles: mutable.ListBuffer[Tile]) {
  if (tiles.size != 13)
    throw new IllegalArgumentException("the number must be non-negative.")

  var fixedTileGroups = mutable.ListBuffer.empty[TileGroup]

  var tileStats = mutable.HashMap.empty[TileValue, Int].withDefaultValue(0)
  //var tileTypeStats = mutable.HashMap.empty[TileType, Int].withDefaultValue(0)
  tiles.foreach(x => {
    tileStats(x.tileValue) += 1
    //tileTypeStats(x.`type`) += 1
  })

  private def isExist(tileValue: TileValue): Boolean = tileStats(tileValue) >= 1
  def isToDiscard: Boolean = tiles.size % 3 == 1

  def canWin(tile: Tile): Boolean = false
  def canKong(tile: Tile): Boolean = tileStats(tile.tileValue) >= 3
  def canPong(tile: Tile): Boolean = tileStats(tile.tileValue) >= 2
  def canChow(tile: Tile): Boolean = {
    val value = tile.tileValue
    tile.`type` != HONOR && (
      value.id % 10 > 2 && isExist(shift(value ,-2)) && isExist(shift(value ,-1))
        || value.id % 10 > 1 && value.id % 10 < 9 && isExist(shift(value ,-1)) && isExist(shift(value ,+1))
        || value.id % 10 < 8 && isExist(shift(value ,+1)) && isExist(shift(value ,+2))
      )
  }

  def kong(tile: Tile): Unit = {
    tileStats(tile.tileValue) -= 3
    1 to 3 foreach(tiles -= tile)
    fixedTileGroups += KongGroup(tile)
  }
  def Pong(tile: Tile): Unit = {
    tileStats(tile.tileValue) -= 2
    1 to 2 foreach(tiles -= tile)
    fixedTileGroups += PongGroup(tile)
  }
  def chow(tile: Tile, existTiles: List[Tile]): Unit = {
    existTiles.foreach{x => {
      tileStats(x.tileValue) -= 1
      tiles -= x
    }}
    fixedTileGroups += ChowGroup(tile :: existTiles)
  }

  def draw(tile: Tile): Unit = {
    tileStats(tile.tileValue) += 1
    tiles += tile
  }
  def discard(tile: Tile): Unit = {
    tileStats(tile.tileValue) -= 1
    tiles -= tile
  }
}
