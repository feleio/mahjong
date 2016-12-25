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

class Hand(var initTiles: List[Tile]) {
  if (initTiles.size % 3 != 1) throw new IllegalArgumentException("invalid number of tiles.")

  var tiles: List[Tile] = initTiles.sortBy(_.value.id)
  var fixedTileGroups: List[TileGroup] = Nil
  var tileStats = mutable.ArraySeq.fill[Int](34)(0)
  //var tileTypeStats = mutable.HashMap.empty[TileType, Int].withDefaultValue(0)
  tiles.foreach(x => {
    tileStats(x.value) += 1
    //tileTypeStats(x.`type`) += 1
  })

  private def isExist(tileValue: TileValue): Boolean = tileStats(tileValue) >= 1
  private def getExistingChowTiles(tile: Tile, position: ChowPosition): List[Tile] = position match {
    case LEFT => List[Tile](tile + 1, tile + 2)
    case MIDDLE => List[Tile](tile - 1, tile + 1)
    case RIGHT => List[Tile](tile - 2, tile - 1)
  }

  private def validate(tiles: List[Tile]): Boolean = tiles match {
    case t if t.isEmpty => true
    case t if t(0) == t(1) && t(1) == t(2) => validate(tiles.drop(3))
    case t if t(0).num <= 7 && t.contains(t(0)+1) && t.contains(t(0)+2) => validate(tiles diff List(t(0), t(0)+1, t(0)+2))
    case _ => false
  }

  def isToDiscard: Boolean = tiles.size % 3 == 2

  def canWin(tile: Tile): Boolean = {
    tileStats.zipWithIndex
      .filter{case (tileCount, i) => tileCount >= 2 || (i == tile.value.id && tileCount >= 1)}
      .map(_._2)
      .exists(tileId => validate((tiles + tile) diff List[Tile](TileValue(tileId), TileValue(tileId))))
  }

  def canKong(tile: Tile): Boolean = tileStats(tile.value) >= 3
  def selfKongSet(): Set[Tile] = tileStats.zipWithIndex.collect{case (count, tileIdx) if count > 4 => Tile(TileValue(tileIdx))}.toSet
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
    fixedTileGroups = ChowGroup(tile :: existTiles) :: fixedTileGroups
  }

  def add(tile: Tile): Unit = {
    tileStats(tile.value) += 1
    tiles = tiles + tile
  }
  def discard(tile: Tile): Unit = {
    tileStats(tile.value) -= 1
    tiles = tiles - tile
  }
}
