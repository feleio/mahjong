package io.fele.app.mahjong

import scala.language.implicitConversions
import scala.util.Random

/**
  * Created by felix.ling on 24/11/2016.
  */

object TileType extends Enumeration {
  type TileType = Value
  val DOT, BAMBOO, CHARACTER, HONOR = Value
}

object TileValue extends Enumeration {
  type TileValue = Value

  // DOT
  val D1 = Value(0)
  val D2 = Value(1)
  val D3 = Value(2)
  val D4 = Value(3)
  val D5 = Value(4)
  val D6 = Value(5)
  val D7 = Value(6)
  val D8 = Value(7)
  val D9 = Value(8)

  // BAMBOO
  val B1 = Value(9)
  val B2 = Value(10)
  val B3 = Value(11)
  val B4 = Value(12)
  val B5 = Value(13)
  val B6 = Value(14)
  val B7 = Value(15)
  val B8 = Value(16)
  val B9 = Value(17)

  // CHARACTER
  val C1 = Value(18)
  val C2 = Value(19)
  val C3 = Value(20)
  val C4 = Value(21)
  val C5 = Value(22)
  val C6 = Value(23)
  val C7 = Value(24)
  val C8 = Value(25)
  val C9 = Value(26)

  // HONOR WIND
  val HW_E = Value(27)
  val HW_S = Value(28)
  val HW_W = Value(29)
  val HW_N = Value(30)

  // HONOR DRAGON
  val HD_R = Value(31)
  val HD_G = Value(32)
  val HD_B = Value(33)

  def shift(tileValue: TileValue, i: Int):TileValue = TileValue(tileValue.id + i)
}


object ChowPosition extends Enumeration {
  type ChowPosition = Value
  val LEFT, MIDDLE, RIGHT = Value
}

import TileType._
import TileValue._

case class Tile (value: TileValue){
  val `type` = value.id / 9 match {
    case 0 => DOT
    case 1 => BAMBOO
    case 2 => CHARACTER
    case 3 => HONOR
  }

  val num = if (`type` == HONOR) 0 else value.id % 9 + 1

  def +(i: Int): Tile = this match {
    case t if t.`type` != HONOR && this.num + i >= 1 && this.num + i <= 9 => Tile(TileValue(this.value.id + i))
  }
  def -(i: Int): Tile = this + -i

  override def toString: String = value.toString
}

object Tile {
  implicit def tileValue2Tile(value: TileValue): Tile = Tile(value)

  implicit def tileValue2Int(value: TileValue): Int = value.id

  implicit class RickList(tiles: List[Tile]) {
    def +(tile: Tile): List[Tile] ={
      (tile :: tiles).sortBy(_.value.id)
    }

    def -(tile: Tile): List[Tile] ={
      tiles diff List(tile)
    }
  }
}

case class DrawerState(shuffledTiles: Seq[Tile], curPos: Int)

trait TileDrawer {
  def pop(): Option[Tile]
  def popHand(): List[Tile]
  def remainingTileNum: Int
  def drawerState: DrawerState
}

class RandomTileDrawer(
  seed: Option[Long] = None,
  tiles: Option[Seq[Tile]] = None,
  var curPos: Int = 0) extends TileDrawer {
  // Seq of Tiles for drawing in one game, 4 of each kind of Tile.
  var shuffledTiles = tiles match {
    case Some(ts) => ts
    case None => ( if (seed.isDefined) new Random(seed.get) else new Random()).shuffle(RandomTileDrawer.tiles)
  }

  override def pop(): Option[Tile] = {
    if (curPos < RandomTileDrawer.tilesNum) {
      curPos += 1
      Some(shuffledTiles(curPos - 1))
    } else None
  }

  override def popHand(): List[Tile] = {
    val drawnHandTile = shuffledTiles.slice(curPos, curPos + 13).toList
    curPos += 13
    drawnHandTile
  }

  override def remainingTileNum: Int = shuffledTiles.size - curPos
  override def drawerState: DrawerState = DrawerState(shuffledTiles, curPos)
}

object RandomTileDrawer {
  val tiles: Seq[Tile] = TileValue.values.toSeq.sorted.flatMap(x => List.fill(4)(Tile(x)))
  val tilesNum = tiles.size
}
