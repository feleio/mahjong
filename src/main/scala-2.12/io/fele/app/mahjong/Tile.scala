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
  val DOT_1 = Value(0)
  val DOT_2 = Value(1)
  val DOT_3 = Value(2)
  val DOT_4 = Value(3)
  val DOT_5 = Value(4)
  val DOT_6 = Value(5)
  val DOT_7 = Value(6)
  val DOT_8 = Value(7)
  val DOT_9 = Value(8)

  // BAMBOO
  val BAMBOO_1 = Value(9)
  val BAMBOO_2 = Value(10)
  val BAMBOO_3 = Value(11)
  val BAMBOO_4 = Value(12)
  val BAMBOO_5 = Value(13)
  val BAMBOO_6 = Value(14)
  val BAMBOO_7 = Value(15)
  val BAMBOO_8 = Value(16)
  val BAMBOO_9 = Value(17)

  // CHARACTER
  val CHARACTER_1 = Value(18)
  val CHARACTER_2 = Value(19)
  val CHARACTER_3 = Value(20)
  val CHARACTER_4 = Value(21)
  val CHARACTER_5 = Value(22)
  val CHARACTER_6 = Value(23)
  val CHARACTER_7 = Value(24)
  val CHARACTER_8 = Value(25)
  val CHARACTER_9 = Value(26)

  // HONOR WIND
  val HONOR_WIND_EAST = Value(27)
  val HONOR_WIND_SOUTH = Value(28)
  val HONOR_WIND_WEST = Value(29)
  val HONOR_WIND_NORTH = Value(30)

  // HONOR DRAGON
  val HONOR_DRAGON_RED = Value(31)
  val HONOR_DRAGON_GREEN = Value(32)
  val HONOR_DRAGON_BLUE = Value(33)

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

trait TileDrawer {
  def pop(): Option[Tile]
  def popHand(): List[Tile]
}

class RandomTileDrawer(seed: Option[Long] = None) extends TileDrawer {
  // Seq of Tiles for drawing in one game, 4 of each kind of Tile.
  var shuffledTiles = ( if (seed.isDefined) new Random(seed.get) else new Random()).shuffle(RandomTileDrawer.tiles)
  var curPos = 0

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
}

case class PrivateState(tiles: List[Tile], tileGroups: List[TileGroup])
case class PublicState(tileGroups: List[TileGroup])
case class CurState(myInfo: PrivateState,
                    otherInfos: List[PublicState],
                    discards: List[Tile],
                    remainTileNum: Int)

object RandomTileDrawer {
  val tiles: Seq[Tile] = TileValue.values.toSeq.sorted.flatMap(x => List.fill(4)(Tile(x)))
  val tilesNum = tiles.size
}
