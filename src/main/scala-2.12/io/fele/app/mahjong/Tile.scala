package io.fele.app.mahjong

import scala.collection.mutable
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
  val DOT_1 = Value(1)
  val DOT_2 = Value(2)
  val DOT_3 = Value(3)
  val DOT_4 = Value(4)
  val DOT_5 = Value(5)
  val DOT_6 = Value(6)
  val DOT_7 = Value(7)
  val DOT_8 = Value(8)
  val DOT_9 = Value(9)

  // BAMBOO
  val BAMBOO_1 = Value(11)
  val BAMBOO_2 = Value(12)
  val BAMBOO_3 = Value(13)
  val BAMBOO_4 = Value(14)
  val BAMBOO_5 = Value(15)
  val BAMBOO_6 = Value(16)
  val BAMBOO_7 = Value(17)
  val BAMBOO_8 = Value(18)
  val BAMBOO_9 = Value(19)

  // CHARACTER
  val CHARACTER_1 = Value(21)
  val CHARACTER_2 = Value(22)
  val CHARACTER_3 = Value(23)
  val CHARACTER_4 = Value(24)
  val CHARACTER_5 = Value(25)
  val CHARACTER_6 = Value(26)
  val CHARACTER_7 = Value(27)
  val CHARACTER_8 = Value(28)
  val CHARACTER_9 = Value(29)

  // HONOR WIND
  val HONOR_WIND_EAST_ = Value(31)
  val HONOR_WIND_SOUTH = Value(32)
  val HONOR_WIND_WEST = Value(33)
  val HONOR_WIND_NORTH = Value(34)

  // HONOR DRAGON
  val HONOR_DRAGON_RED_ = Value(35)
  val HONOR_DRAGON_GREEN = Value(36)
  val HONOR_DRAGON_BLUE = Value(37)

  def shift(tileValue: TileValue, i: Int):TileValue = TileValue(tileValue.id + i)
}

import TileType._
import TileValue._

class Tile (val tileValue: TileValue){
  val `type` = tileValue.id / 10 match {
    case 0 => DOT
    case 1 => BAMBOO
    case 2 => CHARACTER
    case 3 => HONOR
  }

  val num = if (`type` == HONOR) 0 else tileValue.id % 10

  override def toString: String = `type`.toString + "-" + num
}

object Tile {
  def apply(tileValue: TileValue) = new Tile(tileValue)
}

class RandomTileDrawer(){
  // Seq of Tiles for drawing in one game, 4 of each kind of Tile.
  var shuffledTiles = Random.shuffle(RandomTileDrawer.tiles)
  var curPos = 0

  def pop(): Option[Tile] = {
    if (curPos < RandomTileDrawer.tilesNum) {
      curPos += 1
      Some(shuffledTiles(curPos - 1))
    } else None
  }
}

object RandomTileDrawer {
  val tiles: Seq[Tile] = TileValue.values.toSeq.sorted.flatMap(x => List.fill(4)(Tile(x)))
  val tilesNum = tiles.size
}
