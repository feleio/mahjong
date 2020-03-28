package io.fele.app.mahjong

import scala.language.implicitConversions
import scala.util.Random

/**
  * Created by felix.ling on 24/11/2016.
  */

object TileType extends Enumeration {
  type TileType = Value
  val DOT = Value(0)
  val BAMBOO = Value(1)
  val CHARACTER = Value(2)
  val HONOR = Value(3)
}

object HonorTileValue extends Enumeration {
  type HonorTileValue = Value
  // HONOR WIND and HONOR DRAGON
  val HW_E = Value(0)
  val HW_S = Value(1)
  val HW_W = Value(2)
  val HW_N = Value(3)
  val HD_R = Value(4)
  val HD_G = Value(5)
  val HD_B = Value(6)
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

  // TODO: throw if wrong input
  def shift(tileValue: TileValue, i: Int):TileValue = TileValue(tileValue.id + i)
}


object ChowPosition extends Enumeration {
  type ChowPosition = Value
  val LEFT, MIDDLE, RIGHT = Value
}

import TileType._
import TileValue._
import HonorTileValue._

sealed trait Tile {
  def `type`: TileType.Value
  def toString: String

  // TODO: try to delete this later if possible
  def toTileValue: Int
}

object Tile {
  def toTile(tileValue: Int): Tile = {
    if (tileValue >= 0 && tileValue <= 8)
      NumberTile(TileType.DOT, tileValue + 1)
    else if (tileValue >= 9 && tileValue <= 17)
      NumberTile(TileType.BAMBOO, tileValue % 9 + 1)
    else if (tileValue >= 18 && tileValue <= 26)
      NumberTile(TileType.CHARACTER, tileValue % 9 + 1)
    else if (tileValue >= 27 && tileValue <= 33)
      HonorTile(HonorTileValue(tileValue % 9))
    else
      throw new IllegalArgumentException("invalid tileValue")
  }

  def apply(tileValue: TileValue): Tile = Tile.toTile(tileValue.id)

  implicit def tileValue2Tile(value: TileValue): Tile = Tile.toTile(value.id)

  implicit def tileValue2Int(value: TileValue): Int = value.id

  implicit class RickList(tiles: List[Tile]) {
    def +(tile: Tile): List[Tile] ={
      (tile :: tiles).sortBy(_.toTileValue)
    }

    def -(tile: Tile): List[Tile] ={
      tiles diff List(tile)
    }
  }
}

sealed trait NumberTile extends Tile {
  def number: Int
  def +(i: Int): NumberTile = {
    if(number + i < 1 || number + i > 9)
      throw new IllegalArgumentException("invalid value to add / minus on a Tile")
    else
      NumberTile(`type`, number + i)
  }
  def -(i: Int): NumberTile = this + -i

  override def toString: String= s"${`type`.toString} $number"

  override def toTileValue: Int = `type` match {
    case TileType.DOT => number - 1
    case TileType.BAMBOO => 9 + number - 1
    case TileType.CHARACTER => (9 * 2) + number - 1
  }
}

object NumberTile {
  def apply(`type`: TileType, number: Int): NumberTile = `type` match {
    case TileType.DOT => DotTile(number)
    case TileType.BAMBOO => BambooTile(number)
    case TileType.CHARACTER => CharacterTile(number)
  }
}

final case class DotTile(number: Int) extends NumberTile{
  override val `type`: TileType = TileType.DOT

  override def toTileValue: Int = number - 1
}

final case class BambooTile(number: Int) extends NumberTile{
  override val `type`: TileType = TileType.BAMBOO

  override def toTileValue: Int = 9 + number - 1
}

final case class CharacterTile(number: Int) extends NumberTile{
  override val `type`: TileType = TileType.CHARACTER

  override def toTileValue: Int = (9 * 2) + number - 1
}

final case class HonorTile(value: HonorTileValue) extends Tile{
  override val `type`: TileType = TileType.HONOR

  override def toString: String = value.toString

  override def toTileValue: Int = (9 * 3) + value.id
}

case class DrawerState(shuffledTiles: Seq[Tile], curPos: Int)

trait TileDrawer {
  def pop(): Option[Tile]
  def popHand(): List[Tile]
  def remainingTiles: Seq[Tile]
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

  override def remainingTiles: Seq[Tile] = shuffledTiles.slice(curPos, shuffledTiles.size)
  override def drawerState: DrawerState = DrawerState(shuffledTiles, curPos)
}

object RandomTileDrawer {
  val tiles: Seq[Tile] = TileValue.values.toSeq.sorted.flatMap(x => List.fill(4)(Tile.toTile(x.id)))
  val tilesNum = tiles.size
}
