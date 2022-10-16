package io.fele.app.mahjong.model

object TileType extends Enumeration {
  type TileType = Value
  val DOT, BAMBOO, CHARACTER, HONOR = Value
}

object HonorTileValue extends Enumeration {
  type HonorTileValue = Value
  val HW_E = Value(0)
  val HW_S = Value(1)
  val HW_W = Value(2)
  val HW_N = Value(3)
  val HD_R = Value(4)
  val HD_G = Value(5)
  val HD_B = Value(6)
}

import TileType._
import HonorTileValue._

sealed trait Tile {
  def `type`: TileType
}
sealed trait NumberTile extends Tile {
  def number: Int
}

final case class DotTile(number: Int) extends NumberTile {
  override def `type`: TileType = DOT
}
final case class BambooTile(number: Int) extends NumberTile {
  override def `type`: TileType = BAMBOO
}
final case class CharacterTile(number: Int) extends NumberTile {
  override def `type`: TileType = CHARACTER
}
final case class HonorTile(value: HonorTileValue) extends Tile {
  override def `type`: TileType = HONOR
}

object NumberTile {
  def apply(`type`: TileType, number: Int): NumberTile = `type` match {
    case TileType.DOT => DotTile(number)
    case TileType.BAMBOO => BambooTile(number)
    case TileType.CHARACTER => CharacterTile(number)
  }
}
