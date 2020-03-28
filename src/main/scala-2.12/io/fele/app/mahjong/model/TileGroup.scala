package io.fele.app.mahjong.model

object ChowPosition extends Enumeration {
  type ChowPosition = Value
  val LEFT, MIDDLE, RIGHT = Value
}

sealed trait TileGroup
case class KongGroup(tile: Tile) extends TileGroup
case class PongGroup(tile: Tile) extends TileGroup
case class ChowGroup(tiles: Set[Tile]) extends TileGroup
