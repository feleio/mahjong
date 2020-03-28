package io.fele.app.mahjong.model

import io.fele.app.mahjong.util.Utils.TileValue._
import io.fele.app.mahjong.util.Utils.tileValue2Tile

case class Hand(tiles: List[Tile], tileGroups: List[TileGroup] = List.empty)

object Hand {
  val thirteenValidateTiles: List[Tile] = List(D1, D9, B1, B9, C1, C9, HW_E, HW_S, HW_W, HW_N, HD_R, HD_G, HD_B)
}