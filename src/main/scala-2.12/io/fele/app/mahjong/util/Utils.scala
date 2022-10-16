package io.fele.app.mahjong.util

import io.fele.app.mahjong.model.{HonorTile, HonorTileValue, NumberTile, Tile, TileType}
import io.fele.app.mahjong.util.Utils.TileValue.TileValue

object Utils {
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
  }

  implicit def tileValue2Tile(tileValue: TileValue): Tile = {
    int2Tile(tileValue.id)
  }

  def int2Tile(tileValue: Int): Tile = {
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
}
