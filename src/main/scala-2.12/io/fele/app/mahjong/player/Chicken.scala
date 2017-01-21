package io.fele.app.mahjong.player

import io.fele.app.mahjong.ChowPosition._
import io.fele.app.mahjong.{ChowPosition => _, _}

/**
  * Created by felix.ling on 15/01/2017.
  */
class Chicken(id: Int, tiles: List[Tile], tileGroups: List[TileGroup] = List.empty[TileGroup]) extends Player(id, tiles, tileGroups) {
  override def decideSelfWin(tile: Tile, curState: CurState): Boolean = true
  override def decideWin(tile: Tile, curState: CurState): Boolean = true
  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = selfKongTiles.headOption
  override def decideKong(tile: Tile, curState: CurState): Boolean = true
  override def decidePong(tile: Tile, curState: CurState): Boolean = true
  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = positions.headOption
  override def decideDiscard(curState: CurState): Tile = {
    val uselessTiles = hand.tiles.filter(tile => {
      hand.tileStats(tile.value.id) < 2 && !isContinues(tile, hand.tiles)
    })

    uselessTiles.size match {
      case 0 => hand.tiles.head
      case _ => uselessTiles.head
    }
  }
  override def name: String = "Chicken"

  private def isContinues(tile: Tile, tiles: List[Tile]) =
    tile.`type` != TileType.HONOR && (
      tile.num > 1 && tiles.contains(tile-1) || tile.num < 9 && tiles.contains(tile+1)
      )
}
