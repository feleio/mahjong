package io.fele.app.mahjong.player

import io.fele.app.mahjong.ChowPosition._
import io.fele.app.mahjong.{ChowPosition => _, _}

/**
  * Created by felix.ling on 15/01/2017.
  */
class Chicken(id: Int, tiles: List[Tile], tileGroups: List[TileGroup] = List.empty[TileGroup])(implicit c: Config) extends Player(id, tiles, tileGroups)(c) {
  override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean = true
  override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean = true
  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = selfKongTiles.headOption
  override def decideKong(tile: Tile, curState: CurState): Boolean = true
  override def decidePong(tile: Tile, curState: CurState): Boolean = true
  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = positions.headOption
  override def decideDiscard(curState: CurState): Tile = {
    val uselessTiles = hand.dynamicTiles.filter(tile => {
      hand.dynamicTileStats(tile.toTileValue) < 2 && !isContinues(tile, hand.dynamicTiles)
    })

    uselessTiles.size match {
      case 0 => hand.dynamicTiles.head
      case _ => uselessTiles.head
    }
  }
  override def name: String = "Chicken"
}
