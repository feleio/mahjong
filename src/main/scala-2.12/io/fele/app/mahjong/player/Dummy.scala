package io.fele.app.mahjong.player

import io.fele.app.mahjong.ChowPosition._
import io.fele.app.mahjong.{Config, CurState, Tile, TileGroup}

/**
  * Created by felix.ling on 21/01/2017.
  */
class Dummy(id: Int, tiles: List[Tile], tileGroups: List[TileGroup] = List.empty[TileGroup])(implicit c: Config) extends Player(id, tiles, tileGroups)(c) {
  override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean = true
  override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean = true
  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = selfKongTiles.headOption
  override def decideKong(tile: Tile, curState: CurState): Boolean = true
  override def decidePong(tile: Tile, curState: CurState): Boolean = true
  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = positions.headOption
  override def decideDiscard(curState: CurState): Tile = hand.dynamicTiles.head

  override def name: String = this.getClass.getName
}