package io.fele.app.mahjong.player

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.{CurState, Tile, TileGroup}

/**
  * Created by felix.ling on 28/01/2017.
  */
class Manual(id: Int, tiles: List[Tile], tileGroups: List[TileGroup] = List.empty[TileGroup]) extends Player(id, tiles, tileGroups) {
  val logger = Logger(classOf[Manual])

  // abstract decision method
  override def decideSelfWin(tile: Tile, curState: CurState): Boolean = ???

  override def decideWin(tile: Tile, curState: CurState): Boolean = ???

  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = ???

  override def decideKong(tile: Tile, curState: CurState): Boolean = ???

  override def decidePong(tile: Tile, curState: CurState): Boolean = ???

  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = ???

  override def decideDiscard(curState: CurState): Tile = ???

  override def name: String = ???
}
