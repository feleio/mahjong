package io.fele.app.mahjong.repo

import io.fele.app.mahjong.model.{Hand, Tile, TileGroup}

class HandRepo(initialHand: Hand) {
  private var tiles: List[Tile] = initialHand.tiles
  private var tileGroups: List[TileGroup] = initialHand.tileGroups

  def get: Hand = Hand(tiles, tileGroups)
  def setTiles(ts: List[Tile]): Unit = tiles = tiles
  def setTileGroups(gs: List[TileGroup]): Unit = tileGroups = gs
}
