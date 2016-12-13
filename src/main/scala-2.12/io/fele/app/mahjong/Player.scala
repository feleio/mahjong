package io.fele.app.mahjong

import io.fele.app.mahjong.ChowPosition.ChowPosition

/**
  * Created by felix.ling on 12/12/2016.
  */
abstract class Player(tiles: List[Tile]) {
  val hand = new Hand(tiles)

  def isWin(tile: Tile): Boolean
  def isKong(tile: Tile): Boolean
  def isPong(tile: Tile): Boolean
  def isChow(tile: Tile, positions: List[ChowPosition]): (Boolean, ChowPosition)

  def isWinWhenDraw(tile: Tile): Boolean
  def isKongWhenDraw(tile: Tile): Boolean

  def pickToDiscard(): Tile

//  def doKong(in: Tile): Unit = hand.kong(in)
//  def doPong(in: Tile): Unit = hand.pong(in)
//  def doChow(in: Tile, position: ChowPosition): Unit = hand.chow(in, position)
//  def doDraw(in: Tile): Unit = hand.draw(in)
//
//  def discard(out: Tile) = hand.discard(out)
}

class DummyPlayer(tiles: List[Tile]) extends Player(tiles) {
  def isWin(tile: Tile): Boolean = true
  def isKong(tile: Tile): Boolean = true
  def isPong(tile: Tile): Boolean = true
  def isChow(tile: Tile, positions: List[ChowPosition]): (Boolean, ChowPosition) = (true, positions.head)

  def isWinWhenDraw(tile: Tile): Boolean = true
  def isKongWhenDraw(tile: Tile): Boolean = true

  def pickToDiscard(): Tile = hand.tiles.head
}