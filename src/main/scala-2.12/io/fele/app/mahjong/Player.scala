package io.fele.app.mahjong

import io.fele.app.mahjong.ChowPosition.ChowPosition

/**
  * Created by felix.ling on 12/12/2016.
  */
object DrawResult extends Enumeration {
  type DrawResult = Value
  val DISCARD, NO_TILE, WIN = Value
}

import DrawResult._

abstract class Player(tiles: List[Tile]) {
  // TODO: change to private
  protected val hand = new Hand(tiles)

  def canWin(tile: Tile): Boolean = hand.canWin(tile)
  def canKong(tile: Tile): Boolean = hand.canKong(tile)
  def canPong(tile: Tile): Boolean = hand.canPong(tile)
  def canChow(tile: Tile): Set[ChowPosition] = hand.canChow(tile)

  object SelfKongDecision {
    def unapply(hand: Hand): Option[Tile] = {
      hand.selfKongSet() match {
        case kongSet if kongSet.nonEmpty => isSelfKong(kongSet)
        case _ => None
      }
    }
  }

  def kong(tile: Tile, drawer: RandomTileDrawer): (DrawResult, Option[Tile]) = {
    hand.kong(tile)
    draw(drawer)
  }

  def pong(tile: Tile): Tile = {
    hand.pong(tile)
    val discarded = discard()
    hand.discard(discarded)
    discarded
  }

  def chow(tile: Tile, position: ChowPosition): Tile = {
    hand.chow(tile, position)
    val discarded = discard()
    hand.discard(discarded)
    discarded
  }

  def draw(drawer: RandomTileDrawer): (DrawResult, Option[Tile]) = {
    drawer.pop match {
      case Some(drawnTile) => {
        // check self win
        if (hand.canWin(drawnTile) && isWin(drawnTile, true))
          (WIN, None)
        else {
          // check self kong
          hand.add(drawnTile)
          hand match {
            case SelfKongDecision(kongDecision) => kong(kongDecision, drawer)
            case _ => {
              val discarded = discard()
              hand.discard(discarded)
              (DISCARD, Some(discarded))
            }
          }
        }
      }
      case _ => (NO_TILE, None)
    }
  }

  def discard(): Tile = pickToDiscard()

  // abstract decision method
  def isWin(tile: Tile, isSelfWin: Boolean): Boolean
  def isSelfKong(selfKongTiles: Set[Tile]): Option[Tile]
  def isKong(tile: Tile): Boolean
  def isPong(tile: Tile): Boolean
  def isChow(tile: Tile, positions: Set[ChowPosition]): Option[ChowPosition]

  def pickToDiscard(): Tile

  override def toString = hand.toString
}

class DummyPlayer(tiles: List[Tile]) extends Player(tiles) {
  def isWin(tile: Tile, isSelfWin: Boolean): Boolean = true
  def isSelfKong(selfKongTiles: Set[Tile]): Option[Tile] = selfKongTiles.headOption
  def isKong(tile: Tile): Boolean = true
  def isPong(tile: Tile): Boolean = true
  def isChow(tile: Tile, positions: Set[ChowPosition]): Option[ChowPosition] = positions.headOption

  def pickToDiscard(): Tile = hand.tiles.head
}