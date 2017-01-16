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

abstract class Player(val id: Int, tiles: List[Tile], tileGroups: List[TileGroup] = List.empty[TileGroup]) {
  // TODO: change to private
  protected val hand = new Hand(tiles, tileGroups)

  def privateInfo() = SelfInfo(hand.tiles, hand.fixedTileGroups)
  def publicInfo() = OtherInfo(hand.fixedTileGroups)

  def canWin(tile: Tile): Boolean = hand.canWin(tile)
  def canKong(tile: Tile): Boolean = hand.canKong(tile)
  def canPong(tile: Tile): Boolean = hand.canPong(tile)
  def canChow(tile: Tile): Set[ChowPosition] = hand.canChow(tile)

  private object SelfKongDecision {
    def unapply(hand: Hand): Option[Tile] = {
      hand.selfKongableTiles() match {
        case kongSet if kongSet.nonEmpty => verify(kongSet)(decideSelfKong(kongSet))
        case _ => None
      }
    }
    private def verify(kongSet: Set[Tile])(decision: Option[Tile]): Option[Tile] = decision match {
      case Some(tile) if decision.isDefined && !kongSet.contains(tile) =>
        throw new Exception(s"Player $id:invalid self kong decision, $tile not found in $kongSet")
      case _ => decision
    }
  }

  def kong(tile: Tile, drawer: TileDrawer)(implicit gameLogger: GameLogger): (DrawResult, Option[Tile]) = {
    hand.kong(tile)
    gameLogger.kong(id, tile)
    draw(drawer)
  }

  def selfKong(tile: Tile, drawer: TileDrawer)(implicit gameLogger: GameLogger): (DrawResult, Option[Tile]) = {
    hand.selfKong(tile)
    gameLogger.kong(id, tile)
    draw(drawer)
  }

  def pong(tile: Tile)(implicit gameLogger: GameLogger): Tile = {
    hand.pong(tile)
    gameLogger.pong(id, tile)
    val discarded = discard()
    hand.discard(discarded)
    discarded
  }

  def chow(tile: Tile, position: ChowPosition)(implicit gameLogger: GameLogger): Tile = {
    hand.chow(tile, position)
    gameLogger.chow(id, tile, position)
    val discarded = discard()
    hand.discard(discarded)
    discarded
  }

  def draw(drawer: TileDrawer)(implicit gameLogger: GameLogger): (DrawResult, Option[Tile]) = {
    drawer.pop() match {
      case Some(drawnTile) =>
        // check self win
        if (hand.canWin(drawnTile) && decideSelfWin(drawnTile))
          (WIN, Some(drawnTile))
        else {
          // check self kong
          hand.add(drawnTile)
          gameLogger.draw(id, drawnTile)
          hand match {
            case SelfKongDecision(kongDecision) => selfKong(kongDecision, drawer)
            case _ =>
              val discarded = discard()
              hand.discard(discarded)
              (DISCARD, Some(discarded))
          }
        }
      case None => (NO_TILE, None)
    }
  }

  def discard()(implicit gameLogger: GameLogger): Tile = {
    val discarded = decideDiscard()
    if (!hand.tiles.contains(discarded))
      throw new Exception(s"Player $id: discarded tile $discarded does not exist in ${hand.tiles}")
    discarded
  }

  override def toString = hand.toString

  // abstract decision method
  def decideSelfWin(tile: Tile): Boolean
  def decideWin(tile: Tile): Boolean
  def decideSelfKong(selfKongTiles: Set[Tile]): Option[Tile]
  def decideKong(tile: Tile): Boolean
  def decidePong(tile: Tile): Boolean
  def decideChow(tile: Tile, positions: Set[ChowPosition]): Option[ChowPosition]
  def decideDiscard(): Tile

  def name: String
}

class DummyPlayer(id: Int, tiles: List[Tile], tileGroups: List[TileGroup] = List.empty[TileGroup]) extends Player(id, tiles, tileGroups) {
  override def decideSelfWin(tile: Tile): Boolean = true
  override def decideWin(tile: Tile): Boolean = true
  override def decideSelfKong(selfKongTiles: Set[Tile]): Option[Tile] = selfKongTiles.headOption
  override def decideKong(tile: Tile): Boolean = true
  override def decidePong(tile: Tile): Boolean = true
  override def decideChow(tile: Tile, positions: Set[ChowPosition]): Option[ChowPosition] = positions.headOption
  override def decideDiscard(): Tile = hand.tiles.head

  override def name: String = this.getClass.getName
}