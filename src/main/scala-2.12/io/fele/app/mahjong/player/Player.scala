package io.fele.app.mahjong.player

import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong._

/**
  * Created by felix.ling on 12/12/2016.
  */
object DrawResult extends Enumeration {
  type DrawResult = Value
  val DISCARD, NO_TILE, WIN = Value
}

import io.fele.app.mahjong.player.DrawResult._

abstract class Player(val id: Int, tiles: List[Tile], tileGroups: List[TileGroup] = List.empty[TileGroup])(implicit val config: Config) {
  // TODO: change to private
  protected val hand = new Hand(tiles, tileGroups)

  var discardDecision: Option[Tile] = None

  def privateInfo = PrivateState(hand.dynamicTiles, hand.fixedTileGroups)
  def publicInfo = PublicState(hand.fixedTileGroups)

  def canWin(tile: Tile): CanWinResult = hand.canWin(tile)
  def canKong(tile: Tile): Boolean = hand.canKong(tile)
  def canPong(tile: Tile): Boolean = hand.canPong(tile)
  def canChow(tile: Tile): Set[ChowPosition] = hand.canChow(tile)

  private object SelfKongDecision {
    def unapply(hand: Hand)(implicit stateGenerator: CurStateGenerator): Option[Tile] = {
      hand.canSelfKong() match {
        case kongSet if kongSet.nonEmpty => verify(kongSet)(decideSelfKong(kongSet, stateGenerator.curState(id)))
        case _ => None
      }
    }
    private def verify(kongSet: Set[Tile])(decision: Option[Tile]): Option[Tile] = decision match {
      case Some(tile) if decision.isDefined && !kongSet.contains(tile) =>
        throw new Exception(s"Player $id:invalid self kong decision, $tile not found in $kongSet")
      case _ => decision
    }
  }

  def kong(tile: Tile, drawer: TileDrawer)
          (implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): (DrawResult, Option[Tile]) = {
    hand.kong(tile)
    gameLogger.kong(id, tile)
    draw(drawer)
  }

  def selfKong(tile: Tile, drawer: TileDrawer)
          (implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): (DrawResult, Option[Tile]) = {
    hand.selfKong(tile)
    gameLogger.kong(id, tile)
    draw(drawer)
  }

  def pong(tile: Tile)
          (implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): Tile = {
    hand.pong(tile)
    gameLogger.pong(id, tile)
    val discarded = discard()
    hand.discard(discarded)
    discarded
  }

  def chow(tile: Tile, position: ChowPosition)
          (implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): Tile = {
    hand.chow(tile, position)
    gameLogger.chow(id, tile, position)
    val discarded = discard()
    hand.discard(discarded)
    discarded
  }

  def draw(drawer: TileDrawer)
          (implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): (DrawResult, Option[Tile]) = {
    drawer.pop() match {
      case Some(drawnTile) =>
        // check self win
        if (hand.canWin(drawnTile).canWin && decideSelfWin(drawnTile, stateGenerator.curState(id)))
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

  def discard()(implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): Tile = {
    val discarded = discardDecision match {
      case Some(decision) => discardDecision = None; decision
      case None => decideDiscard(stateGenerator.curState(id))
    }
    if (!hand.dynamicTiles.contains(discarded))
      throw new Exception(s"Player $id: discarded tile $discarded does not exist in ${hand.dynamicTiles}")
    discarded
  }

  def overrideDiscardDecision(decision: Tile): Unit = discardDecision = Some(decision)

  override def toString = hand.toString

  // abstract decision method
  def decideSelfWin(tile: Tile, curState: CurState): Boolean
  def decideWin(tile: Tile, curState: CurState): Boolean
  def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile]
  def decideKong(tile: Tile, curState: CurState): Boolean
  def decidePong(tile: Tile, curState: CurState): Boolean
  def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition]
  def decideDiscard(curState: CurState): Tile

  def name: String
}
