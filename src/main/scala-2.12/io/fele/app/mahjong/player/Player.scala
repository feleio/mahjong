package io.fele.app.mahjong.player

import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong._

/**
  * Created by felix.ling on 12/12/2016.
  */
object DrawResultType extends Enumeration {
  type DrawResultType = Value
  val DISCARD, NO_TILE, WIN = Value
}

import io.fele.app.mahjong.player.DrawResultType._

case class DrawResult(result: DrawResultType, tile: Option[Tile] = None, winningScore: Option[Int] = None)

abstract class Player(val id: Int, tiles: List[Tile], tileGroups: List[TileGroup] = List.empty[TileGroup])(implicit val config: Config) {
  val hand = new Hand(tiles, tileGroups)

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

  def kong(tile: Tile, discardPlayerId: Int, drawer: TileDrawer)
          (implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): DrawResult = {
    hand.kong(tile)
    gameLogger.kong(KongEvent(id, discardPlayerId, tile))
    draw(drawer)
  }

  def selfKong(tile: Tile, drawer: TileDrawer)
          (implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): DrawResult = {
    hand.selfKong(tile)
    gameLogger.kong(KongEvent(id, id, tile))
    draw(drawer)
  }

  def pong(tile: Tile, discardPlayerId: Int)
          (implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): Tile = {
    hand.pong(tile)
    gameLogger.pong(PongEvent(id, discardPlayerId, tile))
    val discarded = discard()
    hand.discard(discarded)
    discarded
  }

  def chow(tile: Tile, discardPlayerId: Int, position: ChowPosition)
          (implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): Tile = {
    hand.chow(tile, position)
    gameLogger.chow(ChowEvent(id, discardPlayerId, tile, position))
    val discarded = discard()
    hand.discard(discarded)
    discarded
  }

  def draw(drawer: TileDrawer)
          (implicit stateGenerator: CurStateGenerator, gameLogger: GameLogger): DrawResult = {
    drawer.pop() match {
      case Some(drawnTile) =>
        // check self win
        val canWinResult = hand.canWin(drawnTile)
        if (canWinResult.canWin && decideSelfWin(drawnTile, canWinResult.score, stateGenerator.curState(id)))
          DrawResult(WIN, Some(drawnTile), Some(canWinResult.score))
        else {
          // check self kong
          hand.add(drawnTile)
          gameLogger.draw(DrawEvent(id, drawnTile))
          hand match {
            case SelfKongDecision(kongDecision) =>
              selfKong(kongDecision, drawer)
            case _ =>
              val discarded = discard()
              hand.discard(discarded)
              DrawResult(DISCARD, Some(discarded), None)
          }
        }
      case None => DrawResult(NO_TILE, None, None)
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
  def decideSelfWin(tile: Tile, score:Int, curState: CurState): Boolean
  def decideWin(tile: Tile, score:Int, curState: CurState): Boolean
  def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile]
  def decideKong(tile: Tile, curState: CurState): Boolean
  def decidePong(tile: Tile, curState: CurState): Boolean
  def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition]
  def decideDiscard(curState: CurState): Tile

  def name: String
}
