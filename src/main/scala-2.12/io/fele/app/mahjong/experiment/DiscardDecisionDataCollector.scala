package io.fele.app.mahjong.experiment

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.Tile._
import io.fele.app.mahjong._

import scala.collection.mutable

case class DiscardRecord(ts: List[Tile], discard: Tile)

class DiscardDecisionDataCollector(val gameState: GameState) extends GameLogger {
  val logger = Logger("DataCollector")

  var discardRecords = mutable.MutableList.empty[DiscardRecord]

  override def start(): Unit = {}

  override def resume(): Unit = {}

  override def discard(discardEvent: DiscardEvent): Unit = {
    discardRecords += DiscardRecord(
      gameState.curPlayer.hand.dynamicTiles + discardEvent.tile,
      discardEvent.tile
    )
  }

  override def kong(kongEvent: KongEvent): Unit = {}

  override def pong(pongEvent: PongEvent): Unit = {}

  override def chow(chowEvent: ChowEvent): Unit = {}

  override def end(endEvent: EndEvent): Unit = {}

  override def draw(drawEvent: DrawEvent): Unit = {}

}
