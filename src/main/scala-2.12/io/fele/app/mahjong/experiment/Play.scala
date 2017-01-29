package io.fele.app.mahjong.experiment

import java.util.Random

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong._
import io.fele.app.mahjong.player.{Chicken, Manual}

/**
  * Created by felix.ling on 28/01/2017.
  */
object Play extends App{
  implicit val config: Config = new Config()

  var seed = 0
  val drawer: TileDrawer = new RandomTileDrawer(Some(seed))
  val state = GameState(
    new Manual(0, drawer.popHand()) :: (1 to 3).map(new Chicken(_, drawer.popHand())).toList,
    None,
    Nil,
    new Random(seed).nextInt(4),
    drawer)

  implicit val gameLogger: GameLogger = new DebugGameLogger(state, Some(0))
  val flow: Flow = new FlowImpl(state, Some(seed))

  flow.start()
}
