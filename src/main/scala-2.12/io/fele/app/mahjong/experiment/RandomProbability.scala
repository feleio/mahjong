package io.fele.app.mahjong.experiment

import scala.util.Random

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.{PongGroup, _}
import io.fele.app.mahjong.player.{Chicken, Dummy}
import io.fele.app.mahjong.TileValue._
/**
  * Created by felix.ling on 21/01/2017.
  */
object RandomProbability extends App {
  val logger = Logger("RandomProbability")
  implicit val config: Config = new Config()

  // initial drawer
  val drawerTiles =  Seq[Tile](DOT_6, DOT_9, BAMBOO_3, HONOR_DRAGON_BLUE, BAMBOO_3, DOT_8, BAMBOO_2, CHARACTER_3, BAMBOO_8, BAMBOO_1, CHARACTER_1, CHARACTER_5, BAMBOO_7, CHARACTER_8, BAMBOO_6, CHARACTER_9, DOT_5, BAMBOO_5, BAMBOO_2, HONOR_WIND_EAST, BAMBOO_2, HONOR_WIND_EAST, HONOR_DRAGON_RED, BAMBOO_8, HONOR_DRAGON_BLUE, CHARACTER_4, BAMBOO_5, DOT_2, CHARACTER_7, DOT_3, DOT_2, BAMBOO_6, CHARACTER_3, CHARACTER_4, HONOR_WIND_SOUTH, DOT_8, CHARACTER_4, DOT_3, BAMBOO_7, BAMBOO_5, DOT_6, BAMBOO_9, HONOR_WIND_WEST, CHARACTER_5, BAMBOO_4, HONOR_WIND_WEST, DOT_6, CHARACTER_5, BAMBOO_1, CHARACTER_9, CHARACTER_2, HONOR_WIND_WEST, BAMBOO_9, CHARACTER_6, CHARACTER_7, DOT_4, CHARACTER_8, BAMBOO_4, HONOR_DRAGON_GREEN, BAMBOO_3, HONOR_DRAGON_RED, CHARACTER_4, HONOR_WIND_NORTH, DOT_1, DOT_1, BAMBOO_8, CHARACTER_9, CHARACTER_9, HONOR_WIND_EAST, HONOR_WIND_NORTH, HONOR_WIND_SOUTH, HONOR_DRAGON_BLUE, DOT_9, DOT_4, CHARACTER_3, BAMBOO_7, DOT_7, DOT_9, BAMBOO_5, BAMBOO_6, DOT_4, DOT_1, DOT_5, CHARACTER_7, DOT_2, HONOR_WIND_SOUTH, BAMBOO_8, BAMBOO_9, BAMBOO_4, DOT_9, CHARACTER_3, CHARACTER_1, BAMBOO_3, BAMBOO_9, DOT_5, BAMBOO_7, DOT_1, DOT_2, HONOR_WIND_EAST, DOT_3, CHARACTER_6, CHARACTER_1, BAMBOO_1, CHARACTER_8, HONOR_DRAGON_GREEN, CHARACTER_1, CHARACTER_2, BAMBOO_6, BAMBOO_1, HONOR_DRAGON_GREEN, DOT_3, HONOR_DRAGON_RED, DOT_7, HONOR_WIND_NORTH, HONOR_WIND_SOUTH, CHARACTER_7, HONOR_DRAGON_BLUE, DOT_8, HONOR_DRAGON_GREEN, DOT_7, CHARACTER_6, CHARACTER_2, CHARACTER_5, DOT_6, HONOR_WIND_WEST, HONOR_WIND_NORTH, BAMBOO_4, DOT_8, CHARACTER_6, DOT_4, BAMBOO_2, CHARACTER_2, DOT_5, CHARACTER_8, HONOR_DRAGON_RED, DOT_7)
  val curPos = 65

  // initial players
  val playerTiles = List(
    (List[Tile](DOT_8, DOT_9, CHARACTER_8, HONOR_DRAGON_BLUE), List(ChowGroup(Set[Tile](CHARACTER_1, CHARACTER_2, CHARACTER_3)), ChowGroup(Set[Tile](BAMBOO_7, BAMBOO_8, BAMBOO_9)), ChowGroup(Set[Tile](BAMBOO_1, BAMBOO_2, BAMBOO_3)))),
    (List[Tile](BAMBOO_2, BAMBOO_2, BAMBOO_3, BAMBOO_4, BAMBOO_5, BAMBOO_6, CHARACTER_8, CHARACTER_9, HONOR_WIND_EAST, HONOR_WIND_EAST, HONOR_WIND_NORTH, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE), List()),
    (List[Tile](DOT_1, DOT_2, DOT_2, DOT_3, DOT_3, CHARACTER_7, CHARACTER_7), List(PongGroup(CHARACTER_4), ChowGroup(Set[Tile](BAMBOO_6, BAMBOO_7, BAMBOO_8)))),
    (List[Tile](BAMBOO_4, BAMBOO_5, HONOR_WIND_WEST, HONOR_WIND_WEST, HONOR_WIND_WEST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List(PongGroup(CHARACTER_5), PongGroup(DOT_6)))
  )

  // inital discards
  val discards = List[(Int, Tile)]((2,HONOR_WIND_SOUTH), (1,CHARACTER_6), (0,CHARACTER_4), (3,CHARACTER_9), (2,CHARACTER_3), (0,BAMBOO_9), (2,BAMBOO_5), (0,BAMBOO_3), (3,DOT_4), (2,DOT_8), (1,DOT_5), (0,BAMBOO_1))
  val total = 1000000
  var count = 0
  (0 to 3).foreach(discardId => {
    val firstPlayerWinCount = (0 to total).par.count(_ => {
      count += 1
      if (count % 100000 == 0)
        logger.info(s"$count/$total")

      val (drawns, remainings) = drawerTiles.splitAt(curPos)
      val shuffledRemainings = drawns ++ new Random().shuffle(remainings)
      val players = (0 to 3).map(id => new Chicken(id, playerTiles(id)._1, playerTiles(id)._2)).toList
      players.head.overrideDiscardDecision(playerTiles.head._1(discardId))
      val state = GameState(
        players,
        Set.empty[Int],
        None,
        discards,
        3,
        new RandomTileDrawer(None, Some(shuffledRemainings), curPos))

      implicit val gameLogger: GameLogger = new DebugGameLogger(state)
      val flow: Flow = new FlowImpl(state)

      val discarded: Tile = HONOR_WIND_SOUTH
      val result: GameResult = flow.resume(Some(discarded))
      result.winners.contains(0)
    })
    logger.info(s"player 0 discarded ${playerTiles.head._1(discardId)} with winning probability $firstPlayerWinCount / $total = ${firstPlayerWinCount *100.0 / total}%")
  })
}
