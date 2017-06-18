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
  val drawerTiles =  Seq[Tile](D6, D9, B3, HD_B, B3, D8, B2, C3, B8, B1, C1, C5, B7, C8, B6, C9, D5, B5, B2, HW_E, B2, HW_E, HD_R, B8, HD_B, C4, B5, D2, C7, D3, D2, B6, C3, C4, HW_S, D8, C4, D3, B7, B5, D6, B9, HW_W, C5, B4, HW_W, D6, C5, B1, C9, C2, HW_W, B9, C6, C7, D4, C8, B4, HD_G, B3, HD_R, C4, HW_N, D1, D1, B8, C9, C9, HW_E, HW_N, HW_S, HD_B, D9, D4, C3, B7, D7, D9, B5, B6, D4, D1, D5, C7, D2, HW_S, B8, B9, B4, D9, C3, C1, B3, B9, D5, B7, D1, D2, HW_E, D3, C6, C1, B1, C8, HD_G, C1, C2, B6, B1, HD_G, D3, HD_R, D7, HW_N, HW_S, C7, HD_B, D8, HD_G, D7, C6, C2, C5, D6, HW_W, HW_N, B4, D8, C6, D4, B2, C2, D5, C8, HD_R, D7)
  val curPos = 65

  // initial players
  val playerTiles = List(
    (List[Tile](D8, D9, C8, HD_B), List(ChowGroup(Set[Tile](C1, C2, C3)), ChowGroup(Set[Tile](B7, B8, B9)), ChowGroup(Set[Tile](B1, B2, B3)))),
    (List[Tile](B2, B2, B3, B4, B5, B6, C8, C9, HW_E, HW_E, HW_N, HD_R, HD_B), List()),
    (List[Tile](D1, D2, D2, D3, D3, C7, C7), List(PongGroup(C4), ChowGroup(Set[Tile](B6, B7, B8)))),
    (List[Tile](B4, B5, HW_W, HW_W, HW_W, HD_R, HD_G), List(PongGroup(C5), PongGroup(D6)))
  )

  // inital discards
  val discards = List[(Int, Tile)](
    (2,HW_S), (1,C6), (0,C4), (3,C9), (2,C3), (0,B9), (2,B5), (0,B3), (3,D4), (2,D8), (1,D5), (0,B1)
  ).map(x => DiscardInfo(x._1, x._2))

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
        None,
        discards,
        3,
        new RandomTileDrawer(None, Some(shuffledRemainings), curPos))

      implicit val gameLogger: GameLogger = new DummyGameLogger()
      val flow: Flow = new FlowImpl(state)

      val discarded: Tile = HW_S
      val result: GameResult = flow.resume(Some(discarded))
      result.winnersInfo.isDefined && result.winnersInfo.get.winners.contains(0)
    })
    logger.info(s"player 0 discarded ${playerTiles.head._1(discardId)} with winning probability $firstPlayerWinCount / $total = ${firstPlayerWinCount *100.0 / total}%")
  })
}
