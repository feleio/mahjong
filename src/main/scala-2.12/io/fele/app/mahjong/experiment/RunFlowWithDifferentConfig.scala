package io.fele.app.mahjong.experiment

import java.util.Random

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.Main.{logger, playerWinCount, results}
import io.fele.app.mahjong._
import io.fele.app.mahjong.player.{FirstFelix, ThreePointChicken}

import scala.util.Try

/**
  * Created by felix.ling on 09/07/2017.
  */
object RunFlowWithDifferentConfig extends App {
  val logger = Logger("RunFlowWithDifferentConfig")
  implicit val config: Config = new Config()

  val randomSeed = 10001
  val random = new Random(randomSeed)

  (1 to 13).foreach(sameSuitDecisionNum => {
    logger.info(s"sameSuitDecisionNum: $sameSuitDecisionNum")
    val total = 10000
    var count = 0
    val results = (0 until total).par.map(roundNum => (roundNum, random.nextInt(4)))
      .map{case (roundNum, initPlayer) => {
        count += 1
        if (count % 5000 == 0)
          logger.info(s"$count/$total")

        val drawer: TileDrawer = new RandomTileDrawer(Some(roundNum))

        val state = GameState(
          //new FirstFelix(0, drawer.popHand(), sameSuitDeadline) :: (1 to 3).map(new Chicken(_, drawer.popHand())).toList,
          //(0 to 3).map(new FirstFelix(_, drawer.popHand(), sameSuitDeadline)).toList,
          new FirstFelix(0, drawer.popHand(), sameSuitDecisionNum) :: (1 to 3).map(new ThreePointChicken(_, drawer.popHand())).toList,
          //(0 to 3).map(new ThreePointChicken(_, drawer.popHand())).toList,
          None,
          Nil,
          initPlayer,
          drawer)

        // implicit val gameLogger: GameLogger = new DebugGameLogger(state)
        implicit val gameLogger: GameLogger = new DummyGameLogger()
        val flow: Flow = new FlowImpl(state, Some(roundNum))

        flow.start()
      }}

    logger.info(s"Total games: $total")
    val winnerCount = results.groupBy(_.winnersInfo match{
      case Some(info) => info.winners.size
      case None => 0
    }).mapValues(_.size)
    logger.info(winnerCount.toList.sortBy(_._1).toString())

    val playerWinCount = results.flatMap(x => x.winnersInfo match {
      case Some(info) => info.winners.map(_.id).toList
      case None => List.empty[Int]
    }).groupBy[Int](identity).mapValues(_.size)

    val playerBalances = results.foldLeft((0 to 4).map(i => WinnerBalance(i, 0))){
      case (s, t) => t.winnersInfo match{
        case Some(info) => (s zip info.winnersBalance).map{case (lhs, rhs) => lhs + rhs}
        case _ => s
      }
    }

    (0 to 3).foreach(id => logger.info(s"Player $id wins: ${Try{playerWinCount(id)}.getOrElse(0)} money: ${playerBalances(id).amount}"))
  })

}
