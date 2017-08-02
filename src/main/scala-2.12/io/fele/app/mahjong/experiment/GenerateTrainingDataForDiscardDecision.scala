package io.fele.app.mahjong.experiment

import java.io.{File, PrintWriter}
import java.util.Random

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong._
import io.fele.app.mahjong.player._
import org.json4s.native.Serialization.write

object GenerateTrainingDataForDiscardDecision extends App {
  implicit val formats = JsonSerializer.serializers
  implicit val config: Config = new Config()
  val logger = Logger("GenerateTrainingDataForDiscardDecision")

  val total = 40000
  var count = 0

  val randomSeed = 10001
  val random = new Random(randomSeed)

  private val trainingDataFile = new File("train_discard_decition_v1.json")
  private val printWriter: PrintWriter = new PrintWriter(trainingDataFile)

  val results: List[DiscardRecord] = (0 until total)
    .par
    .map(roundNum => (roundNum, random.nextInt(4)))
    .flatMap{case (roundNum, initPlayer) => {
      count += 1
      if (count % 10000 == 0)
        logger.info(s"$count/$total")

      val drawer: TileDrawer = new RandomTileDrawer(Some(roundNum))

      val state = GameState(
        (0 to 3).map(new ThreePointChicken(_, drawer.popHand())).toList,
        None,
        Nil,
        initPlayer,
        drawer)

      implicit val gameLogger = new DiscardDecisionDataCollector(state)
      val flow: Flow = new FlowImpl(state, Some(roundNum))

      flow.start()

      gameLogger.discardRecords.toList
    }}.toList

  private case class TrainingData(wins: List[List[Tile]], nowins: List[List[Tile]])

  printWriter.write(write(results))
  printWriter.close()
}

