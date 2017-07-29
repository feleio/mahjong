package io.fele.app.mahjong.experiment

import java.io.{File, PrintWriter}
import java.util.Random

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.Tile._
import io.fele.app.mahjong._
import io.fele.app.mahjong.player._
import org.json4s.native.Serialization.write

object GenerateTrainingData extends App {
  implicit val formats = JsonSerializer.serializers
  implicit val config: Config = new Config()
  val logger = Logger("GenerateTrainingData")

  val total = 1000
  var count = 0

  val randomSeed = 10001
  val random = new Random(randomSeed)

  private val labelTrainingData = new File("train_v5.json")
  private val printWriter: PrintWriter = new PrintWriter(labelTrainingData)


  //val results = (0 until total).par.map(roundNum => (roundNum, random.nextInt(4)))
  //
  val results: List[List[Tile]] = (0 until total)
    .par
    .map(roundNum => (roundNum, random.nextInt(4)))
    .flatMap{case (roundNum, initPlayer) => {
      count += 1
      if (count % 10000 == 0)
        logger.info(s"$count/$total")

      val drawer: TileDrawer = new RandomTileDrawer(Some(roundNum))

      val state = GameState(
        //new FirstFelix(0, drawer.popHand()) :: (1 to 3).map(new Chicken(_, drawer.popHand())).toList,
        //(0 to 3).map(new FirstFelix(_, drawer.popHand())).toList,
        new FirstFelix(0, drawer.popHand(), 5) :: (1 to 3).map(new Chicken(_, drawer.popHand())).toList,
        //(0 to 3).map(new ThreePointChicken(_, drawer.popHand())).toList,
        None,
        Nil,
        initPlayer,
        drawer)

      //implicit val gameLogger: GameLogger = new DebugGameLogger(state)
      implicit val gameLogger: GameLogger = new DummyGameLogger()
      val flow: Flow = new FlowImpl(state, Some(roundNum))

      val result = flow.start()

      result.winnersInfo match{
        case Some(info) =>
          info.winners.toList.map{
            winner => state.players(winner.id).hand.dynamicTiles + info.winningTile
          }
        case None => List.empty[List[Tile]]
      }
    }}.toList

  private case class TrainingData(wins: List[List[Tile]], nowins: List[List[Tile]])

  logger.info(s"wins size: ${results.size}")

  printWriter.write(write[List[List[Tile]]](results))
  printWriter.close()
}

