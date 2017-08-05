package io.fele.app.mahjong.experiment

import java.io.{File, PrintWriter}
import java.util.Random

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.Tile._
import io.fele.app.mahjong._
import io.fele.app.mahjong.player._
import org.json4s.native.Serialization.write

object GenerateTrainingDataForIsWin extends App {
  implicit val formats = JsonSerializer.serializers
  implicit val config: Config = new Config()
  val logger = Logger("GenerateTrainingData")

  val total = 10000000
  var count = 0

  val randomSeed = 10001
  val random = new Random(randomSeed)

  private val labelTrainingData = new File("train_v7_isWin.json")
  private val printWriter: PrintWriter = new PrintWriter(labelTrainingData)


  //val results = (0 until total).par.map(roundNum => (roundNum, random.nextInt(4)))
  //
  val results: List[(List[List[Tile]], List[List[Tile]])] = (0 until total)
    .par
    .map(roundNum => (roundNum, random.nextInt(4)))
    .map{case (roundNum, initPlayer) => {
      count += 1
      if (count % 10000 == 0)
        logger.info(s"$count/$total")

      val drawer: TileDrawer = new RandomTileDrawer(Some(roundNum))

      val state = GameState(
        //new FirstFelix(0, drawer.popHand()) :: (1 to 3).map(new Chicken(_, drawer.popHand())).toList,
        //(0 to 3).map(new FirstFelix(_, drawer.popHand())).toList,
        (0 to 3).map(new ThreePointChicken(_, drawer.popHand())).toList,
        //(0 to 3).map(new ThreePointChicken(_, drawer.popHand())).toList,
        None,
        Nil,
        initPlayer,
        drawer)

      //implicit val gameLogger: GameLogger = new DebugGameLogger(state)
      implicit val gameLogger: GameLogger = new DummyGameLogger()
      val flow: Flow = new FlowImpl(state, Some(roundNum))

      val result = flow.start()

      result.winnersInfo match {
        case Some(info) =>
          val wins: List[List[Tile]] = info.winners.map(_.id).toList.map(winnerId => state.players(winnerId).hand.dynamicTiles + info.winningTile )
          val loserIds: List[Int] = (0 to 3).filterNot(x => info.winners.map(_.id).contains(x)).toList
          val nowins: List[List[Tile]] = loserIds.collect{
            case id if !state.players(id).hand.canWin(info.winningTile).canWin =>
              state.players(id).hand.dynamicTiles + info.winningTile
          }
          (wins, nowins)
        case None =>
          val nowins: List[List[Tile]] = (0 to 3).toList.collect{
            case id if !state.players(id).hand.canWin(state.discards.head.tile).canWin =>
              state.players(id).hand.dynamicTiles + state.discards.head.tile
          }
          (List.empty[List[Tile]], nowins)
      }
    }}.toList

  val trainingData = results.foldLeft((List.empty[List[Tile]], List.empty[List[Tile]])){
    case (s,t) => (s._1 ++ t._1 ,s._2 ++ t._2 )
  }

  private case class TrainingData(wins: List[List[Tile]], nowins: List[List[Tile]])

  logger.info(s"wins size: ${trainingData._1.size} nowins size: ${trainingData._2.size} ")

  printWriter.write(write(TrainingData(trainingData._1, trainingData._2)))
  printWriter.close()
}

