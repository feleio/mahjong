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
  val logger = Logger("GenerateTrainingDataForIsWin")

  val total = 1000
  var count = 0

  val randomSeed = 5000000
  val random = new Random(randomSeed)

  private val labelTrainingData = new File("train_v7_isWin_nowins.json")
  private val printWriter: PrintWriter = new PrintWriter(labelTrainingData)

  private def winsHands(gameResult: GameResult, state: GameState): List[List[Tile]] = {
    gameResult.winnersInfo match {
      case Some(info) =>
        info.winners.map(_.id).toList.map(winnerId => state.players(winnerId).hand.dynamicTiles + info.winningTile )
      case None => List.empty[List[Tile]]
    }
  }

  private def loseHands(gameResult: GameResult, state: GameState): List[List[Tile]] = {
    gameResult.winnersInfo match {
      case Some(info) =>
        val loserIds: List[Int] = (0 to 3).filterNot(x => info.winners.map(_.id).contains(x)).toList
        loserIds.collect{
          case id if !state.players(id).hand.canWin(info.winningTile).canWin =>
            state.players(id).hand.dynamicTiles + info.winningTile
        }
      case None =>  (0 to 3).toList.collect{
        case id if !state.players(id).hand.canWin(state.discards.head.tile).canWin =>
          state.players(id).hand.dynamicTiles + state.discards.head.tile
      }
    }
  }

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

      loseHands(result, state)
    }}.toList

  val resultDistinct = results.distinct

  logger.info(s"size: ${resultDistinct.size}")

  printWriter.write(write(resultDistinct))
  printWriter.close()
}

