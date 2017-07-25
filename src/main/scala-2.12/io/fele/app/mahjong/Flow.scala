package io.fele.app.mahjong

import java.io.{File, FileOutputStream, PrintWriter}
import java.util.Random

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.player.DrawResultType._
import io.fele.app.mahjong.player._

import scala.util.Try

/**
  * Created by felix.ling on 12/12/2016.
  */

case class WinnerInfo(id: Int, score: Int)

case class WinnerBalance(id: Int, amount: Int) {
  def +(lhs: WinnerBalance): WinnerBalance = WinnerBalance(id, this.amount + lhs.amount)
}

case class WinnersInfo(winners: Set[WinnerInfo], loserId: Option[Int], winningTile: Tile, isSelfWin: Boolean){
  def winnersBalance(implicit config: Config) : List[WinnerBalance] = if (isSelfWin){
    (0 to 3).map{ i =>
      val amount = config.scoreMap(winners.head.score.toString)
      WinnerBalance(i, if(winners.head.id == i) amount * 3 / 2 else -amount / 2 )
    }.toList
  } else {
    (0 to 3).map{ i =>
      val amount: Int = i match {
        case winnerId if winners.exists(_.id == winnerId) =>
          config.scoreMap(winners.find(_.id == winnerId).get.score.toString)
        case winnerId if winnerId == loserId.get =>
          winners.toList.map(w => -config.scoreMap(w.score.toString)).sum
        case _ => 0
      }
      WinnerBalance(i, amount)
    }.toList
  }
}

case class GameResult(winnersInfo: Option[WinnersInfo])

case class DiscardInfo(playerId: Int, tile: Tile)
case class GameState(players: List[Player],
                     var winnersInfo: Option[WinnersInfo],
                     var discards: List[DiscardInfo],
                     var curPlayerId: Int,
                     drawer: TileDrawer){
  def addDiscarded(tile: Tile) = discards = DiscardInfo(curPlayerId, tile) :: discards
  def setCurPlayer(playerId: Int) = curPlayerId = playerId

  def nextPlayerId: Int = (curPlayerId + 1) % 4

  def curPlayer: Player = players(curPlayerId)
  def nextPlayer: Player = players(nextPlayerId)

  def otherPlayers(): List[(Int, Player)] = {
    (1 to 3).map(x => (x + curPlayerId) % 4).map(y => (y, players(y))).toList
  }


}

trait Flow {
  def start(): GameResult
  def resume(discarded: Option[Tile]): GameResult
}

class FlowImpl(val state: GameState, seed: Option[Long] = None)
          (implicit gameLogger: GameLogger) extends Flow{
  // logger
  val logger = Logger(classOf[Flow])

  implicit val curStateGenerator = new CurStateGenerator(state)

  private def checkPlayersTile(f: ((Int, Player)) => Boolean): Set[Int] = {
    state.otherPlayers().filter(f).map(_._1).toSet
  }

  // check if there are players who can Win with this tile
  private object WiningTile {
    def unapply(tile: Tile): Option[Set[WinnerInfo]] = {
      val anyoneWin: ((Int, Player)) => (Boolean, Int, Int) = {
        case (i: Int, p: Player) => {
          val canWinResult = p.canWin(tile)
          (canWinResult.canWin && p.decideWin(tile, canWinResult.score,curStateGenerator.curState(i)), i, canWinResult.score)
        }
      }
      val winners = state.otherPlayers().map(anyoneWin).filter(_._1).map(w => WinnerInfo(w._2, w._3)).toSet
      if (winners.isEmpty) None else Some(winners)
    }
  }

  // check if there are players who can Kong with this tile
  private object KongableTile {
    def unapply(tile: Tile): Option[Int] = {
      val anyoneKong: ((Int, Player)) => Boolean = {
        case (i: Int, p: Player) => p.canKong(tile) && p.decideKong(tile, curStateGenerator.curState(i))
      }
      checkPlayersTile(anyoneKong).headOption
    }
  }

  private object PongableTile {
    def unapply(tile: Tile): Option[Int] = {
      val anyonePong: ((Int, Player)) => Boolean = {
        case (i: Int, p: Player) => p.canPong(tile) && p.decidePong(tile, curStateGenerator.curState(i))
      }
      checkPlayersTile(anyonePong).headOption
    }
  }

  private object ChowableTile {
    def unapply(tile: Tile): Option[(Int, ChowPosition)] = {
      val canChowPositions = state.nextPlayer.canChow(tile)
      if (canChowPositions.nonEmpty) {
        val chowPosition = verify(state.nextPlayerId, canChowPositions)(
          state.nextPlayer.decideChow(tile, canChowPositions, curStateGenerator.curState(state.nextPlayerId))
        )
        chowPosition.map(pos => (state.nextPlayerId, pos))
      }
      else
        None
    }

    // verify if the next player can chow with its decided position
    private def verify(nextPlayerId: Int, positions: Set[ChowPosition])(decision: Option[ChowPosition]): Option[ChowPosition] = decision match {
      case Some(position) if !positions.contains(position) =>
        throw new Exception(s"Player $nextPlayerId: invalid self kong decision, $position not found in $positions")
      case _ => decision
    }
  }

  def round(discard: DiscardInfo): Option[Tile] = discard.tile match {
    case WiningTile(playerInfos) =>
      state.winnersInfo = Some(WinnersInfo(playerInfos, Some(discard.playerId), discard.tile, isSelfWin = false))
      None
    case KongableTile(playerId) =>
      state.setCurPlayer(playerId)
      state.curPlayer.kong(discard.tile, discard.playerId, state.drawer) match {
        case DrawResult(DISCARD, discarded: Option[Tile], _) => discarded
        case DrawResult(WIN, Some(winningTile), Some(score)) => {
          state.winnersInfo = Some(WinnersInfo(Set(WinnerInfo(playerId, score)), None, winningTile, isSelfWin = true))
          None
        }
        case DrawResult(NO_TILE, _, _) => None
      }
    case PongableTile(playerId) =>
      state.setCurPlayer(playerId)
      Some(state.curPlayer.pong(discard.tile, discard.playerId))
    case ChowableTile(playerId, chowPosition) =>
      state.setCurPlayer(playerId)
      Some(state.curPlayer.chow(discard.tile, discard.playerId, chowPosition))
    case _ =>
      state.addDiscarded(discard.tile)
      state.setCurPlayer(state.nextPlayerId)
      state.curPlayer.draw(state.drawer) match {
        case DrawResult(DISCARD, discarded: Option[Tile], _) => discarded
        case DrawResult(WIN, Some(winningTile), Some(score)) => {
          state.winnersInfo = Some(WinnersInfo(Set(WinnerInfo(state.curPlayerId, score)), None, winningTile, isSelfWin = true))
          None
        }
        case DrawResult(NO_TILE, _, _) => None
      }
  }

  override def start(): GameResult = {
    gameLogger.start()

    // TODO: check if kong at the first place is allowed?
    var discardedTile = state.curPlayer.draw(state.drawer) match {
      case DrawResult(DISCARD, discarded: Option[Tile], _) => discarded
      case DrawResult(WIN, Some(winningTile), Some(score)) => {
        state.winnersInfo = Some(WinnersInfo(Set(WinnerInfo(state.curPlayerId, score)), None, winningTile, isSelfWin = true))
        None
      }
    }

    while (discardedTile.isDefined) {
      gameLogger.discard(DiscardEvent(state.curPlayerId, discardedTile.get))
      discardedTile = round(DiscardInfo(state.curPlayerId, discardedTile.get))
    }
    // state.players.head.asInstanceOf[FirstFelix].printDebug
    gameLogger.end(EndEvent(state.winnersInfo))
    GameResult(state.winnersInfo)
  }

  override def resume(discarded: Option[Tile]): GameResult = {
    gameLogger.resume()

    var discardedTile = discarded
    while (discardedTile.isDefined) {
      gameLogger.discard(DiscardEvent(state.curPlayerId, discardedTile.get))
      discardedTile = round(DiscardInfo(state.curPlayerId, discardedTile.get))
    }
    gameLogger.end(EndEvent(state.winnersInfo))
    GameResult(state.winnersInfo)
  }
}

object Main extends App {
  val logger = Logger("main")
  implicit val config: Config = new Config()
  val total = 10000
  var count = 0

  val randomSeed = 10001
  val random = new Random(randomSeed)

  private val f = new File("result.json")
  private val printWriter: PrintWriter = new PrintWriter(f)
  printWriter.append("[")

  //val results = (0 until total).par.map(roundNum => (roundNum, random.nextInt(4)))
  val results = (0 until total).map(roundNum => (roundNum, random.nextInt(4)))
    .map{case (roundNum, initPlayer) => {
    if(roundNum != 0) printWriter.append(",")
    count += 1
    if (count % 100 == 0)
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
    //implicit val gameLogger: GameLogger = new DummyGameLogger()
    val jsonDataGenerator: JsonDataGenerator = new JsonDataGenerator(state, printWriter)
    implicit val gameLogger: GameLogger = jsonDataGenerator
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
  printWriter.append("]")
  printWriter.close()
}

