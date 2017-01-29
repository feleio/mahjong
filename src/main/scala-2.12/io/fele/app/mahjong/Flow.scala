package io.fele.app.mahjong

import java.util.Random

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.player.DrawResult._
import io.fele.app.mahjong.player.{Chicken, Dummy, Player}

/**
  * Created by felix.ling on 12/12/2016.
  */

case class WinnersInfo(winners: Set[Int], winningTile: Tile, isSelfWin: Boolean)

case class GameResult(winnersInfo: Option[WinnersInfo])
case class GameState(players: List[Player],
                     var winnersInfo: Option[WinnersInfo],
                     var discards: List[(Int, Tile)],
                     var curPlayerId: Int,
                     drawer: TileDrawer){
  def addDiscarded(tile: Tile) = discards = (curPlayerId, tile) :: discards
  def setCurPlayer(playerId: Int) = curPlayerId = playerId

  def curPlayer(): Player = players(curPlayerId)
  def getCurPlayerId: Int = curPlayerId
  def nextPlayer(): Player = players((curPlayerId + 1) % 4)
  def getNextPlayerId: Int = (curPlayerId + 1) % 4
  def otherPlayers(): List[(Int, Player)] = {
    (1 to 3).map(x => (x + curPlayerId) % 4).map(x => (x, players(x))).toList
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

  private object WiningTile {
    def unapply(tile: Tile): Option[Set[Int]] = {
      val ws = checkPlayersTile{
        case (i: Int, p: Player) => p.canWin(tile) && p.decideWin(tile, curStateGenerator.curState(i))
      }
      if (ws.isEmpty) None else Some(ws)
    }
  }

  private object KongableTile {
    def unapply(tile: Tile): Option[Int] = {
      checkPlayersTile{
        case (i: Int, p: Player) => p.canKong(tile) && p.decideKong(tile, curStateGenerator.curState(i))
      }.headOption
    }
  }

  private object PongableTile {
    def unapply(tile: Tile): Option[Int] = {
      checkPlayersTile{
        case (i: Int, p: Player) => p.canPong(tile) && p.decidePong(tile, curStateGenerator.curState(i))
      }.headOption
    }
  }

  private object ChowableTile {
    def unapply(tile: Tile): Option[(Int, ChowPosition)] = {
      val canChowPositions = state.nextPlayer().canChow(tile)
      if (canChowPositions.nonEmpty) {
        val chowPosition = verify(state.getNextPlayerId, canChowPositions)(
          state.nextPlayer().decideChow(tile, canChowPositions, curStateGenerator.curState(state.getNextPlayerId))
        )
        if (chowPosition.isDefined)
          Some((state.getNextPlayerId, chowPosition.get))
        else
          None
      }
      else
        None
    }
    private def verify(playerId: Int, positions: Set[ChowPosition])(decision: Option[ChowPosition]): Option[ChowPosition] = decision match {
      case Some(position) if !positions.contains(position) =>
        throw new Exception(s"Player $playerId: invalid self kong decision, $position not found in $positions")
      case _ => decision
    }
  }

  def round(discardedTile: Tile): Option[Tile] = discardedTile match {
    case WiningTile(playerIds) =>
      state.winnersInfo = Some(WinnersInfo(playerIds, discardedTile, false))
      None
    case KongableTile(playerId) =>
      state.setCurPlayer(playerId)
      state.curPlayer().kong(discardedTile, state.drawer) match {
        case (DISCARD, discarded: Option[Tile]) => discarded
        case (WIN, Some(winningTile)) => state.winnersInfo = Some(WinnersInfo(Set(playerId), winningTile, true)); None
        case (NO_TILE, _) => None
      }
    case PongableTile(playerId) =>
      state.setCurPlayer(playerId)
      Some(state.curPlayer().pong(discardedTile))
    case ChowableTile(playerId, chowPosition) =>
      state.setCurPlayer(playerId)
      Some(state.curPlayer().chow(discardedTile, chowPosition))
    case _ =>
      state.addDiscarded(discardedTile)
      state.setCurPlayer(state.getNextPlayerId)
      state.curPlayer().draw(state.drawer) match {
        case (DISCARD, discarded: Option[Tile]) => discarded
        case (WIN, Some(winningTile)) => state.winnersInfo = Some(WinnersInfo(Set(state.getCurPlayerId), winningTile, true)); None
        case (NO_TILE, _) => None
      }
  }

  override def start(): GameResult = {
    gameLogger.start()

    // TODO: check if kong at the first place is allowed?
    var discardedTile = state.curPlayer().draw(state.drawer) match {
      case (DISCARD, discarded: Option[Tile]) => discarded
      case (WIN, Some(winningTile)) => state.winnersInfo = Some(WinnersInfo(Set(state.getCurPlayerId), winningTile, true)); None
    }

    while (discardedTile.isDefined) {
      gameLogger.discard(state.getCurPlayerId, discardedTile.get)
      discardedTile = round(discardedTile.get)
    }
    gameLogger.end(state.winnersInfo)
    GameResult(state.winnersInfo)
  }

  override def resume(discarded: Option[Tile]): GameResult = {
    gameLogger.resume()

    var discardedTile = discarded
    while (discardedTile.isDefined) {
      gameLogger.discard(state.getCurPlayerId, discardedTile.get)
      discardedTile = round(discardedTile.get)
    }
    gameLogger.end(state.winnersInfo)
    GameResult(state.winnersInfo)
  }
}

object Main extends App{
  val logger = Logger("main")
  implicit val config: Config = new Config()
  val total = 100000
  var count = 0

  val results = (0 to total).par.map(roundNum => {
    count += 1
    if (count % 2000 == 0)
      logger.info(s"$count/$total")

    val drawer: TileDrawer = new RandomTileDrawer(Some(roundNum))
    val state = GameState(
      new Chicken(0, drawer.popHand()) :: (1 to 3).map(new Dummy(_, drawer.popHand())).toList,
      None,
      Nil,
      new Random(roundNum).nextInt(4),
      drawer)

    implicit val gameLogger: GameLogger = new DebugGameLogger(state)
    val flow: Flow = new FlowImpl(state, Some(roundNum))

    flow.start()
  })

  logger.info(s"Total games: $total")
  val winnerCount = results.groupBy(_.winnersInfo match{
    case Some(info) => info.winners.size
    case None => 0
  }).mapValues(_.size)
  logger.info(winnerCount.toList.sortBy(_._1).toString())

  val playerWinCount = results.flatMap(_.winnersInfo match{
    case Some(info) => info.winners.toList
    case None => List.empty[Int]
  }).groupBy(identity).mapValues(_.size)
  (0 to 3).foreach(id => logger.info(s"Player $id wins: ${playerWinCount(id)}"))
}
