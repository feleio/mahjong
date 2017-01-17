package io.fele.app.mahjong

import java.util.Random

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.DrawResult._
import io.fele.app.mahjong.player.Chicken

/**
  * Created by felix.ling on 12/12/2016.
  */
case class GameResult(winners: Set[Int])
case class GameState(players: List[Player],
                     var winners: Set[Int],
                     var winningTile: Option[Tile],
                     var discards: List[(Int, Tile)],
                     var curPlayerId: Int){
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
}

class FlowImpl(val state: GameState, val drawer: TileDrawer, seed: Option[Long] = None)
          (implicit gameLogger: GameLogger) extends Flow{
  // logger
  val logger = Logger(classOf[Flow])

  implicit val curStateGenerator = new CurStateGenerator(state, drawer)

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
      state.winners = playerIds
      state.winningTile = Some(discardedTile)
      None
    case KongableTile(playerId) =>
      state.setCurPlayer(playerId)
      state.curPlayer().kong(discardedTile, drawer) match {
        case (DISCARD, discarded: Option[Tile]) => discarded
        case (WIN, Some(winningTile)) => state.winners = Set(playerId); state.winningTile = Some(winningTile); None
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
      state.curPlayer().draw(drawer) match {
        case (DISCARD, discarded: Option[Tile]) => discarded
        case (WIN, Some(winningTile)) => state.winners = Set(state.getCurPlayerId); state.winningTile = Some(winningTile); None
        case (NO_TILE, _) => None
      }
  }

  override def start(): GameResult = {
    gameLogger.start()

    // TODO: check if kong at the first place is allowed?
    var discardedTile = state.curPlayer().draw(drawer) match {
      case (DISCARD, discarded: Option[Tile]) => discarded
      case (WIN, Some(winningTile)) => state.winners = Set(state.getCurPlayerId); state.winningTile = Some(winningTile); None
    }

    while (discardedTile.isDefined) {
      gameLogger.discard(state.getCurPlayerId, discardedTile.get)
      discardedTile = round(discardedTile.get)
    }
    if (state.winners.nonEmpty)
      gameLogger.win(state.winners, state.winningTile.get)
    else
      gameLogger.noOneWin()

    GameResult(state.winners)
  }
}

object Main extends App{
  val logger = Logger("main")
  implicit val config: Config = new Config()
  val total = 10000
  var count = 0

  val results = (0 to total).par.map(roundNum => {
    count += 1
    if (count % 2000 == 0)
      logger.info(s"$count/$total")

    val drawer: TileDrawer = new RandomTileDrawer(Some(roundNum))
    val state = GameState(
      new Chicken(0, drawer.popHand()) :: (1 to 3).map(new DummyPlayer(_, drawer.popHand())).toList,
      Set.empty[Int],
      None,
      Nil,
      new Random(roundNum).nextInt(4))

    implicit val gameLogger: GameLogger = new DebugGameLogger(state)
    val flow: Flow = new FlowImpl(state, drawer, Some(roundNum))

    flow.start()
  })

  logger.info(s"Total games: $total")
  val winnerCount = results.groupBy(_.winners.size).mapValues(_.size)
  logger.info(winnerCount.toList.sortBy(_._1).toString())

  val playerWinCount = results.flatMap(_.winners.toList).groupBy(identity).mapValues(_.size)
  (0 to 3).foreach(id => logger.info(s"Player $id wins: ${playerWinCount(id)}"))
}
