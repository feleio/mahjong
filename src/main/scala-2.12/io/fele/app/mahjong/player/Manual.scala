package io.fele.app.mahjong.player

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong._

import scala.util.Try

/**
  * Created by felix.ling on 28/01/2017.
  */
class Manual(id: Int, tiles: List[Tile], tileGroups: List[TileGroup] = List.empty[TileGroup]) extends Player(id, tiles, tileGroups) {
  val logger = Logger(classOf[Manual])

  // abstract decision method
  override def decideSelfWin(tile: Tile, curState: CurState): Boolean = {
    logger.info(s"you draw $tile do you want to win? [y/n]")
    scala.io.StdIn.readLine() match {
      case y => true
      case n => false
      case _ => decideSelfWin(tile, curState)
    }
  }

  override def decideWin(tile: Tile, curState: CurState): Boolean = {
    logger.info(s"player ${curState.curPlayerId} discarded $tile, do you want to win? [y/n]")
    scala.io.StdIn.readLine() match {
      case y => true
      case n => false
      case _ => decideWin(tile, curState)
    }
  }

  override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = {
    logger.info(s"you can kong youself, do you want to kong? " +
      s"[${selfKongTiles.map(t => t.toString.toLowerCase).mkString("/")}/no]")
    scala.io.StdIn.readLine() match {
      case "no" => None
      case t if Try(TileValue.withName(t.toUpperCase())).isSuccess && selfKongTiles.contains(TileValue.withName(t))
        => Some[Tile](TileValue.withName(t.toUpperCase()))
      case _ => decideSelfKong(selfKongTiles, curState)
    }
  }

  override def decideKong(tile: Tile, curState: CurState): Boolean = {
      logger.info(s"player ${curState.curPlayerId} discarded $tile, do you want to kong? [y/n]")
      scala.io.StdIn.readLine() match {
        case y => true
        case n => false
        case _ => decideKong(tile, curState)
      }
  }

  override def decidePong(tile: Tile, curState: CurState): Boolean = {
    logger.info(s"player ${curState.curPlayerId} discarded $tile, do you want to pong? [y/n]")
    scala.io.StdIn.readLine() match {
      case y => true
      case n => false
      case _ => decidePong(tile, curState)
    }
  }

  override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = {
    logger.info(s"player ${curState.curPlayerId} discarded $tile, do you want to chow? " +
      s"[${positions.map(pos => pos.toString.toLowerCase).mkString("/")}/no]")
    scala.io.StdIn.readLine() match {
      case "no" => None
      case s if Try(ChowPosition.withName(s.toUpperCase())).isSuccess && positions.contains(ChowPosition.withName(s.toUpperCase()))
        => Some(ChowPosition.withName(s.toUpperCase()))
      case _ => decideChow(tile, positions, curState)
    }
  }

  override def decideDiscard(curState: CurState): Tile = {
    logger.info(s"which tile do you want to discard? [${curState.myInfo.tiles.mkString("/")}]")
    scala.io.StdIn.readLine() match {
      case s if Try(TileValue.withName(s.toUpperCase())).isSuccess
        && curState.myInfo.tiles.contains(Tile(TileValue.withName(s.toUpperCase())))
          => TileValue.withName(s.toUpperCase())
      case _ => decideDiscard(curState)
    }
  }

  override def name: String = "Manual"
}
