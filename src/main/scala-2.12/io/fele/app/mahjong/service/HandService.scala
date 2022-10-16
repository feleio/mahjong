package io.fele.app.mahjong.service

import io.fele.app.mahjong.Config
import io.fele.app.mahjong.model.ChowPosition.ChowPosition
import io.fele.app.mahjong.model.{BambooTile, CharacterTile, ChowGroup, ChowPosition, DotTile, Hand, HonorTile, KongGroup, NumberTile, PongGroup, Tile}
import io.fele.app.mahjong.repo.HandRepo
import io.fele.app.mahjong.service.HandService.CanWinResult
import io.fele.app.mahjong.util.Utils
import io.fele.app.mahjong.util.Utils.TileValue

import scala.collection.mutable

class HandService(config: Config, handRepo: HandRepo) {
  import HandService.RichTile
  import HandService.RichNumberTile
  import HandService.RichTileList

  val initialHand: Hand = handRepo.get
  if (initialHand.tiles.length + (initialHand.tileGroups.length * 3) != 13)
    throw new IllegalArgumentException("invalid number of tiles.")

  var dynamicTileStats: mutable.ArraySeq[Int] = mutable.ArraySeq.fill[Int](34)(0)

  initialHand.tiles.foreach(tile => {
    dynamicTileStats(tile.toValue) += 1
  })

  handRepo.setTiles(initialHand.tiles.sortBy(_.toValue))

  // check if a tile exist in this hand. Tiles in TileGroups are not counted
  private def isExist(t: NumberTile): Boolean = dynamicTileStats(t.toValue) >= 1

  // recursively check if the hand is winning with a sorted tile List
  private def checkWin(sortedTiles: List[Tile]): Boolean = sortedTiles match {
    case Nil =>
      true
    case t1 :: t2 :: t3 :: rest if t1 == t2 && t2 == t3 =>
      checkWin(rest)
    case (t1: NumberTile) :: rest
      if t1.number <= 7 && rest.contains(t1 + 1) && rest.contains(t1 + 2) =>
      checkWin(sortedTiles diff List(t1, t1 + 1, t1 + 2))
    case _ => false
  }

  private def validateThirteen(sortedTiles: List[Tile]): Boolean = sortedTiles == Hand.thirteenValidateTiles

  // check if this hand can win with the specific tile
  def canWin(tile: Tile): CanWinResult = {
    // filter the eyes tile value id, check there exists winning hands with these eyes
    val eyeTileIds: Seq[Int] = dynamicTileStats.zipWithIndex
      .filter {
        case (tileCount, i) =>
          tileCount >= 2 || (tileCount == 1 && i == tile.toValue)
      }.map(_._2)

    val hand: Hand = handRepo.get
    val dynamicTiles: List[Tile] = hand.tiles
    val fixedTileGroups = hand.tileGroups
    var result: CanWinResult = eyeTileIds.foldLeft(CanWinResult(canWin = false))((result, eyeTileId) => {
      if(checkWin((dynamicTiles + tile) diff List.fill[Tile](2)(TileValue(eyeTileId)))) {
        val scoreResult = new ScoreCalculator(
          dynamicTiles + tile,
          fixedTileGroups,
          Utils.tileValue2Tile(TileValue(eyeTileId)),
          config.maxScore
        ).cal
        CanWinResult(scoreResult.score >= config.minScore, Math.max(result.score, scoreResult.score))
      } else result
    })

    if(eyeTileIds.size == 1 && validateThirteen((dynamicTiles + tile) diff List[Tile](TileValue(eyeTileIds.head)))) {
      val scoreResult = new ScoreCalculator(
        dynamicTiles + tile,
        fixedTileGroups,
        Utils.int2Tile(eyeTileIds.head),
        config.maxScore
      ).cal
      result = CanWinResult(scoreResult.score >= config.minScore, Math.max(result.score, scoreResult.score))
    }
    result
  }

  // find which tile can be kong in current hand. It can be tile count == 4 OR pong group + count == 1
  def canSelfKong(): Set[Tile] = {
    val hand: Hand = handRepo.get
    val kongableTiles = dynamicTileStats.zipWithIndex.collect {
      case (count, tileId) if count >= 4 =>
        Utils.int2Tile(tileId)
    }.toSet
    val kongablePongGroupTiles = hand.tileGroups.collect {
      case PongGroup(tile) if dynamicTileStats(tile.toValue) >= 1 => tile
    }.toSet
    kongableTiles | kongablePongGroupTiles
  }

  def canKong(tile: Tile): Boolean = dynamicTileStats(tile.toValue) >= 3
  def canPong(tile: Tile): Boolean = dynamicTileStats(tile.toValue) >= 2
  def canChow(tile: Tile): Set[ChowPosition] = {
    import ChowPosition._
    tile match {
      case t: NumberTile =>
        Set(
          (t.number > 2 && isExist((t - 2)) && isExist((t - 1))) -> RIGHT,
          (t.number > 1 && t.number < 9 && isExist((t - 1)) && isExist((t + 1))) -> MIDDLE,
          (t.number < 8 && isExist((t + 1)) && isExist((t + 2))) -> LEFT
        ).collect { case (condition, position) if condition => position }
      case _: HonorTile =>
        Set.empty
    }
  }

  def kong(tile: Tile): Unit = {
    val currentHand = handRepo.get
    dynamicTileStats(tile.toValue) -= 3
    handRepo.setTiles(currentHand.tiles --- List(tile, tile, tile))
    handRepo.setTileGroups(KongGroup(tile) :: currentHand.tileGroups)
  }

  def selfKong(tile: Tile): Unit = {
    val currentHand = handRepo.get
    dynamicTileStats(tile.toValue) match {
      case 4 => {
        dynamicTileStats(tile.toValue) -= 4
        handRepo.setTiles(currentHand.tiles --- List(tile, tile, tile, tile))
        handRepo.setTileGroups(KongGroup(tile) :: currentHand.tileGroups)
      }
      case 1 => {
        dynamicTileStats(tile.toValue) -= 1
        handRepo.setTiles(currentHand.tiles - tile)
        handRepo.setTileGroups(KongGroup(tile) :: (currentHand.tileGroups diff List(PongGroup(tile))))
      }
    }
  }

  def pong(tile: Tile): Unit = {
    val currentHand = handRepo.get
    dynamicTileStats(tile.toValue) -= 2
    handRepo.setTiles(currentHand.tiles --- List(tile, tile))
    handRepo.setTileGroups(PongGroup(tile) :: currentHand.tileGroups)
  }

  def chow(tile: NumberTile, position: ChowPosition): Unit = {
    import ChowPosition._
    val existTiles: List[Tile] = position match {
      case LEFT => List[Tile](tile + 1, tile + 2)
      case MIDDLE => List[Tile](tile - 1, tile + 1)
      case RIGHT => List[Tile](tile - 2, tile - 1)
    }

    val currentHand: Hand = handRepo.get
    existTiles.foreach { tile =>
      dynamicTileStats(tile.toValue) -= 1
    }
    handRepo.setTiles(currentHand.tiles --- existTiles)
    handRepo.setTileGroups(ChowGroup(existTiles.toSet + tile) :: currentHand.tileGroups)
  }

  def add(tile: Tile): Unit = {
    dynamicTileStats(tile.toValue) += 1
    handRepo.setTiles(handRepo.get.tiles + tile)
  }

  def discard(tile: Tile): Unit = {
    dynamicTileStats(tile.toValue) -= 1
    handRepo.setTiles(handRepo.get.tiles - tile)
  }

  override def toString: String = {
    val currentHand: Hand = handRepo.get
    s"fixed: ${currentHand.tileGroups.mkString(" ")}\ntiles: ${currentHand.tiles.sortBy(_.toValue).mkString(" ")}\n"
  }
}



object HandService {
  case class CanWinResult(canWin: Boolean, score: Int = 0)

  implicit class RichTile(tile: Tile) {
    def toValue: Int = {
      tile match {
        case DotTile(number) => number - 1
        case BambooTile(number) => 9 + number - 1
        case CharacterTile(number) => (9 * 2) + number - 1
        case HonorTile(value) => (9 * 3) + value.id
      }
    }
  }

  implicit class RichNumberTile(tile: NumberTile) {
    def +(i: Int): NumberTile = {
      if(tile.number + i < 1 || tile.number + i > 9)
        throw new IllegalArgumentException("invalid value to add / minus on a Tile")
      else
        NumberTile(tile.`type`, tile.number + i)
    }
    def -(i: Int): NumberTile = this + -i
  }

  implicit class RichTileList(tiles: List[Tile]) {
    def +(tile: Tile): List[Tile] = {
      (tile :: tiles).sortBy(_.toValue)
    }

    def -(tile: Tile): List[Tile] = {
      tiles diff List(tile)
    }

    def +++(ts: List[Tile]): List[Tile] = {
      (tiles ++ ts).sortBy(_.toValue)
    }

    def ---(ts: List[Tile]): List[Tile] = {
      tiles diff ts
    }
  }
}