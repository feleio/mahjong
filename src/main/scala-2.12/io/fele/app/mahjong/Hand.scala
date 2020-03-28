package io.fele.app.mahjong

import io.fele.app.mahjong.Tile._
import io.fele.app.mahjong.TileType._
import io.fele.app.mahjong.TileValue._
import io.fele.app.mahjong.ChowPosition._

import scala.collection.mutable

/**
  * Created by felix.ling on 04/12/2016.
  */

sealed trait TileGroup {
  def getCount: Int
  def toString: String
}

case class KongGroup(tile: Tile) extends TileGroup{
  override def getCount: Int = 4
  override def toString: String = s"Kong(${List.fill(getCount)(tile).mkString(", ")})"
}

case class PongGroup(tile: Tile) extends TileGroup{
  override def getCount: Int = 3
  override def toString: String = s"Pong(${List.fill(getCount)(tile).mkString(", ")})"
}

case class ChowGroup(tiles: Set[Tile]) extends TileGroup{
  override def getCount: Int = 3
  override def toString: String = s"Chow(${tiles.toList.sortBy(t => t.toTileValue).mkString(", ")})"
}

case class CanWinResult(canWin: Boolean = false, score: Int = 0)

object Hand{
  val thirteenValidateTiles: List[Tile] = List(D1, D9, B1, B9, C1, C9, HW_E, HW_S, HW_W, HW_N, HD_R, HD_G, HD_B)
}

class Hand(ts: List[Tile], gs: List[TileGroup] = List.empty[TileGroup])(implicit val config: Config) {
  if (ts.length + (gs.length * 3) != 13) throw new IllegalArgumentException("invalid number of tiles.")

  var dynamicTiles: List[Tile] = ts.sortBy(_.toTileValue)
  var fixedTileGroups: List[TileGroup] = gs
  var dynamicTileStats = mutable.ArraySeq.fill[Int](34)(0)

  dynamicTiles.foreach(x => {
    dynamicTileStats(x.toTileValue) += 1
  })

  // check if a tile exist in this hand. Tiles in TileGroups are not counted
  private def isExist(t: NumberTile): Boolean = dynamicTileStats(t.toTileValue) >= 1

  // recursively check if the hand is winning with a sorted tile List
  private def validate(sortedTiles: List[Tile]): Boolean = sortedTiles match {
    case Nil =>
      true
    case t1 :: t2 :: t3 :: rest if t1 == t2 && t2 == t3 =>
      validate(rest)
    case (t1: NumberTile) :: rest
      if t1.number <= 7 && rest.contains(t1 + 1) && rest.contains(t1 + 2) =>
        validate(sortedTiles diff List(t1, t1 + 1, t1 + 2))
    case _ => false
  }

  private def validateThirteen(sortedTiles: List[Tile]): Boolean = sortedTiles == Hand.thirteenValidateTiles

  // check if this hand can win with the specific tile
  def canWin(tile: Tile): CanWinResult = {
    // filter the eyes tile value id, check there exists winning hands with these eyes
    val eyeTileIds = dynamicTileStats.zipWithIndex
      .filter{case (tileCount, i) => tileCount >= 2 || (tileCount == 1 && i == tile.toTileValue )}
      .map(_._2)

    var res = eyeTileIds.foldLeft(CanWinResult())((result, eyeTileId) => {
      if(validate((dynamicTiles + tile) diff List.fill[Tile](2)(TileValue(eyeTileId)))) {
        val scoreResult = new ScoreCalculator(
          dynamicTiles + tile,
          fixedTileGroups,
          Tile.toTile(eyeTileId),
          config.maxScore
        ).cal
        CanWinResult(scoreResult.score >= config.minScore, Math.max(result.score, scoreResult.score))
      } else result
    })

    if(eyeTileIds.size == 1 && validateThirteen((dynamicTiles + tile) diff List[Tile](TileValue(eyeTileIds.head)))) {
      val scoreResult = new ScoreCalculator(
        dynamicTiles + tile,
        fixedTileGroups,
        Tile.toTile(eyeTileIds.head),
        config.maxScore
      ).cal
      res = CanWinResult(scoreResult.score >= config.minScore, Math.max(res.score, scoreResult.score))
    }
    res
  }

  // find which tile can be kong in current hand. It can be tile count == 4 OR pong group + count == 1
  def canSelfKong(): Set[Tile] = {
    val kongableTiles = dynamicTileStats.zipWithIndex.collect{case (count, tileId) if count >= 4 => Tile.toTile(tileId)}.toSet
    val kongablePongGroupTiles = fixedTileGroups.collect{case PongGroup(tile) if dynamicTileStats(tile.toTileValue) >= 1 => tile}.toSet
    kongableTiles | kongablePongGroupTiles
  }

  def canKong(tile: Tile): Boolean = dynamicTileStats(tile.toTileValue) >= 3
  def canPong(tile: Tile): Boolean = dynamicTileStats(tile.toTileValue) >= 2
  def canChow(tile: Tile): Set[ChowPosition] = {
    tile match {
      case t: NumberTile =>
        Set(
          (t.number > 2 && isExist((t - 2)) && isExist((t - 1))) -> RIGHT,
          (t.number > 1 && t.number < 9 && isExist((t - 1)) && isExist((t + 1))) -> MIDDLE,
          (t.number < 8 && isExist((t + 1)) && isExist((t + 2))) -> LEFT
        ).collect{case x if x._1 => x._2}
      case _: HonorTile =>
        Set.empty
    }
  }

  def kong(tile: Tile): Unit = {
    dynamicTileStats(tile.toTileValue) -= 3
    (1 to 3).foreach(_ => dynamicTiles = dynamicTiles - tile)
    fixedTileGroups = KongGroup(tile) :: fixedTileGroups
  }

  def selfKong(tile: Tile): Unit = {
    dynamicTileStats(tile.toTileValue) match {
      case 4 => {
        dynamicTileStats(tile.toTileValue) -= 4
        (1 to 4).foreach(_ => dynamicTiles = dynamicTiles - tile)
        fixedTileGroups = KongGroup(tile) :: fixedTileGroups
      }
      case 1 => {
        dynamicTileStats(tile.toTileValue) -= 1
        dynamicTiles = dynamicTiles - tile
        fixedTileGroups = fixedTileGroups diff List(PongGroup(tile))
        fixedTileGroups = KongGroup(tile) :: fixedTileGroups
      }
    }
  }

  def pong(tile: Tile): Unit = {
    dynamicTileStats(tile.toTileValue) -= 2
    (1 to 2).foreach(_ => dynamicTiles = dynamicTiles - tile)
    fixedTileGroups = PongGroup(tile) :: fixedTileGroups
  }

  def chow(tile: NumberTile, position: ChowPosition): Unit = {
    val existTiles: List[Tile] = position match {
      case LEFT => List[Tile](tile + 1, tile + 2)
      case MIDDLE => List[Tile](tile - 1, tile + 1)
      case RIGHT => List[Tile](tile - 2, tile - 1)
    }

    existTiles.foreach(x => {
      dynamicTileStats(x.toTileValue) -= 1
      dynamicTiles -= x
    })
    fixedTileGroups = ChowGroup(existTiles.toSet + tile) :: fixedTileGroups
  }

  def add(tile: Tile): Unit = {
    dynamicTileStats(tile.toTileValue) += 1
    dynamicTiles = dynamicTiles + tile
  }

  def discard(tile: Tile): Unit = {
    dynamicTileStats(tile.toTileValue) -= 1
    dynamicTiles = dynamicTiles - tile
  }

  override def toString: String = {
    s"fixed: ${fixedTileGroups.mkString(" ")}\ntiles: ${dynamicTiles.sortBy(_.toTileValue).mkString(" ")}\n"
  }
}
