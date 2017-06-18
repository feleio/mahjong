package io.fele.app.mahjong

import scala.collection.mutable
import io.fele.app.mahjong.TileType._

/**
  * Created by felix.ling on 18/06/2017.
  */

class ScoreCalculator(
  val dynamicTiles: List[Tile],
  val fixedTileGroups: List[TileGroup],
  val eyeTile: Tile,
  val maxScore: Int) {

  val tileStats: mutable.ArraySeq[Int] = mutable.ArraySeq.fill[Int](34)(0)
  dynamicTiles.foreach(t => tileStats(t.value.id) += 1)
  fixedTileGroups.foreach{
    case KongGroup(t) => tileStats(t.value.id) += 4
    case PongGroup(t) => tileStats(t.value.id) += 3
    case ChowGroup(tileSet) => tileSet.foreach(t => tileStats(t.value.id) += 1)
  }

  val tileTypeStats: mutable.ArraySeq[Int] = mutable.ArraySeq.fill(4)(0)
  dynamicTiles.foreach(t => tileTypeStats(t.`type`.id) += 1)
  fixedTileGroups.foreach{
    case KongGroup(tile) => tileTypeStats(tile.`type`.id) += 4
    case PongGroup(tile) => tileTypeStats(tile.`type`.id) += 3
    case ChowGroup(tileSet) => tileSet.foreach(t => tileTypeStats(t.`type`.id) += 1)
  }

  // define Rules
  private def differentSuit = tileTypeStats.count(_ > 0) == 2 && tileTypeStats(HONOR.id) > 0

  private def sameSuit = {
    val tileTypesIds = tileTypeStats.zipWithIndex.filter(_._1 > 0).map(_._2)
    tileTypesIds.size == 1 && tileTypesIds.head != HONOR.id
  }

  private def allHonor = {
    val tileTypesIds = tileTypeStats.zipWithIndex.filter(_._1 > 0).map(_._2)
    tileTypesIds.size == 1 && tileTypesIds.head == HONOR.id
  }

  private def allPong = {
    val isNoChowGroups = fixedTileGroups.forall{
      case g:ChowGroup => false
      case _ => true
    }
    val dTilesWithoutEyes = dynamicTiles diff List.fill[Tile](2)(eyeTile.value)
    val isAllPong = validateAllPong(dTilesWithoutEyes.sortBy(t => t.value.id))
    isNoChowGroups && isAllPong
  }

  private def allChow = {
    val isOnlyChowGroups = fixedTileGroups.forall{
      case g:ChowGroup => true
      case _ => false
    }
    val dTilesWithoutEyes = dynamicTiles diff List.fill[Tile](2)(eyeTile.value)
    val isAllChow = validateAllChow(dTilesWithoutEyes.sortBy(t => t.value.id))
    isOnlyChowGroups && isAllChow
  }

  private def validateAllPong(sortedTiles: List[Tile]): Boolean = sortedTiles match {
    case t if t.isEmpty => true
    case t if t.head == t(1) && t(1) == t(2) => validateAllPong(sortedTiles.drop(3))
    case _ => false
  }

  private def validateAllChow(sortedTiles: List[Tile]): Boolean = sortedTiles match {
    case t if t.isEmpty => true
    case t if t.head.`type` != HONOR && t.head.num <= 7 && t.contains(t.head+1) && t.contains(t.head+2) => validateAllChow(sortedTiles diff List(t.head, t.head+1, t.head+2))
    case _ => false
  }

  def cal: Int = {
    val score = List(
      differentSuit -> 3,
      allPong -> 3,
      allChow -> 1,
      sameSuit -> 7,
      allHonor -> maxScore
    ).foldLeft(0){(score, rule) => {
      println(s"tileTypeStats: ${tileTypeStats}")
      println(rule)
      score + (if (rule._1) rule._2 else 0)}
    }
    Math.min(score, maxScore)
  }
}