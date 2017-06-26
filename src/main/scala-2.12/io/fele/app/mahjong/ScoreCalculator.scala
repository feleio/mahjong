package io.fele.app.mahjong

import scala.collection.mutable
import io.fele.app.mahjong.TileType._
import io.fele.app.mahjong.TileValue._

/**
  * Created by felix.ling on 18/06/2017.
  */

object ScoreType extends Enumeration {
  type ScoreType = Value
  val DifferentSuit,
  AllPong,
  AllChow,
  SameSuit,
  AllHonor,
  BigFourDirection,
  SmallFourDirection,
  BigThreeCircle,
  SmallThreeCircle,
  EighteenGods,
  OneNine,
  PureOneNine,
  Thirteen
  = Value
}

import ScoreType._

case class ScoreResult(score: Int = 0, scoreTypes: Set[ScoreType] = Set.empty)

class ScoreCalculator(
  val dynamicTiles: List[Tile],
  val fixedTileGroups: List[TileGroup],
  val eyeTile: Tile,
  val maxScore: Int)(implicit val config: Config) {

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

  private def bigFourDirection = {
    Set(HW_E, HW_S, HW_W, HW_N).forall(t => tileStats(t.id) >= 3)
  }

  private def smallFourDirection = {
    val allDirections = List(HW_E, HW_S, HW_W, HW_N)
    allDirections.contains(eyeTile.value) && (allDirections diff List(eyeTile.value)).forall(t => tileStats(t.id) >= 3)
  }

  private def bigThreeCircle = {
    Set(HD_G, HD_R, HD_B).forall(t => tileStats(t.id) >= 3)
  }

  private def smallThreeCircle = {
    val allCircle = List(HD_G, HD_R, HD_B)
    allCircle.contains(eyeTile.value) && (allCircle diff List(eyeTile.value)).forall(t => tileStats(t.id) >= 3)
  }

  private def eighteenGods = {
    fixedTileGroups.size == 4 && fixedTileGroups.forall{
      case g: KongGroup => true
      case _ => false
    }
  }

  private def oneNine = {
    val allTiles: Set[Tile] = (0 to 33).map(x => Tile(TileValue(x))).toSet
    val oneNineTiles: Set[Tile] = Set(D1, D9, B1, B9, C1, C9).map(Tile(_))
    val honorTiles: Set[Tile] = Set(HW_E, HW_S, HW_W, HW_N, HD_R, HD_G, HD_B).map(Tile(_))

    val allHonorOrOneNine = (allTiles diff oneNineTiles diff honorTiles).forall(t => tileStats(t.value.id) == 0)
    val isOneNineExist = oneNineTiles.exists(t => tileStats(t.value.id) > 0)
    val isHonorExist = honorTiles.exists(t => tileStats(t.value.id) > 0)

    allHonorOrOneNine && isHonorExist && isOneNineExist && !thirteen
  }

  private def pureOneNine = {
    val allTiles: Set[Tile] = (0 to 33).map(x => Tile(TileValue(x))).toSet
    val oneNineTiles: Set[Tile] = Set(D1, D9, B1, B9, C1, C9).map(Tile(_))

    (allTiles diff oneNineTiles).forall(t => tileStats(t.value.id) == 0)
  }

  private def thirteen = (dynamicTiles diff List(eyeTile)) == Hand.thirteenValidateTiles && fixedTileGroups.isEmpty

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

  val scoreTypes: Map[ScoreType, (Boolean, Int)] = Map(
    (DifferentSuit , (differentSuit, 3)),
    AllPong -> (allPong, 3),
    AllChow -> (allChow, 1),
    SameSuit -> (sameSuit, 7),
    AllHonor -> (allHonor, maxScore),
    BigFourDirection -> (bigFourDirection, maxScore),
    SmallFourDirection -> (smallFourDirection, maxScore),
    BigThreeCircle -> (bigThreeCircle, 8),
    SmallThreeCircle -> (smallThreeCircle, 5),
    EighteenGods -> (eighteenGods, maxScore),
    OneNine -> (oneNine, 1),
    PureOneNine -> (pureOneNine, maxScore),
    Thirteen -> (thirteen, maxScore)
  )

  def cal: ScoreResult = {
    val result: ScoreResult = scoreTypes.keySet.foldLeft(ScoreResult())((scoreResult, scoreType) => {
      val rule = scoreTypes(scoreType)
      val s = scoreResult.score + (if (rule._1) rule._2 else 0)
      val types = scoreResult.scoreTypes ++ (if (rule._1) Set(scoreType) else Set.empty[ScoreType])
      ScoreResult(s, types)
    })
    ScoreResult(Math.min(result.score, maxScore), result.scoreTypes)
  }
}