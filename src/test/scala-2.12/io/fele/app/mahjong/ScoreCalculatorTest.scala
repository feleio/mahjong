package io.fele.app.mahjong

import org.scalatest.{FreeSpec, Matchers}
import io.fele.app.mahjong.ScoreType._
import io.fele.app.mahjong.TileValue._

/**
  * Created by felix.ling on 18/06/2017.
  */
class ScoreCalculatorTest extends FreeSpec with Matchers {
  implicit val config = new Config()
  val maxScore = 100

  "hand with different suit " - {
    "scores 3 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](D2, D3, D4, D6, D6, D6, D7, D7),
        List[TileGroup](PongGroup(HW_S), ChowGroup(Set[Tile](D6, D7, D8))),
        Tile(D7),
        maxScore
      )
      calculator.cal should equal(ScoreResult(3, Set(DifferentSuit)))
    }
  }

  "hand with same suit " - {
    "scores 7 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](C2, C2, C2, C6, C7, C8, C3, C3),
        List[TileGroup](KongGroup(C1), ChowGroup(Set[Tile](C6, C7, C8))),
        Tile(C3),
        maxScore
      )
      calculator.cal should equal(ScoreResult(7, Set(SameSuit)))
    }
  }

  "all honor " - {
    "scores maxScore" in {
      val calculator = new ScoreCalculator(
        List[Tile](HW_W, HW_W, HD_B, HD_B, HD_B, HD_G, HD_G, HD_G),
        List[TileGroup](KongGroup(HW_N), KongGroup(HD_R)),
        Tile(HW_W),
        maxScore
      )
      calculator.cal should equal(ScoreResult(maxScore, Set(AllPong, BigThreeCircle, AllHonor)))
    }
  }

  "all pong " - {
    "scores 3 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](D4, D4, D4, C6, C6, C6, HW_W, HW_W),
        List[TileGroup](PongGroup(HW_S), PongGroup(HW_E)),
        Tile(HW_W),
        maxScore
      )
      calculator.cal should equal(ScoreResult(3, Set(AllPong)))
    }
  }

  "all chow " - {
    "scores 1 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](D2, D3, D4, C6, C7, C8, B6, B7, B8, HW_W, HW_W),
        List[TileGroup](ChowGroup(Set[Tile](B6, B7, B8))),
        Tile(HW_W),
        maxScore
      )
      calculator.cal should equal(ScoreResult(1, Set(AllChow)))
    }
  }

  "different suit and all chow " - {
    "scores 4 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](D2, D3, D4, D2, D3, D4, D6, D7, D8, D6, D7, D8, HW_W, HW_W),
        List.empty[TileGroup],
        Tile(HW_W),
        maxScore
      )
      calculator.cal should equal(ScoreResult(3 + 1, Set(DifferentSuit, AllChow)))
    }
  }

  "different suit and all pong " - {
    "scores 6 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](C4, C4, C4, C6, C6, C6, HW_W, HW_W),
        List[TileGroup](PongGroup(HW_S), PongGroup(HW_E)),
        Tile(HW_W),
        maxScore
      )
      calculator.cal should equal(ScoreResult(3 + 3, Set(DifferentSuit, AllPong)))
    }
  }

  "hand with same suit and all chow" - {
    "scores 8 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](C2, C3, C4, C6, C7, C8, C3, C3),
        List[TileGroup](ChowGroup(Set[Tile](C6, C7, C8)), ChowGroup(Set[Tile](C1, C2, C3))),
        Tile(C3),
        maxScore
      )
      calculator.cal should equal(ScoreResult(7 + 1, Set(SameSuit, AllChow)))
    }
  }

  "same suit and all pong " - {
    "scores 10 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](B4, B4, B4, B6, B6, B6, B9, B9),
        List[TileGroup](KongGroup(B8), PongGroup(B7)),
        Tile(B9),
        maxScore
      )
      calculator.cal should equal(ScoreResult(7 + 3, Set(SameSuit, AllPong)))
    }
  }

  "hand with pong big four direction " - {
    "scores max points" in {
      val calculator = new ScoreCalculator(
        List[Tile](HW_E, HW_E, HW_E, HW_N, HW_N, HW_N, D7, D7),
        List[TileGroup](KongGroup(HW_S), PongGroup(HW_W)),
        Tile(D7),
        maxScore
      )
      calculator.cal should equal(ScoreResult(maxScore, Set(DifferentSuit, BigFourDirection, AllPong)))
    }
  }

  "hand with pong small four direction " - {
    "scores max points" in {
      val calculator = new ScoreCalculator(
        List[Tile](HW_E, HW_E, HW_E, HW_N, HW_N, D7, D8, D9),
        List[TileGroup](PongGroup(HW_S), KongGroup(HW_W)),
        Tile(HW_N),
        maxScore
      )
      calculator.cal should equal(ScoreResult(maxScore, Set(DifferentSuit, SmallFourDirection)))
    }
  }

  "hand with pong big three circle" - {
    "scores 8 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](HD_R, HD_R, HD_R, B6, B7, B8, D7, D7),
        List[TileGroup](KongGroup(HD_B), PongGroup(HD_G)),
        Tile(D7),
        maxScore
      )
      calculator.cal should equal(ScoreResult(8, Set(BigThreeCircle)))
    }
  }

  "hand with pong small three circle" - {
    "scores 5 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](HD_R, HD_R, B6, B7, B8, D7, D7, D7),
        List[TileGroup](KongGroup(HD_B), PongGroup(HD_G)),
        Tile(HD_R),
        maxScore
      )
      calculator.cal should equal(ScoreResult(5, Set(SmallThreeCircle)))
    }
  }

  "hand with eighteen gods" - {
    "scores max points" in {
      val calculator = new ScoreCalculator(
        List[Tile](D9, D9),
        List[TileGroup](KongGroup(B7), KongGroup(D6), KongGroup(HD_R), KongGroup(B6)),
        Tile(D9),
        maxScore
      )
      calculator.cal should equal(ScoreResult(maxScore, Set(EighteenGods, AllPong)))
    }
  }

  "hand with honor and one nine" - {
    "scores 1 more points" in {
      val calculator = new ScoreCalculator(
        List[Tile](HD_R, HD_R, HD_R, B1, B1, B1, D9, D9),
        List[TileGroup](KongGroup(HW_E), PongGroup(HW_W)),
        Tile(D9),
        maxScore
      )
      calculator.cal should equal(ScoreResult(3 + 1, Set(OneNine, AllPong)))
    }
  }

  "hand with pure one nine" - {
    "scores max point" in {
      val calculator = new ScoreCalculator(
        List[Tile](B9, B9, B9, B1, B1, B1, D9, D9),
        List[TileGroup](KongGroup(C9), PongGroup(C1)),
        Tile(D9),
        maxScore
      )
      calculator.cal should equal(ScoreResult(maxScore, Set(PureOneNine, AllPong)))
    }
  }

  "hand with Thirteen " - {
    "scores max points" in {
      val calculator = new ScoreCalculator(
        List[Tile](B1, D1, C1, D9, B9, C9, C9, HW_E, HW_S, HW_W, HW_N, HD_R, HD_G, HD_B).sortBy(_.value.id),
        List.empty[TileGroup],
        Tile(C9),
        maxScore
      )
      calculator.cal should equal(ScoreResult(maxScore, Set(Thirteen)))
    }
  }
}
