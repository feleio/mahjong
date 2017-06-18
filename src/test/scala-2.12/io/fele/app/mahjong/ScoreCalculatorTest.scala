package io.fele.app.mahjong

import org.scalatest.{FreeSpec, Matchers}
import io.fele.app.mahjong.TileValue._

/**
  * Created by felix.ling on 18/06/2017.
  */
class ScoreCalculatorTest extends FreeSpec with Matchers {
  val maxScore = 100

  "hand with different suit " - {
    "scores 3 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](D2, D3, D4, D6, D6, D6, D7, D7),
        List[TileGroup](PongGroup(HW_S), ChowGroup(Set[Tile](D6, D7, D8))),
        Tile(D7),
        maxScore
      )
      calculator.cal should equal(3)
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
      calculator.cal should equal(7)
    }
  }

  "all honor " - {
    "scores 11 points" in {
      val calculator = new ScoreCalculator(
        List[Tile](HW_W, HW_W, HD_B, HD_B, HD_B, HD_G, HD_G, HD_G),
        List[TileGroup](KongGroup(HW_N), KongGroup(HD_R)),
        Tile(HW_W),
        maxScore
      )
      calculator.cal should equal(maxScore)
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
      calculator.cal should equal(3)
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
      calculator.cal should equal(1)
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
      calculator.cal should equal(4)
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
      calculator.cal should equal(6)
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
      calculator.cal should equal(8)
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
      calculator.cal should equal(10)
    }
  }
}
