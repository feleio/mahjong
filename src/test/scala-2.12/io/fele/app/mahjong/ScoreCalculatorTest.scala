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
}
