package io.fele.app.mahjong

import io.fele.app.mahjong.TileValue._
import org.scalatest.{FreeSpec, Matchers}

class WinnersInfoTest extends FreeSpec with Matchers {
  implicit val config = new Config(Some("application-test.conf"))

  "The amount of money should be arranged that" - {
    "all player give money to winner when self win" in {
      val info = WinnersInfo(Set(WinnerInfo(2, 8)), None, D3, isSelfWin = true)
      info.winnersBalance should equal(List((0, -32), (1, -32), (2, 96), (3, -32)).map(x => WinnerBalance(x._1,x._2)))
    }

    "loser player give money to the only winner when not self win" in {
      val info = WinnersInfo(Set(WinnerInfo(0, 7)), Some(2), D9, isSelfWin = false)
      info.winnersBalance should equal(List((0, 48), (1, 0), (2, -48), (3, 0)).map(x => WinnerBalance(x._1,x._2)))
    }

    "loser player give money to the 2 winners when not self win" in {
      val info = WinnersInfo(Set(WinnerInfo(1, 7), WinnerInfo(3, 5)), Some(2), D9, isSelfWin = false)
      info.winnersBalance should equal(List((0, 0), (1, 48), (2, -72), (3, 24)).map(x => WinnerBalance(x._1,x._2)))
    }

    "loser player give money to the 2 winners with same score when not self win!" in {
      val info = WinnersInfo(Set(WinnerInfo(2, 3), WinnerInfo(3, 3)), Some(1), D9, isSelfWin = false)
      info.winnersBalance should equal(List((0, 0), (1, -16), (2, 8), (3, 8)).map(x => WinnerBalance(x._1,x._2)))
    }
  }
}
