package io.fele.app.mahjong

import org.scalatest.{FreeSpec, Matchers}

import io.fele.app.mahjong.Tile._
import io.fele.app.mahjong.TileValue._
import io.fele.app.mahjong.ChowPosition._

/**
  * Created by felix.ling on 09/12/2016.
  */
class HandTest extends FreeSpec with Matchers {
  implicit val config = new Config(Some("application-test.conf"))

  "Hand should indicate whether it" - {
    "can win" in {
      var hand = new Hand(List[Tile](D9), List[TileGroup](PongGroup(D1), ChowGroup(Set[Tile](D2, D3, D4)), KongGroup(D6), PongGroup(D4)))
      hand.canWin(D9) should equal(CanWinResult(canWin = true, 7))
      hand.canWin(HW_E) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](HW_E), List[TileGroup](PongGroup(D1), ChowGroup(Set[Tile](D2, D3, D4)), KongGroup(D6), PongGroup(D4)))
      hand.canWin(HW_E) should equal(CanWinResult(canWin = true, 3))
      hand.canWin(D9) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](HW_E, D8,  HW_E, D7), List[TileGroup](PongGroup(D1), ChowGroup(Set[Tile](D2, D3, D4)), KongGroup(D6)))
      hand.canWin(D9) should equal(CanWinResult(canWin = true, 3))
      hand.canWin(D6) should equal(CanWinResult(canWin = true, 3))
      hand.canWin(D7) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D8) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](C3, C3, C3, C4), List[TileGroup](PongGroup(D1), ChowGroup(Set[Tile](D2, D3, D4)), KongGroup(D6)))
      hand.canWin(C2) should equal(CanWinResult(canWin = true, 0))
      hand.canWin(C4) should equal(CanWinResult(canWin = true, 0))
      hand.canWin(C5) should equal(CanWinResult(canWin = true, 0))
      hand.canWin(C1) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(C6) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D2) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](B8, B8, B8, D2, D2, D2, HW_N, HW_N, HW_N, D3, D3, C3, C3))
      hand.canWin(D3) should equal(CanWinResult(canWin = true, 3))
      hand.canWin(C3) should equal(CanWinResult(canWin = true, 3))
      hand.canWin(B8) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](D1, D2, D3, D2, D3, D4, D5, D5, D5, D8, D8, D9, D9))
      hand.canWin(D8) should equal(CanWinResult(canWin = true, 7))
      hand.canWin(D9) should equal(CanWinResult(canWin = true, 7))
      hand.canWin(D1) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D2) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D3) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D4) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D5) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D6) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D7) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(C8) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(C9) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](HW_S, HW_N, HD_R, HD_B), List[TileGroup](PongGroup(D1), ChowGroup(Set[Tile](D2, D3, D4)), KongGroup(D6)))
      hand.canWin(HD_R) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](B1, D1, C1, D9, B9, C9, HW_E, HW_S, HW_W, HW_N, HD_R, HD_G, HD_B))
      hand.canWin(D1) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(D9) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(B1) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(B9) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(C1) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(C9) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(HW_E) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(HW_S) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(HW_W) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(HW_N) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(HD_R) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(HD_G) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(HD_B) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(C3) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](B1, D1, C1, C1, D9, B9, C9, HW_E, HW_S, HW_N, HD_R, HD_G, HD_B))
      hand.canWin(D1) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D9) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(B1) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(B9) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(C1) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(C9) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(HW_E) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(HW_S) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(HW_W) should equal(CanWinResult(canWin = true, config.maxScore))
      hand.canWin(HW_N) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(HD_R) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(HD_G) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(HD_B) should equal(CanWinResult(canWin = false, 0))
    }

    "can win with min score 3" in {
      val configWithMinScore = new Config()
      var hand = new Hand(List[Tile](D9), List[TileGroup](PongGroup(D1), ChowGroup(Set[Tile](D2, D3, D4)), KongGroup(D6), PongGroup(D4)))(configWithMinScore)
      hand.canWin(D9) should equal(CanWinResult(canWin = true, 7))
      hand.canWin(HW_E) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](HW_E), List[TileGroup](PongGroup(D1), ChowGroup(Set[Tile](D2, D3, D4)), KongGroup(D6), PongGroup(D4)))(configWithMinScore)
      hand.canWin(HW_E) should equal(CanWinResult(canWin = true, 3))
      hand.canWin(D9) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](HW_E, D8,  HW_E, D7), List[TileGroup](PongGroup(D1), ChowGroup(Set[Tile](D2, D3, D4)), KongGroup(D6)))(configWithMinScore)
      hand.canWin(D9) should equal(CanWinResult(canWin = true, 3))
      hand.canWin(D6) should equal(CanWinResult(canWin = true, 3))
      hand.canWin(D7) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D8) should equal(CanWinResult(canWin = false, 0))

      hand = new Hand(List[Tile](C3, C3, C3, C4), List[TileGroup](PongGroup(D1), ChowGroup(Set[Tile](D2, D3, D4)), KongGroup(D6)))(configWithMinScore)
      hand.canWin(C2) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(C4) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(C5) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(C1) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(C6) should equal(CanWinResult(canWin = false, 0))
      hand.canWin(D2) should equal(CanWinResult(canWin = false, 0))
    }

    "can kong" in {
      val tiles = List[Tile](D8, HW_N, C3, HD_G, HW_E, B8, HW_W, D2, B8, C9, D6, D8, B8)
      val hand = new Hand(tiles)
      hand.canKong(B8) should equal(true)
      hand.canKong(D8) should equal(false)
    }

    "can pong" in {
      val tiles = List[Tile](B6, HD_G, C6, D3, C1, B1, D8, B6, B3, C9, B1, HD_R, C3)
      val hand = new Hand(tiles)
      hand.canPong(B6) should equal(true)
      hand.canPong(HD_G) should equal(false)
    }

    "can chow" in {
      var tiles = List[Tile](B6, HD_G, C6, D3, C1, B1, D8, B6, B3, C9, B1, HD_R, C2)
      var hand = new Hand(tiles)
      hand.canChow(C3) should equal(Set(RIGHT))

      tiles = List(B6, HD_G, C6, D3, C1, B2, D8, B6, B3, C9, B1, HD_R, C3)
      hand = new Hand(tiles)
      hand.canChow(B1) should equal(Set(LEFT))

      tiles = List(B6, HD_G, C6, D3, C1, B2, D8, B6, B3, D6, B1, HD_R, C3)
      hand = new Hand(tiles)
      hand.canChow(D7) should equal(Set(MIDDLE))

      tiles = List(B6, HD_G, C6, D3, C1, B2, D8, D9, B3, D6, B1, HD_R, C3)
      hand = new Hand(tiles)
      hand.canChow(D7) should equal(Set(MIDDLE, LEFT))

      tiles = List(B6, HD_G, C6, D3, C1, D5, D8, D9, B3, D6, B1, HD_R, C3)
      hand = new Hand(tiles)
      hand.canChow(D7) should equal(Set(MIDDLE, LEFT, RIGHT))
    }

    "cannot chow" in {
      var tiles = List[Tile](B6, HD_G, C6, D3, C1, B1, D8, B6, B3, C9, B1, HD_R, C3)
      var hand = new Hand(tiles)
      hand.canChow(HD_B) should equal(Set())

      tiles = List(B6, HD_G, C6, D3, C1, B2, D8, B6, B3, C9, B1, HD_R, C3)
      hand = new Hand(tiles)
      hand.canChow(B5) should equal(Set())

      tiles = List(B6, HD_G, C6, D3, C1, B2, D8, B6, B3, D9, B1, HD_R, C3)
      hand = new Hand(tiles)
      hand.canChow(D9) should equal(Set())
    }
  }

  "Hand should properly execute" - {
    "kong" in {
      val tiles = List[Tile](D9, HW_N, C3, HD_G, HW_E, B8, HW_W, D2, B8, C9, D6, D8, B8)
      val hand = new Hand(tiles)
      hand.kong(B8)

      hand.dynamicTiles.size should equal(10)
      hand.dynamicTiles.sortBy(_.value.id) should equal((tiles diff List.fill[Tile](3)(B8)).sortBy(_.value.id))
      hand.fixedTileGroups
    }

    "pong" in {
      val tiles = List[Tile](D9, HW_N, C3, HD_G, HW_E, B8, HW_W, D2, B8, C9, D6, D8, B8)
      val hand = new Hand(tiles)
      hand.pong(B8)

      hand.dynamicTiles.size should equal(11)
      hand.dynamicTiles.sortBy(_.value.id) should equal((tiles diff List.fill[Tile](2)(B8)).sortBy(_.value.id))
    }

    "chow" in {
      var tiles = List[Tile](B6, HD_G, C1, D3, C1, B1, D8, B6, B3, C9, B1, HD_R, C2)
      var hand = new Hand(tiles)
      hand.chow(C3, RIGHT)

      hand.dynamicTiles.size should equal(11)
      hand.dynamicTiles.sortBy(_.value.id) should equal((tiles diff List[Tile](C1, C2)).sortBy(_.value.id))
      hand.dynamicTileStats(C1) should equal(1)
      hand.dynamicTileStats(C2) should equal(0)

      tiles = List[Tile](B6, HD_G, C6, D3, C1, B2, D8, B6, B3, C9, B1, HD_R, C3)
      hand = new Hand(tiles)
      hand.chow(B1, LEFT)

      hand.dynamicTiles.size should equal(11)
      hand.dynamicTiles.sortBy(_.value.id) should equal((tiles diff List[Tile](B2, B3)).sortBy(_.value.id))
      hand.dynamicTileStats(B2) should equal(0)
      hand.dynamicTileStats(B3) should equal(0)

      tiles = List[Tile](B6, HD_G, C6, D3, C1, B2, D8, B6, B3, D6, B1, HD_R, C3)
      hand = new Hand(tiles)
      hand.chow(D7, MIDDLE)

      hand.dynamicTiles.size should equal(11)
      hand.dynamicTiles.sortBy(_.value.id) should equal((tiles diff List[Tile](D6, D8)).sortBy(_.value.id))
      hand.dynamicTileStats(D6) should equal(0)
      hand.dynamicTileStats(D8) should equal(0)

      tiles = List[Tile](B6, HD_G, C6, D3, C1, B2, D8, D9, B3, D6, B1, HD_R, C3)
      hand = new Hand(tiles)
      hand.chow(D7, LEFT)

      hand.dynamicTiles.size should equal(11)
      hand.dynamicTiles.sortBy(_.value.id) should equal((tiles diff List[Tile](D8, D9)).sortBy(_.value.id))
      hand.dynamicTileStats(D8) should equal(0)
      hand.dynamicTileStats(D9) should equal(0)
    }
  }


  "tiles in hand should always be sorted" in {
    var tiles = List[Tile](D9, HW_N, C3, HD_G, HW_E, B8, HW_W, D2, B8, C9, D6, D8, B8)
    val hand = new Hand(tiles)
    hand.dynamicTiles should equal(tiles.sortBy(_.value.id))

    tiles = tiles diff List[Tile](B8, B8)
    hand.pong(B8)
    hand.dynamicTiles should equal(tiles.sortBy(_.value.id))

    tiles = D9 :: tiles
    hand.add(D9)
    hand.dynamicTiles should equal(tiles.sortBy(_.value.id))

    tiles = tiles diff List[Tile](HW_E)
    hand.discard(HW_E)
    hand.dynamicTiles should equal(tiles.sortBy(_.value.id))
  }
}
