package io.fele.app.mahjong

import org.scalatest.{FreeSpec, Matchers}

import io.fele.app.mahjong.Tile._
import io.fele.app.mahjong.TileValue._
import io.fele.app.mahjong.ChowPosition._

/**
  * Created by felix.ling on 09/12/2016.
  */
class HandTest extends FreeSpec with Matchers {
  "Hand should indicate whether it" - {
    "can win" in {
      assert(false)
    }

    "can kong" in {
      val tiles = List[Tile](DOT_9, HONOR_WIND_NORTH, CHARACTER_3, HONOR_DRAGON_GREEN, HONOR_WIND_EAST, BAMBOO_8, HONOR_WIND_WEST, DOT_2, BAMBOO_8, CHARACTER_9, DOT_6, DOT_8, BAMBOO_8)
      val hand = new Hand(tiles)
      hand.canKong(BAMBOO_8) should equal(true)
    }

    "cannot kong" in {
      val tiles = List[Tile](DOT_9, HONOR_WIND_NORTH, CHARACTER_3, HONOR_DRAGON_GREEN, HONOR_WIND_EAST, BAMBOO_7, HONOR_WIND_WEST, DOT_2, BAMBOO_8, CHARACTER_9, DOT_6, DOT_8, BAMBOO_8)
      val hand = new Hand(tiles)
      hand.canKong(BAMBOO_8) should equal(false)
    }

    "can pong" in {
      val tiles = List[Tile](BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_1, DOT_8, BAMBOO_6, BAMBOO_3, CHARACTER_9, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      val hand = new Hand(tiles)
      hand.canPong(BAMBOO_6) should equal(true)
    }

    "cannot pong" in {
      val tiles = List[Tile](BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_1, DOT_8, BAMBOO_6, BAMBOO_3, CHARACTER_9, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      val hand = new Hand(tiles)
      hand.canPong(HONOR_DRAGON_GREEN) should equal(false)
    }

    "can chow" in {
      var tiles = List[Tile](BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_1, DOT_8, BAMBOO_6, BAMBOO_3, CHARACTER_9, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_2)
      var hand = new Hand(tiles)
      hand.canChow(CHARACTER_3) should equal(Set(RIGHT))

      tiles = List(BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_2, DOT_8, BAMBOO_6, BAMBOO_3, CHARACTER_9, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      hand = new Hand(tiles)
      hand.canChow(BAMBOO_1) should equal(Set(LEFT))

      tiles = List(BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_2, DOT_8, BAMBOO_6, BAMBOO_3, DOT_6, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      hand = new Hand(tiles)
      hand.canChow(DOT_7) should equal(Set(MIDDLE))

      tiles = List(BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_2, DOT_8, DOT_9, BAMBOO_3, DOT_6, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      hand = new Hand(tiles)
      hand.canChow(DOT_7) should equal(Set(MIDDLE, LEFT))

      tiles = List(BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, DOT_5, DOT_8, DOT_9, BAMBOO_3, DOT_6, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      hand = new Hand(tiles)
      hand.canChow(DOT_7) should equal(Set(MIDDLE, LEFT, RIGHT))
    }

    "cannot chow" in {
      var tiles = List[Tile](BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_1, DOT_8, BAMBOO_6, BAMBOO_3, CHARACTER_9, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      var hand = new Hand(tiles)
      hand.canChow(HONOR_DRAGON_BLUE) should equal(Set())

      tiles = List(BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_2, DOT_8, BAMBOO_6, BAMBOO_3, CHARACTER_9, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      hand = new Hand(tiles)
      hand.canChow(BAMBOO_5) should equal(Set())

      tiles = List(BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_2, DOT_8, BAMBOO_6, BAMBOO_3, DOT_9, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      hand = new Hand(tiles)
      hand.canChow(DOT_9) should equal(Set())
    }
  }

  "Hand should properly execute" - {
    "kong" in {
      val tiles = List[Tile](DOT_9, HONOR_WIND_NORTH, CHARACTER_3, HONOR_DRAGON_GREEN, HONOR_WIND_EAST, BAMBOO_8, HONOR_WIND_WEST, DOT_2, BAMBOO_8, CHARACTER_9, DOT_6, DOT_8, BAMBOO_8)
      val hand = new Hand(tiles)
      hand.kong(BAMBOO_8)

      hand.tiles.size should equal(10)
      hand.tiles.toList.sortBy(_.value.id) should equal((tiles diff List.fill[Tile](3)(BAMBOO_8)).sortBy(_.value.id))
      hand.fixedTileGroups
    }

    "pong" in {
      val tiles = List[Tile](DOT_9, HONOR_WIND_NORTH, CHARACTER_3, HONOR_DRAGON_GREEN, HONOR_WIND_EAST, BAMBOO_8, HONOR_WIND_WEST, DOT_2, BAMBOO_8, CHARACTER_9, DOT_6, DOT_8, BAMBOO_8)
      val hand = new Hand(tiles)
      hand.pong(BAMBOO_8)

      hand.tiles.size should equal(11)
      hand.tiles.toList.sortBy(_.value.id) should equal((tiles diff List.fill[Tile](2)(BAMBOO_8)).sortBy(_.value.id))
    }

    "chow" in {
      var tiles = List[Tile](BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_1, DOT_3, CHARACTER_1, BAMBOO_1, DOT_8, BAMBOO_6, BAMBOO_3, CHARACTER_9, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_2)
      var hand = new Hand(tiles)
      hand.chow(CHARACTER_3, RIGHT)

      hand.tiles.size should equal(11)
      hand.tiles.toList.sortBy(_.value.id) should equal((tiles diff List[Tile](CHARACTER_1, CHARACTER_2)).sortBy(_.value.id))
      hand.tileStats(CHARACTER_1) should equal(1)
      hand.tileStats(CHARACTER_2) should equal(0)

      tiles = List[Tile](BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_2, DOT_8, BAMBOO_6, BAMBOO_3, CHARACTER_9, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      hand = new Hand(tiles)
      hand.chow(BAMBOO_1, LEFT)

      hand.tiles.size should equal(11)
      hand.tiles.toList.sortBy(_.value.id) should equal((tiles diff List[Tile](BAMBOO_2, BAMBOO_3)).sortBy(_.value.id))
      hand.tileStats(BAMBOO_2) should equal(0)
      hand.tileStats(BAMBOO_3) should equal(0)

      tiles = List[Tile](BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_2, DOT_8, BAMBOO_6, BAMBOO_3, DOT_6, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      hand = new Hand(tiles)
      hand.chow(DOT_7, MIDDLE)

      hand.tiles.size should equal(11)
      hand.tiles.toList.sortBy(_.value.id) should equal((tiles diff List[Tile](DOT_6, DOT_8)).sortBy(_.value.id))
      hand.tileStats(DOT_6) should equal(0)
      hand.tileStats(DOT_8) should equal(0)

      tiles = List[Tile](BAMBOO_6, HONOR_DRAGON_GREEN, CHARACTER_6, DOT_3, CHARACTER_1, BAMBOO_2, DOT_8, DOT_9, BAMBOO_3, DOT_6, BAMBOO_1, HONOR_DRAGON_RED_, CHARACTER_3)
      hand = new Hand(tiles)
      hand.chow(DOT_7, LEFT)

      hand.tiles.size should equal(11)
      hand.tiles.toList.sortBy(_.value.id) should equal((tiles diff List[Tile](DOT_8, DOT_9)).sortBy(_.value.id))
      hand.tileStats(DOT_8) should equal(0)
      hand.tileStats(DOT_9) should equal(0)
    }
  }


  "tiles in hand should always be sorted" in {
    var tiles = List[Tile](DOT_9, HONOR_WIND_NORTH, CHARACTER_3, HONOR_DRAGON_GREEN, HONOR_WIND_EAST, BAMBOO_8, HONOR_WIND_WEST, DOT_2, BAMBOO_8, CHARACTER_9, DOT_6, DOT_8, BAMBOO_8)
    val hand = new Hand(tiles)
    hand.tiles should equal(tiles.sortBy(_.value.id))

    tiles = tiles diff List[Tile](BAMBOO_8, BAMBOO_8)
    hand.pong(BAMBOO_8)
    hand.tiles should equal(tiles.sortBy(_.value.id))

    tiles = DOT_9 :: tiles
    hand.draw(DOT_9)
    hand.tiles should equal(tiles.sortBy(_.value.id))

    tiles = tiles diff List[Tile](HONOR_WIND_EAST)
    hand.discard(HONOR_WIND_EAST)
    hand.tiles should equal(tiles.sortBy(_.value.id))
  }
}
