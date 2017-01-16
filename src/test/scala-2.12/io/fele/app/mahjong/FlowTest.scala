package io.fele.app.mahjong

import io.fele.app.mahjong.ChowPosition.ChowPosition
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers.any
import io.fele.app.mahjong.TileValue._

/**
  * Created by felix.ling on 04/01/2017.
  */
class FlowTest extends FreeSpec with Matchers with MockitoSugar {
  private val DEFAULT_DISCARDED: Tile = CHARACTER_1
  implicit val mockLogger = mock[GameLogger]

  private def fixture = new {
    val drawer: TileDrawer = mock[TileDrawer]
  }

  private def spyPlayer(dummpyPlayer: Player): Player = {
    val spyPlayer = spy(dummpyPlayer)

    doReturn(false).when(spyPlayer).decideWin(any[Tile])
    doReturn(false).when(spyPlayer).decideSelfWin(any[Tile])
    doReturn(false).when(spyPlayer).decideKong(any[Tile])
    doReturn(None).when(spyPlayer).decideSelfKong(any[Set[Tile]])
    doReturn(false).when(spyPlayer).decidePong(any[Tile])
    doReturn(None).when(spyPlayer).decideChow(any[Tile], any[Set[ChowPosition]])
    spyPlayer
  }

  "It is successfully done when player can and decide to " - {
    "win" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_7, CHARACTER_8, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).decideWin(BAMBOO_3)
      when(f.drawer.pop()).thenReturn(Some(Tile(BAMBOO_6)))

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        1)

      val flow = new FlowImpl(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(BAMBOO_3)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set(subjectPlayers.id))
      gameState.winningTile should equal(Some(Tile(BAMBOO_3)))
      verify(subjectPlayers, never()).draw(f.drawer)
      verify(subjectPlayers, never()).discard()
      verify(subjectPlayers, never()).kong(any[Tile], any[TileDrawer])(any[GameLogger])
      verify(subjectPlayers, never()).pong(any[Tile])(any[GameLogger])
      verify(subjectPlayers, never()).chow(any[Tile], any[ChowPosition])(any[GameLogger])
    }

    "kong" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).decideKong(CHARACTER_9)
      when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_2))
      doReturn(Tile(CHARACTER_2)).when(subjectPlayers).decideDiscard()

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        1)

      val flow = new FlowImpl(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(CHARACTER_9)

      // Then
      discarded should equal(Some[Tile](CHARACTER_2))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(CHARACTER_9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_9)))
      verify(subjectPlayers, times(1)).draw(f.drawer)
      verify(subjectPlayers, times(1)).discard()
      verify(subjectPlayers, times(1)).kong(CHARACTER_9, f.drawer)
    }

    "kong twice" in {
      val f = fixture

      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(PongGroup(CHARACTER_7))))

      doReturn(true).when(subjectPlayers).decideKong(CHARACTER_9)
      doReturn(Some(Tile(CHARACTER_7))).when(subjectPlayers).decideSelfKong(Set(CHARACTER_7))
      when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_7)).thenReturn(Some[Tile](CHARACTER_2))
      doReturn(Tile(CHARACTER_2)).when(subjectPlayers).decideDiscard()

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        1)

      val flow = new FlowImpl(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(CHARACTER_9)

      // Then
      discarded should equal(Some[Tile](CHARACTER_2))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(CHARACTER_9)))
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(CHARACTER_7)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_7)))
      verify(subjectPlayers, times(2)).draw(f.drawer)
      verify(subjectPlayers, times(1)).discard()
      verify(subjectPlayers, times(1)).kong(CHARACTER_9, f.drawer)
      verify(subjectPlayers, times(1)).selfKong(CHARACTER_7, f.drawer)
    }

    "kong then win" in {
      val f = fixture

      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
        List(PongGroup(CHARACTER_7))))

      doReturn(true).when(subjectPlayers).decideSelfWin(Tile(HONOR_DRAGON_RED))
      doReturn(true).when(subjectPlayers).decideKong(CHARACTER_9)
      doReturn(Some(Tile(CHARACTER_7))).when(subjectPlayers).decideSelfKong(Set(CHARACTER_7))
      when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_7)).thenReturn(Some[Tile](HONOR_DRAGON_RED))
      doReturn(Tile(CHARACTER_2)).when(subjectPlayers).decideDiscard()

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        1)

      val flow = new FlowImpl(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(CHARACTER_9)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set(subjectPlayers.id))
      gameState.winningTile should equal(Some(Tile(HONOR_DRAGON_RED)))
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(CHARACTER_9)))
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(CHARACTER_7)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_7)))
      verify(subjectPlayers, times(2)).draw(f.drawer)
      verify(subjectPlayers, never()).discard()
      verify(subjectPlayers, times(1)).kong(CHARACTER_9, f.drawer)
      verify(subjectPlayers, times(1)).selfKong(CHARACTER_7, f.drawer)

    }

    "kong then run out of tiles" in {
      println("started")
      val f = fixture

      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
        List(PongGroup(CHARACTER_7))))

      doReturn(true).when(subjectPlayers).decideKong(CHARACTER_9)
      doReturn(Some(Tile(CHARACTER_7))).when(subjectPlayers).decideSelfKong(Set(CHARACTER_7))
      when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_7)).thenReturn(None)

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        1)

      val flow = new FlowImpl(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(CHARACTER_9)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(CHARACTER_9)))
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(CHARACTER_7)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_7)))
      verify(subjectPlayers, times(2)).draw(f.drawer)
      verify(subjectPlayers, never()).discard()
      verify(subjectPlayers, times(1)).kong(CHARACTER_9, f.drawer)
      verify(subjectPlayers, times(1)).selfKong(CHARACTER_7, f.drawer)

    }

    "Pong" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_8, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).decidePong(CHARACTER_9)
      doReturn(Tile(CHARACTER_2)).when(subjectPlayers).decideDiscard()

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        1)

      val flow = new FlowImpl(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(CHARACTER_9)

      // Then
      discarded should equal(Some[Tile](CHARACTER_2))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(PongGroup(CHARACTER_9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_9)))
      verify(subjectPlayers, never()).draw(f.drawer)
      verify(subjectPlayers, times(1)).discard()
      verify(subjectPlayers, times(1)).pong(CHARACTER_9)
    }

    "Chow" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_8, CHARACTER_8, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(Some(ChowPosition.RIGHT)).when(subjectPlayers).decideChow(CHARACTER_3, Set(ChowPosition.RIGHT))
      doReturn(Tile(BAMBOO_3)).when(subjectPlayers).decideDiscard()

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        2)

      val flow = new FlowImpl(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(CHARACTER_3)

      // Then
      discarded should equal(Some[Tile](BAMBOO_3))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(ChowGroup(Set[Tile](CHARACTER_1, CHARACTER_2, CHARACTER_3))))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_1)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_2)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_3)))
      verify(subjectPlayers, never()).draw(f.drawer)
      verify(subjectPlayers, times(1)).discard()
      verify(subjectPlayers, times(1)).chow(CHARACTER_3, ChowPosition.RIGHT)
    }

    "draw and discard" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_8, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(Tile(BAMBOO_3)).when(subjectPlayers).decideDiscard()
      when(f.drawer.pop()).thenReturn(Some(Tile(DOT_1)))

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        2)

      val flow = new FlowImpl(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(BAMBOO_6)

      // Then
      discarded should equal(Some[Tile](BAMBOO_3))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      verify(subjectPlayers, times(1)).draw(f.drawer)
      verify(subjectPlayers, times(1)).discard()
      verify(subjectPlayers, never()).kong(any[Tile], any[TileDrawer])(any[GameLogger])
      verify(subjectPlayers, never()).pong(any[Tile])(any[GameLogger])
      verify(subjectPlayers, never()).chow(any[Tile], any[ChowPosition])(any[GameLogger])
    }

    "draw and win" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_7, CHARACTER_8, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).decideSelfWin(BAMBOO_3)
      when(f.drawer.pop()).thenReturn(Some(Tile(BAMBOO_3)))

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        2)

      val flow = new FlowImpl(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(BAMBOO_6)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set(subjectPlayers.id))
      gameState.winningTile should equal(Some(Tile(BAMBOO_3)))
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      verify(subjectPlayers, times(1)).draw(f.drawer)
      verify(subjectPlayers, never()).discard()
      verify(subjectPlayers, never()).kong(any[Tile], any[TileDrawer])(any[GameLogger])
      verify(subjectPlayers, never()).pong(any[Tile])(any[GameLogger])
      verify(subjectPlayers, never()).chow(any[Tile], any[ChowPosition])(any[GameLogger])
    }

    "all tiles has been drawn" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_7, CHARACTER_8, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).decideSelfWin(BAMBOO_3)
      when(f.drawer.pop()).thenReturn(None)

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        2)

      val flow = new FlowImpl(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(BAMBOO_6)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      verify(subjectPlayers, times(1)).draw(f.drawer)
      verify(subjectPlayers, never()).discard()
      verify(subjectPlayers, never()).kong(any[Tile], any[TileDrawer])(any[GameLogger])
      verify(subjectPlayers, never()).pong(any[Tile])(any[GameLogger])
      verify(subjectPlayers, never()).chow(any[Tile], any[ChowPosition])(any[GameLogger])
    }
  }
  // TODO: add test case for wrong decisiton
}
