package io.fele.app.mahjong

import io.fele.app.mahjong.ChowPosition.ChowPosition
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers.any
import org.mockito.Matchers.{eq => eqTo}
import io.fele.app.mahjong.TileValue._
import io.fele.app.mahjong.player.{Dummy, Player}

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

    doReturn(false).when(spyPlayer).decideWin(any[Tile], any[CurState])
    doReturn(false).when(spyPlayer).decideSelfWin(any[Tile], any[CurState])
    doReturn(false).when(spyPlayer).decideKong(any[Tile], any[CurState])
    doReturn(None).when(spyPlayer).decideSelfKong(any[Set[Tile]], any[CurState])
    doReturn(false).when(spyPlayer).decidePong(any[Tile], any[CurState])
    doReturn(None).when(spyPlayer).decideChow(any[Tile], any[Set[ChowPosition]], any[CurState])
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
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_7, CHARACTER_8, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).decideWin(eqTo(BAMBOO_3), any[CurState])
      when(f.drawer.pop()).thenReturn(Some(Tile(BAMBOO_6)))

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[(Int,Tile)],
        1,
        f.drawer)

      implicit val stateGenerator = new CurStateGenerator(gameState)

      val flow = new FlowImpl(gameState)

      // When
      val discarded: Option[Tile] = flow.round(BAMBOO_3)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set(subjectPlayers.id))
      gameState.winningTile should equal(Some(Tile(BAMBOO_3)))
      verify(subjectPlayers, never()).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).kong(any[Tile], any[TileDrawer])(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).pong(any[Tile])(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).chow(any[Tile], any[ChowPosition])(any[CurStateGenerator], any[GameLogger])
    }

    "kong" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).decideKong(eqTo(CHARACTER_9), any[CurState])
      when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_2))
      doReturn(Tile(CHARACTER_2)).when(subjectPlayers).decideDiscard(any[CurState])

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[(Int,Tile)],
        1,
        f.drawer)

      implicit val stateGenerator = new CurStateGenerator(gameState)

      val flow = new FlowImpl(gameState)

      // When
      val discarded: Option[Tile] = flow.round(CHARACTER_9)

      // Then
      discarded should equal(Some[Tile](CHARACTER_2))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(CHARACTER_9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_9)))
      verify(subjectPlayers, times(1)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).kong(eqTo(CHARACTER_9), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
    }

    "kong twice" in {
      val f = fixture

      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(PongGroup(CHARACTER_7))))

      doReturn(true).when(subjectPlayers).decideKong(eqTo(CHARACTER_9), any[CurState])
      doReturn(Some(Tile(CHARACTER_7))).when(subjectPlayers).decideSelfKong(eqTo(Set(CHARACTER_7)), any[CurState])
      when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_7)).thenReturn(Some[Tile](CHARACTER_2))
      doReturn(Tile(CHARACTER_2)).when(subjectPlayers).decideDiscard(any[CurState])

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[(Int,Tile)],
        1,
        f.drawer)

      implicit val stateGenerator = new CurStateGenerator(gameState)

      val flow = new FlowImpl(gameState)

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
      verify(subjectPlayers, times(2)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).kong(eqTo(CHARACTER_9), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).selfKong(eqTo(CHARACTER_7), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
    }

    "kong then win" in {
      val f = fixture

      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
        List(PongGroup(CHARACTER_7))))

      doReturn(true).when(subjectPlayers).decideSelfWin(eqTo(Tile(HONOR_DRAGON_RED)), any[CurState])
      doReturn(true).when(subjectPlayers).decideKong(eqTo(CHARACTER_9), any[CurState])
      doReturn(Some(Tile(CHARACTER_7))).when(subjectPlayers).decideSelfKong(eqTo(Set(CHARACTER_7)), any[CurState])
      when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_7)).thenReturn(Some[Tile](HONOR_DRAGON_RED))
      doReturn(Tile(CHARACTER_2)).when(subjectPlayers).decideDiscard(any[CurState])

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[(Int,Tile)],
        1,
        f.drawer)

      implicit val stateGenerator = new CurStateGenerator(gameState)

      val flow = new FlowImpl(gameState)

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
      verify(subjectPlayers, times(2)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).kong(eqTo(CHARACTER_9), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).selfKong(eqTo(CHARACTER_7), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])

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
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
        List(PongGroup(CHARACTER_7))))

      doReturn(true).when(subjectPlayers).decideKong(eqTo(CHARACTER_9), any[CurState])
      doReturn(Some(Tile(CHARACTER_7))).when(subjectPlayers).decideSelfKong(eqTo(Set(CHARACTER_7)), any[CurState])
      when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_7)).thenReturn(None)

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[(Int,Tile)],
        1,
        f.drawer)

      implicit val stateGenerator = new CurStateGenerator(gameState)

      val flow = new FlowImpl(gameState)

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
      verify(subjectPlayers, times(2)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).kong(eqTo(CHARACTER_9), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).selfKong(eqTo(CHARACTER_7), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])

    }

    "Pong" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_8, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).decidePong(eqTo(CHARACTER_9), any[CurState])
      doReturn(Tile(CHARACTER_2)).when(subjectPlayers).decideDiscard(any[CurState])

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[(Int,Tile)],
        1,
        f.drawer)

      implicit val stateGenerator = new CurStateGenerator(gameState)

      val flow = new FlowImpl(gameState)

      // When
      val discarded: Option[Tile] = flow.round(CHARACTER_9)

      // Then
      discarded should equal(Some[Tile](CHARACTER_2))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(PongGroup(CHARACTER_9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(CHARACTER_9)))
      verify(subjectPlayers, never()).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).pong(eqTo(CHARACTER_9))(any[CurStateGenerator], any[GameLogger])
    }

    "Chow" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_8, CHARACTER_8, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(Some(ChowPosition.RIGHT)).when(subjectPlayers).decideChow(eqTo(CHARACTER_3), eqTo(Set(ChowPosition.RIGHT)), any[CurState])
      doReturn(Tile(BAMBOO_3)).when(subjectPlayers).decideDiscard(any[CurState])

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[(Int,Tile)],
        2,
        f.drawer)

      implicit val stateGenerator = new CurStateGenerator(gameState)

      val flow = new FlowImpl(gameState)

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
      verify(subjectPlayers, never()).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).chow(eqTo(CHARACTER_3), eqTo(ChowPosition.RIGHT))(any[CurStateGenerator], any[GameLogger])
    }

    "draw and discard" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_8, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(Tile(BAMBOO_3)).when(subjectPlayers).decideDiscard(any[CurState])
      when(f.drawer.pop()).thenReturn(Some(Tile(DOT_1)))

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[(Int,Tile)],
        2,
        f.drawer)

      implicit val stateGenerator = new CurStateGenerator(gameState)

      val flow = new FlowImpl(gameState)

      // When
      val discarded: Option[Tile] = flow.round(BAMBOO_6)

      // Then
      discarded should equal(Some[Tile](BAMBOO_3))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      verify(subjectPlayers, times(1)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).kong(any[Tile], any[TileDrawer])(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).pong(any[Tile])(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).chow(any[Tile], any[ChowPosition])(any[CurStateGenerator], any[GameLogger])
    }

    "draw and win" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_7, CHARACTER_8, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).decideSelfWin(eqTo(BAMBOO_3), any[CurState])
      when(f.drawer.pop()).thenReturn(Some(Tile(BAMBOO_3)))

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[(Int,Tile)],
        2,
        f.drawer)

      implicit val stateGenerator = new CurStateGenerator(gameState)

      val flow = new FlowImpl(gameState)

      // When
      val discarded: Option[Tile] = flow.round(BAMBOO_6)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set(subjectPlayers.id))
      gameState.winningTile should equal(Some(Tile(BAMBOO_3)))
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      verify(subjectPlayers, times(1)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).kong(any[Tile], any[TileDrawer])(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).pong(any[Tile])(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).chow(any[Tile], any[ChowPosition])(any[CurStateGenerator], any[GameLogger])
    }

    "all tiles has been drawn" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(Set[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_7, CHARACTER_8, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
        List(ChowGroup(Set[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).decideSelfWin(eqTo(BAMBOO_3), any[CurState])
      when(f.drawer.pop()).thenReturn(None)

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[(Int,Tile)],
        2,
        f.drawer)

      implicit val stateGenerator = new CurStateGenerator(gameState)

      val flow = new FlowImpl(gameState)

      // When
      val discarded: Option[Tile] = flow.round(BAMBOO_6)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      verify(subjectPlayers, times(1)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).kong(any[Tile], any[TileDrawer])(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).pong(any[Tile])(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).chow(any[Tile], any[ChowPosition])(any[CurStateGenerator], any[GameLogger])
    }
  }
  // TODO: add test case for wrong decisiton
}
