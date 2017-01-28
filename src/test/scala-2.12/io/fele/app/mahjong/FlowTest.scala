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
  private val DEFAULT_DISCARDED: Tile = C1
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
        (List[Tile](D2, D6, D9, B2, B4, C2, C3, C5, C7, C8), List(PongGroup(HW_S))),
        (List[Tile](D1, D3, D4, B5, C1, C1, C3, C5, HW_E, HD_G), List(ChowGroup(Set[Tile](D6, D7, D8)))),
        (List[Tile](D2, D4, D6, B2, B3, B7, B7, C4, C5, C7, HW_E, HD_R, HD_G), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](B3, B3, C1, C2, C3, C7, C8, C9, HD_R, HD_R),
        List(ChowGroup(Set[Tile](D7, D8, D9)))))

      doReturn(true).when(subjectPlayers).decideWin(eqTo(B3), any[CurState])
      when(f.drawer.pop()).thenReturn(Some(Tile(B6)))

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
      val discarded: Option[Tile] = flow.round(B3)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set(subjectPlayers.id))
      gameState.winningTile should equal(Some(Tile(B3)))
      verify(subjectPlayers, never()).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).kong(any[Tile], any[TileDrawer])(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).pong(any[Tile])(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).chow(any[Tile], any[ChowPosition])(any[CurStateGenerator], any[GameLogger])
    }

    "kong" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](D2, D6, D9, B2, B4, C2, C3, C5, C7, C8), List(PongGroup(HW_S))),
        (List[Tile](D1, D3, D4, B5, C1, C1, C3, C5, HW_E, HD_G), List(ChowGroup(Set[Tile](D6, D7, D8)))),
        (List[Tile](D2, D4, D6, B2, B3, B7, B7, C4, C5, C7, HW_E, HD_R, HD_G), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](B3, B5, C1, C2, C2, C9, C9, C9, HD_R, HD_B),
        List(ChowGroup(Set[Tile](D7, D8, D9)))))

      doReturn(true).when(subjectPlayers).decideKong(eqTo(C9), any[CurState])
      when(f.drawer.pop()).thenReturn(Some[Tile](C2))
      doReturn(Tile(C2)).when(subjectPlayers).decideDiscard(any[CurState])

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
      val discarded: Option[Tile] = flow.round(C9)

      // Then
      discarded should equal(Some[Tile](C2))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(C9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C9)))
      verify(subjectPlayers, times(1)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).kong(eqTo(C9), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
    }

    "kong twice" in {
      val f = fixture

      val playerTiles = List(
        (List[Tile](D2, D6, D9, B2, B4, C2, C3, C5, C7, C8), List(PongGroup(HW_S))),
        (List[Tile](D1, D3, D4, B5, C1, C1, C3, C5, HW_E, HD_G), List(ChowGroup(Set[Tile](D6, D7, D8)))),
        (List[Tile](D2, D4, D6, B2, B3, B7, B7, C4, C5, C7, HW_E, HD_R, HD_G), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](B3, B5, C1, C2, C2, C9, C9, C9, HD_R, HD_B),
        List(PongGroup(C7))))

      doReturn(true).when(subjectPlayers).decideKong(eqTo(C9), any[CurState])
      doReturn(Some(Tile(C7))).when(subjectPlayers).decideSelfKong(eqTo(Set(C7)), any[CurState])
      when(f.drawer.pop()).thenReturn(Some[Tile](C7)).thenReturn(Some[Tile](C2))
      doReturn(Tile(C2)).when(subjectPlayers).decideDiscard(any[CurState])

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
      val discarded: Option[Tile] = flow.round(C9)

      // Then
      discarded should equal(Some[Tile](C2))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(C9)))
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(C7)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C7)))
      verify(subjectPlayers, times(2)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).kong(eqTo(C9), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).selfKong(eqTo(C7), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
    }

    "kong then win" in {
      val f = fixture

      val playerTiles = List(
        (List[Tile](D2, D6, D9, B2, B4, C2, C3, C5, C7, C8), List(PongGroup(HW_S))),
        (List[Tile](D1, D3, D4, B5, C1, C1, C3, C5, HW_E, HD_G), List(ChowGroup(Set[Tile](D6, D7, D8)))),
        (List[Tile](D2, D4, D6, B2, B3, B7, B7, C4, C5, C7, HW_E, HD_R, HD_G), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](B3, B3, C1, C2, C3, C9, C9, C9, HD_R, HD_R),
        List(PongGroup(C7))))

      doReturn(true).when(subjectPlayers).decideSelfWin(eqTo(Tile(HD_R)), any[CurState])
      doReturn(true).when(subjectPlayers).decideKong(eqTo(C9), any[CurState])
      doReturn(Some(Tile(C7))).when(subjectPlayers).decideSelfKong(eqTo(Set(C7)), any[CurState])
      when(f.drawer.pop()).thenReturn(Some[Tile](C7)).thenReturn(Some[Tile](HD_R))
      doReturn(Tile(C2)).when(subjectPlayers).decideDiscard(any[CurState])

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
      val discarded: Option[Tile] = flow.round(C9)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set(subjectPlayers.id))
      gameState.winningTile should equal(Some(Tile(HD_R)))
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(C9)))
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(C7)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C7)))
      verify(subjectPlayers, times(2)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).kong(eqTo(C9), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).selfKong(eqTo(C7), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])

    }

    "kong then run out of tiles" in {
      println("started")
      val f = fixture

      val playerTiles = List(
        (List[Tile](D2, D6, D9, B2, B4, C2, C3, C5, C7, C8), List(PongGroup(HW_S))),
        (List[Tile](D1, D3, D4, B5, C1, C1, C3, C5, HW_E, HD_G), List(ChowGroup(Set[Tile](D6, D7, D8)))),
        (List[Tile](D2, D4, D6, B2, B3, B7, B7, C4, C5, C7, HW_E, HD_R, HD_G), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](B3, B3, C1, C2, C3, C9, C9, C9, HD_R, HD_R),
        List(PongGroup(C7))))

      doReturn(true).when(subjectPlayers).decideKong(eqTo(C9), any[CurState])
      doReturn(Some(Tile(C7))).when(subjectPlayers).decideSelfKong(eqTo(Set(C7)), any[CurState])
      when(f.drawer.pop()).thenReturn(Some[Tile](C7)).thenReturn(None)

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
      val discarded: Option[Tile] = flow.round(C9)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(C9)))
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(C7)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C7)))
      verify(subjectPlayers, times(2)).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, never()).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).kong(eqTo(C9), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).selfKong(eqTo(C7), eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])

    }

    "Pong" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](D2, D6, D9, B2, B4, C2, C3, C5, C7, C8), List(PongGroup(HW_S))),
        (List[Tile](D1, D3, D4, B5, C1, C1, C3, C5, HW_E, HD_G), List(ChowGroup(Set[Tile](D6, D7, D8)))),
        (List[Tile](D2, D4, D6, B2, B3, B7, B7, C4, C5, C7, HW_E, HD_R, HD_G), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](B3, B5, C1, C2, C2, C8, C9, C9, HD_R, HD_B),
        List(ChowGroup(Set[Tile](D7, D8, D9)))))

      doReturn(true).when(subjectPlayers).decidePong(eqTo(C9), any[CurState])
      doReturn(Tile(C2)).when(subjectPlayers).decideDiscard(any[CurState])

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
      val discarded: Option[Tile] = flow.round(C9)

      // Then
      discarded should equal(Some[Tile](C2))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(PongGroup(C9)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C9)))
      verify(subjectPlayers, never()).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).pong(eqTo(C9))(any[CurStateGenerator], any[GameLogger])
    }

    "Chow" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](D2, D6, D9, B2, B4, C2, C3, C5, C7, C8), List(PongGroup(HW_S))),
        (List[Tile](D1, D3, D4, B5, C1, C1, C3, C5, HW_E, HD_G), List(ChowGroup(Set[Tile](D6, D7, D8)))),
        (List[Tile](D2, D4, D6, B2, B3, B7, B7, C4, C5, C7, HW_E, HD_R, HD_G), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](B3, B5, C1, C2, C8, C8, C9, C9, HD_R, HD_B),
        List(ChowGroup(Set[Tile](D7, D8, D9)))))

      doReturn(Some(ChowPosition.RIGHT)).when(subjectPlayers).decideChow(eqTo(C3), eqTo(Set(ChowPosition.RIGHT)), any[CurState])
      doReturn(Tile(B3)).when(subjectPlayers).decideDiscard(any[CurState])

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
      val discarded: Option[Tile] = flow.round(C3)

      // Then
      discarded should equal(Some[Tile](B3))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(ChowGroup(Set[Tile](C1, C2, C3))))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C1)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C2)))
      assert(!subjectPlayers.privateInfo.tiles.contains(Tile(C3)))
      verify(subjectPlayers, never()).draw(eqTo(f.drawer))(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).discard()(any[CurStateGenerator], any[GameLogger])
      verify(subjectPlayers, times(1)).chow(eqTo(C3), eqTo(ChowPosition.RIGHT))(any[CurStateGenerator], any[GameLogger])
    }

    "draw and discard" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](D2, D6, D9, B2, B4, C2, C3, C5, C7, C8), List(PongGroup(HW_S))),
        (List[Tile](D1, D3, D4, B5, C1, C1, C3, C5, HW_E, HD_G), List(ChowGroup(Set[Tile](D6, D7, D8)))),
        (List[Tile](D2, D4, D6, B2, B3, B7, B7, C4, C5, C7, HW_E, HD_R, HD_G), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](B3, B5, C1, C2, C2, C8, C9, C9, HD_R, HD_B),
        List(ChowGroup(Set[Tile](D7, D8, D9)))))

      doReturn(Tile(B3)).when(subjectPlayers).decideDiscard(any[CurState])
      when(f.drawer.pop()).thenReturn(Some(Tile(D1)))

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
      val discarded: Option[Tile] = flow.round(B6)

      // Then
      discarded should equal(Some[Tile](B3))
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
        (List[Tile](D2, D6, D9, B2, B4, C2, C3, C5, C7, C8), List(PongGroup(HW_S))),
        (List[Tile](D1, D3, D4, B5, C1, C1, C3, C5, HW_E, HD_G), List(ChowGroup(Set[Tile](D6, D7, D8)))),
        (List[Tile](D2, D4, D6, B2, B3, B7, B7, C4, C5, C7, HW_E, HD_R, HD_G), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](B3, B3, C1, C2, C3, C7, C8, C9, HD_R, HD_R),
        List(ChowGroup(Set[Tile](D7, D8, D9)))))

      doReturn(true).when(subjectPlayers).decideSelfWin(eqTo(B3), any[CurState])
      when(f.drawer.pop()).thenReturn(Some(Tile(B3)))

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
      val discarded: Option[Tile] = flow.round(B6)

      // Then
      discarded should equal(None)
      gameState.winners should equal(Set(subjectPlayers.id))
      gameState.winningTile should equal(Some(Tile(B3)))
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
        (List[Tile](D2, D6, D9, B2, B4, C2, C3, C5, C7, C8), List(PongGroup(HW_S))),
        (List[Tile](D1, D3, D4, B5, C1, C1, C3, C5, HW_E, HD_G), List(ChowGroup(Set[Tile](D6, D7, D8)))),
        (List[Tile](D2, D4, D6, B2, B3, B7, B7, C4, C5, C7, HW_E, HD_R, HD_G), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new Dummy(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new Dummy(
        3,
        List[Tile](B3, B3, C1, C2, C3, C7, C8, C9, HD_R, HD_R),
        List(ChowGroup(Set[Tile](D7, D8, D9)))))

      doReturn(true).when(subjectPlayers).decideSelfWin(eqTo(B3), any[CurState])
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
      val discarded: Option[Tile] = flow.round(B6)

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
