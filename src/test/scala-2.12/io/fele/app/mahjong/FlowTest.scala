package io.fele.app.mahjong

import io.fele.app.mahjong.ChowPosition.ChowPosition
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.mockito.MockitoSugar
import org.mockito.Mockito._
import org.mockito.Matchers.{any, eq => eqTo}
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

    doReturn(false).when(spyPlayer).isWin(any[Tile], any[Boolean])
    doReturn(false).when(spyPlayer).isKong(any[Tile])
    doReturn(None).when(spyPlayer).isSelfKong(any[Set[Tile]])
    doReturn(false).when(spyPlayer).isPong(any[Tile])
    doReturn(None).when(spyPlayer).isChow(any[Tile], any[Set[ChowPosition]])
    spyPlayer
  }

  "Flow kong end when player can and decide to " - {
    "kong" in {
      val f = fixture
      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(List[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(ChowGroup(List[Tile](DOT_7, DOT_8, DOT_9)))))

      doReturn(true).when(subjectPlayers).isKong(CHARACTER_9)
      when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_2))
      doReturn(Tile(CHARACTER_2)).when(subjectPlayers).pickToDiscard()

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        1)

      val flow = new Flow(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(CHARACTER_9)

      // Then
      discarded should equal(Some[Tile](CHARACTER_2))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo().tileGroups.contains(KongGroup(CHARACTER_9)))
      assert(!subjectPlayers.privateInfo().tileGroups.contains(CHARACTER_9))
      verify(subjectPlayers, times(1)).draw(f.drawer)
      verify(subjectPlayers, times(1)).discard()
      verify(subjectPlayers, times(1)).kong(CHARACTER_9, f.drawer)
    }

    "kong twice" in {
      val f = fixture

      val playerTiles = List(
        (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(List[Tile](DOT_6, DOT_7, DOT_8)))),
        (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
      )

      // Given
      val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(PongGroup(CHARACTER_7))))

      doReturn(true).when(subjectPlayers).isKong(CHARACTER_9)
      doReturn(Some(Tile(CHARACTER_7))).when(subjectPlayers).isSelfKong(Set(CHARACTER_7))
      when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_7)).thenReturn(Some[Tile](CHARACTER_2))
      doReturn(Tile(CHARACTER_2)).when(subjectPlayers).pickToDiscard()

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        1)

      val flow = new Flow(gameState, f.drawer)

      // When
      val discarded: Option[Tile] = flow.round(CHARACTER_9)

      // Then
      discarded should equal(Some[Tile](CHARACTER_2))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo().tileGroups.contains(KongGroup(CHARACTER_9)))
      assert(subjectPlayers.privateInfo().tileGroups.contains(KongGroup(CHARACTER_7)))
      assert(!subjectPlayers.privateInfo().tileGroups.contains(CHARACTER_9))
      assert(!subjectPlayers.privateInfo().tileGroups.contains(CHARACTER_7))
      verify(subjectPlayers, times(2)).draw(f.drawer)
      verify(subjectPlayers, times(1)).discard()
      verify(subjectPlayers, times(1)).kong(CHARACTER_9, f.drawer)
      verify(subjectPlayers, times(1)).selfKong(CHARACTER_7, f.drawer)
    }
  }

  "kong then win" in {
    val f = fixture

    val playerTiles = List(
      (List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
      (List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(List[Tile](DOT_6, DOT_7, DOT_8)))),
      (List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN), List())
    )

    // Given
    val otherPlayers: List[Player] = (0 to 2).toList.map(i => spyPlayer(new DummyPlayer(i, playerTiles(i)._1, playerTiles(i)._2)))
    val subjectPlayers = spyPlayer(new DummyPlayer(
      3,
      List[Tile](BAMBOO_3, BAMBOO_3, CHARACTER_1, CHARACTER_2, CHARACTER_3, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_RED),
      List(PongGroup(CHARACTER_7))))

    doReturn(true).when(subjectPlayers).isWin(Tile(HONOR_DRAGON_RED), true)
    doReturn(true).when(subjectPlayers).isKong(CHARACTER_9)
    doReturn(Some(Tile(CHARACTER_7))).when(subjectPlayers).isSelfKong(Set(CHARACTER_7))
    when(f.drawer.pop()).thenReturn(Some[Tile](CHARACTER_7)).thenReturn(Some[Tile](HONOR_DRAGON_RED))
    doReturn(Tile(CHARACTER_2)).when(subjectPlayers).pickToDiscard()

    val gameState = GameState(
      otherPlayers ++ List(subjectPlayers),
      Set.empty[Int],
      None,
      List.empty[Tile],
      1)

    val flow = new Flow(gameState, f.drawer)

    // When
    val discarded: Option[Tile] = flow.round(CHARACTER_9)

    // Then
    discarded should equal(None)
    gameState.winners should equal(Set(subjectPlayers.id))
    gameState.winningTile should equal(Some(Tile(HONOR_DRAGON_RED)))
    gameState.getCurPlayerId should equal(subjectPlayers.id)
    assert(subjectPlayers.privateInfo().tileGroups.contains(KongGroup(CHARACTER_9)))
    assert(subjectPlayers.privateInfo().tileGroups.contains(KongGroup(CHARACTER_7)))
    assert(!subjectPlayers.privateInfo().tileGroups.contains(CHARACTER_9))
    assert(!subjectPlayers.privateInfo().tileGroups.contains(CHARACTER_7))
    verify(subjectPlayers, times(2)).draw(f.drawer)
    verify(subjectPlayers, never()).discard()
    verify(subjectPlayers, times(1)).kong(CHARACTER_9, f.drawer)
    verify(subjectPlayers, times(1)).selfKong(CHARACTER_7, f.drawer)
  }
}
