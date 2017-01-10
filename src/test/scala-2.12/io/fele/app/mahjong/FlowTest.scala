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
  }

  private def spyPlayer(dummpyPlayer: Player): Player = {
    val spyPlayer = spy(dummpyPlayer)

    doReturn(false).when(spyPlayer).isWin(any[Tile], any[Boolean])
    doReturn(false).when(spyPlayer).isKong(any[Tile])
    doReturn(None).when(spyPlayer).isSelfKong(any[Set[Tile]])
    doReturn(false).when(spyPlayer).isPong(any[Tile])
    doReturn(None).when(spyPlayer).isChow(any[Tile], any[Set[ChowPosition]])
    doReturn(DEFAULT_DISCARDED).when(spyPlayer).pickToDiscard()
    spyPlayer
  }

  "Flow kong end when player decide to " - {
    "kong" in {
      val f = fixture

      // Given
      val drawer: TileDrawer = mock[TileDrawer]

      // When
      val otherPlayers: List[Player] = List(
        new DummyPlayer(0, List[Tile](DOT_2, DOT_6, DOT_9, BAMBOO_2, BAMBOO_4, CHARACTER_2, CHARACTER_3, CHARACTER_5, CHARACTER_7, CHARACTER_8), List(PongGroup(HONOR_WIND_SOUTH))),
        new DummyPlayer(1, List[Tile](DOT_1, DOT_3, DOT_4, BAMBOO_5, CHARACTER_1, CHARACTER_1, CHARACTER_3, CHARACTER_5, HONOR_WIND_EAST, HONOR_DRAGON_GREEN), List(ChowGroup(List[Tile](DOT_6, DOT_7, DOT_8)))),
        new DummyPlayer(2, List[Tile](DOT_2, DOT_4, DOT_6, BAMBOO_2, BAMBOO_3, BAMBOO_7, BAMBOO_7, CHARACTER_4, CHARACTER_5, CHARACTER_7, HONOR_WIND_EAST, HONOR_DRAGON_RED, HONOR_DRAGON_GREEN))
      ).map(spyPlayer)
      val subjectPlayers = spyPlayer(new DummyPlayer(
        3,
        List[Tile](BAMBOO_3, BAMBOO_5, CHARACTER_1, CHARACTER_2, CHARACTER_2, CHARACTER_9, CHARACTER_9, CHARACTER_9, HONOR_DRAGON_RED, HONOR_DRAGON_BLUE),
        List(ChowGroup(List[Tile](DOT_7, DOT_8, DOT_9)))))

      when(subjectPlayers.isKong(CHARACTER_9)).thenReturn(true)
      when(drawer.pop()).thenReturn(Some[Tile](CHARACTER_2))

      val gameState = GameState(
        otherPlayers ++ List(subjectPlayers),
        Set.empty[Int],
        None,
        List.empty[Tile],
        1)

      val flow = new Flow(gameState, drawer)
      val discarded: Option[Tile] = flow.round(CHARACTER_9)

      // Then
      discarded should equal(Some(DEFAULT_DISCARDED))
      gameState.winners should equal(Set.empty[Int])
      gameState.winningTile should equal(None)
      gameState.getCurPlayerId should equal(subjectPlayers.id)
      assert(subjectPlayers.privateInfo.tileGroups.contains(KongGroup(CHARACTER_9)))
    }
  }
}
