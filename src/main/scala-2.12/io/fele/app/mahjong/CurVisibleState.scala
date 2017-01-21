package io.fele.app.mahjong

/**
  * Created by felix.ling on 17/01/2017.
  */

case class PrivateState(tiles: List[Tile], tileGroups: List[TileGroup])
case class PublicState(tileGroups: List[TileGroup])
case class CurState(myInfo: PrivateState,
                    otherInfos: List[PublicState],
                    discards: List[(Int, Tile)],
                    curPlayerId: Int,
                    remainTileNum: Int)

class CurStateGenerator(val gameState: GameState, val tileDrawer: TileDrawer) {
  def curState(playerId: Int): CurState = CurState(
    gameState.players(playerId).privateInfo,
    (1 to 3).map(id => gameState.players((id + playerId) % 4).publicInfo).toList,
    gameState.discards,
    gameState.curPlayerId,
    tileDrawer.remainingTileNum
  )
}
