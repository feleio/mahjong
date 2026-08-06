package io.fele.mahjong.server

import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong._
import io.fele.app.mahjong.player.Player
import io.fele.app.mahjong.rl.PolicyOut

/** Deterministic re-execution of a recorded game (issue #40).
  *
  * `game_records.wall_json` + the ordered `game_events` stream fully determine
  * a game (proven by GameRecordingSpec): the wall fixes the deal and every
  * draw, and each recorded event reveals what every decision resolved to. So
  * we re-drive the real [[FlowImpl]] with scripted players that answer each
  * `decide*` by peeking at the next unconsumed recorded event, and a verifying
  * [[GameLogger]] that asserts the engine re-emits exactly the recorded
  * stream — any divergence throws [[GameReplayer.ReplayMismatchException]]
  * rather than producing a wrong analysis.
  *
  * An observer receives every decision point of the observed seat with the
  * engine's own [[CurState]], which is what the champion obs encoders consume.
  */
object GameReplayer {

  class ReplayMismatchException(msg: String) extends RuntimeException(msg)

  /** Decision-point callback. `keys` maps action labels to logit indices of
    * `head` (same conventions as CoachService.hint); `chosen` is the label the
    * recorded player actually picked. `eventPos` is the index of the next
    * unconsumed event — a stable per-decision cursor into the stream. */
  trait DecisionObserver {
    def onDecision(
      eventPos:    Int,
      kind:        String,
      cs:          CurState,
      contextTile: Option[Int],
      head:        PolicyOut => Array[Float],
      keys:        List[(String, Int)],
      chosen:      String
    ): Unit
  }

  object NoopObserver extends DecisionObserver {
    override def onDecision(eventPos: Int, kind: String, cs: CurState, contextTile: Option[Int],
                            head: PolicyOut => Array[Float], keys: List[(String, Int)], chosen: String): Unit = ()
  }

  /** Replays the game to completion (or throws ReplayMismatchException). */
  def replay(
    wall:         List[String],
    events:       List[GameEventRow],
    outcome:      GameOutcome,
    observedSeat: Option[Int],
    observer:     DecisionObserver
  )(implicit config: Config): Unit = new Run(wall, events, outcome, observedSeat, observer).execute()

  private class Run(
    wallWire:     List[String],
    events:       List[GameEventRow],
    outcome:      GameOutcome,
    observedSeat: Option[Int],
    observer:     DecisionObserver
  )(implicit config: Config) {

    private var pos = 0

    private def fail(msg: String): Nothing =
      throw new ReplayMismatchException(s"replay diverged at event $pos: $msg")

    private def peek: GameEventRow =
      if (pos < events.size) events(pos) else fail("ran past the end of the recorded stream")

    private def parseTile(w: String): Tile =
      Models.tileFromWire(w).fold(e => fail(e), identity)

    /* ---- verifying logger: every engine event must equal the recorded one ---- */

    private def consume(expected: String, seat: Option[Int], source: Option[Int], tile: Option[Tile], chowPos: Option[String]): Unit = {
      val e = peek
      val ok = e.eventType == expected &&
        e.seat == seat &&
        (source.isEmpty || e.sourceSeat == source) &&
        e.tile == tile.map(Models.tileToWire) &&
        (chowPos.isEmpty || e.chowPosition == chowPos)
      if (!ok) fail(s"engine produced $expected(seat=$seat, source=$source, tile=${tile.map(Models.tileToWire)}) " +
        s"but the record says ${e.eventType}(seat=${e.seat}, source=${e.sourceSeat}, tile=${e.tile})")
      pos += 1
    }

    private val verifier: GameLogger = new GameLogger {
      override def start():  Unit = consume("start", None, None, None, None)
      override def resume(): Unit = consume("resume", None, None, None, None)
      override def draw(e: DrawEvent):       Unit = consume("draw",    Some(e.playerId), None, Some(e.tile), None)
      override def discard(e: DiscardEvent): Unit = consume("discard", Some(e.playerId), None, Some(e.tile), None)
      override def kong(e: KongEvent):       Unit = consume("kong",    Some(e.playerId), Some(e.sourcePlayerId), Some(e.tile), None)
      override def pong(e: PongEvent):       Unit = consume("pong",    Some(e.playerId), Some(e.sourcePlayerId), Some(e.tile), None)
      override def chow(e: ChowEvent):       Unit = consume("chow",    Some(e.playerId), Some(e.sourcePlayerId), Some(e.tile), Some(e.position.toString))
      override def end(e: EndEvent):         Unit = consume("end", None, None, None, None)
    }

    /* ---- scripted players: answer each decision from the recorded stream ---- */

    private class ScriptedPlayer(id: Int, tiles: List[Tile]) extends Player(id, tiles) {
      override def name: String = "Replay"

      private def observe(kind: String, cs: CurState, contextTile: Option[Int],
                          head: PolicyOut => Array[Float], keys: List[(String, Int)], chosen: String): Unit =
        if (observedSeat.contains(id)) observer.onDecision(pos, kind, cs, contextTile, head, keys, chosen)

      // Accepting a self-drawn win ends the game immediately (no draw event is
      // logged for the winning tile); declining logs the draw as usual.
      override def decideSelfWin(tile: Tile, score: Int, cs: CurState): Boolean = {
        val e = peek
        val ans = e.eventType == "end"
        if (!ans && !(e.eventType == "draw" && e.seat.contains(id))) fail(s"self_win by seat $id: unexpected ${e.eventType}")
        observe("self_win", cs, Some(tile.toTileValue), _.selfWin, CoachService.binaryKeys, if (ans) "accept" else "pass")
        ans
      }

      // A claimed win ends the game; the stream shows "end" next, and the
      // outcome names the winners (there can be several on one discard, and
      // "end" also follows a wall-exhausted draw — the outcome disambiguates).
      override def decideWin(tile: Tile, score: Int, cs: CurState): Boolean = {
        val ans = peek.eventType == "end" && !outcome.drawn && !outcome.isSelfWin &&
          outcome.winners.exists(_.seat == id) && outcome.winningTile.contains(Models.tileToWire(tile))
        observe("win", cs, Some(tile.toTileValue), _.win, CoachService.binaryKeys, if (ans) "accept" else "pass")
        ans
      }

      override def decideSelfKong(selfKongTiles: Set[Tile], cs: CurState): Option[Tile] = {
        val e = peek
        val ans =
          if (e.eventType == "kong" && e.seat.contains(id) && e.sourceSeat.contains(id)) e.tile.map(parseTile)
          else None
        val keys = ("pass" -> 0) :: selfKongTiles.toList.map(_.toTileValue).sorted.map(v => CoachService.tileWire(v) -> (v + 1))
        observe("self_kong", cs, None, _.selfKong, keys, ans.fold("pass")(Models.tileToWire))
        ans.foreach(t => if (!selfKongTiles.contains(t)) fail(s"recorded self-kong tile $t not offered to seat $id"))
        ans
      }

      override def decideKong(tile: Tile, cs: CurState): Boolean = {
        val e = peek
        val ans = e.eventType == "kong" && e.seat.contains(id) && e.sourceSeat.exists(_ != id)
        observe("kong", cs, Some(tile.toTileValue), _.kong, CoachService.binaryKeys, if (ans) "accept" else "pass")
        ans
      }

      override def decidePong(tile: Tile, cs: CurState): Boolean = {
        val e = peek
        val ans = e.eventType == "pong" && e.seat.contains(id)
        observe("pong", cs, Some(tile.toTileValue), _.pong, CoachService.binaryKeys, if (ans) "accept" else "pass")
        ans
      }

      override def decideChow(tile: Tile, positions: Set[ChowPosition], cs: CurState): Option[ChowPosition] = {
        val e = peek
        val ans =
          if (e.eventType == "chow" && e.seat.contains(id))
            e.chowPosition.map(p => ChowPosition.withName(p))
          else None
        val keys = ("pass" -> 0) :: positions.toList.sortBy(_.id).map(p => p.toString -> (p.id + 1))
        observe("chow", cs, Some(tile.toTileValue), _.chow, keys, ans.fold("pass")(_.toString))
        ans
      }

      override def decideDiscard(cs: CurState): Tile = {
        val e = peek
        if (e.eventType != "discard" || !e.seat.contains(id))
          fail(s"seat $id must discard but the record says ${e.eventType}(seat=${e.seat})")
        val t = parseTile(e.tile.getOrElse(fail("discard event without a tile")))
        val keys = cs.myInfo.tiles.map(_.toTileValue).distinct.sorted.map(v => CoachService.tileWire(v) -> v)
        observe("discard", cs, None, _.discard, keys, Models.tileToWire(t))
        t
      }
    }

    def execute(): Unit = {
      if (wallWire.size != 136) fail(s"wall has ${wallWire.size} tiles, expected 136")
      val wall = wallWire.map(parseTile)
      val drawer: TileDrawer = new RandomTileDrawer(None, Some(wall))
      val players: List[Player] = (0 until 4).map(i => new ScriptedPlayer(i, drawer.popHand()): Player).toList
      val state = GameState(players, None, Nil, 0, drawer)
      implicit val gl: GameLogger = verifier
      new FlowImpl(state).start()
      if (pos != events.size) fail(s"replay finished with ${events.size - pos} recorded events left over")
    }
  }
}
