package io.fele.mahjong.server

import cats.effect.IO
import cats.effect.std.Dispatcher
import fs2.Stream
import fs2.concurrent.Topic
import io.circe.Json
import io.circe.syntax._
import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.player._
import io.fele.app.mahjong._
import io.fele.mahjong.server.Models._

import scala.concurrent.ExecutionContext

/**
 * Owns the lifecycle of a single in-memory game. Spins up the existing
 * [[FlowImpl]] on a worker thread (the engine is synchronous and blocking)
 * and surfaces events / decision prompts to subscribers via an fs2 Topic.
 * Player actions arrive via per-seat [[WebSocketPlayer]] mailboxes.
 */
class GameRunner private (
  val roomId:     RoomId,
  val seats:      List[Seat],
  val players:    List[Player],
  val state:      GameState,
  val webPlayers: Map[Int, WebSocketPlayer],
  val hooks:      GameHooks,
  val recorder:   Option[GameRecorder]
)(implicit config: Config) {

  /** Subscribe to the live event stream. The most recent snapshot and any
    * pending prompt for this seat are replayed first so a (re)connecting
    * client immediately sees the current state. */
  def subscribe(seat: Option[Int]): Stream[IO, Json] = {
    val replay = Stream.emits(hooks.snapshot.toList ++ hooks.promptFor(seat).toList).covary[IO]
    val live   = hooks.topic.subscribe(64)
    (replay ++ live).map(j => annotateForSeat(j, seat)).filter(_ != Json.Null)
  }

  /** Submit a websocket-borne action for a seat. Returns false if the seat is not human. */
  def submitAction(seat: Int, action: WebSocketPlayer.ClientAction): Boolean =
    webPlayers.get(seat).fold(false) { p => p.submit(action); true }

  def isFinished: Boolean = hooks.finished

  /** Run the engine on a daemon worker thread. */
  def start()(implicit ec: ExecutionContext): Unit = {
    val t = new Thread(new Runnable {
      override def run(): Unit = {
        try {
          recorder.foreach(_.begin())
          val liveLogger = hooks.gameLogger(state, seats)
          implicit val gl: GameLogger =
            recorder.fold(liveLogger)(r => new TeeGameLogger(r.logger, liveLogger))
          val flow: Flow = new FlowImpl(state)
          flow.start()
          // Wall exhausted with no winner → push a final isFinished=true snapshot
          if (state.winnersInfo.isEmpty) hooks.forceFinishSnapshot()
          hooks.publish(Json.obj("type" -> Json.fromString("end")))
        } catch {
          case e: Throwable =>
            hooks.publish(Json.obj(
              "type"    -> Json.fromString("error"),
              "message" -> Json.fromString(Option(e.getMessage).getOrElse(e.toString))
            ))
        } finally {
          recorder.foreach(_.close())
          hooks.markFinished()
        }
      }
    }, s"mahjong-game-$roomId")
    t.setDaemon(true)
    t.start()
  }

  /** Hide other players' hand tiles before sending a snapshot to a particular seat,
    * and only deliver prompts to the targeted seat. */
  private def annotateForSeat(j: Json, seat: Option[Int]): Json = {
    j.hcursor.get[String]("type").toOption match {
      case Some("snapshot") =>
        val isFinished = j.hcursor.get[Boolean]("isFinished").toOption.getOrElse(false)
        val players = j.hcursor.downField("players").as[List[Json]].toOption.getOrElse(Nil)
        val redacted = if (isFinished) players else players.map { pj =>
          val pseat = pj.hcursor.get[Int]("seat").toOption
          if (seat.exists(_ == pseat.getOrElse(-1))) pj
          else pj.mapObject(_.remove("handTiles"))
        }
        j.mapObject(_
          .add("players",  Json.fromValues(redacted))
          .add("yourSeat", seat.fold(Json.Null)(Json.fromInt)))
      case Some("prompt") =>
        val target = j.hcursor.get[Int]("seat").toOption
        if (seat.isDefined && target.contains(seat.get)) j else Json.Null
      case _ => j
    }
  }
}

/** Container for the pieces shared between the GameLogger, PromptSink, and runner. */
class GameHooks(
  val roomId:     RoomId,
  val topic:      Topic[IO, Json],
  val dispatcher: Dispatcher[IO],
  onFinished:     IO[Unit] = IO.unit
) {
  @volatile private var _snapshot:    Option[Json] = None
  @volatile private var _lastPrompts: Map[Int, Json] = Map.empty
  @volatile private var _finished:    Boolean = false

  /** Set by [[GameRunner.create]] once the recorder exists, so an expired
    * prompt can be persisted as a non-decision (issue #42). */
  @volatile private var _onTimeout: (Int, String) => Unit = (_, _) => ()
  def onTimeout: (Int, String) => Unit = _onTimeout
  def onTimeout_=(f: (Int, String) => Unit): Unit = _onTimeout = f

  def snapshot:           Option[Json] = _snapshot
  def promptFor(seat: Option[Int]): Option[Json] = seat.flatMap(_lastPrompts.get)
  def finished:           Boolean      = _finished
  def markFinished():     Unit         = { _finished = true; dispatcher.unsafeRunAndForget(onFinished) }

  /** Patch the cached snapshot to isFinished=true and re-broadcast (used for wall-exhausted draws). */
  def forceFinishSnapshot(): Unit = _snapshot.foreach { j =>
    val updated = j.deepMerge(Json.obj("isFinished" -> Json.fromBoolean(true)))
    _snapshot = Some(updated)
    publish(updated)
  }

  def publish(j: Json): Unit = if (j != Json.Null) {
    dispatcher.unsafeRunAndForget(topic.publish1(j).void)
  }

  /** Build a GameLogger that publishes a fresh snapshot after every event. */
  def gameLogger(state: GameState, seats: List[Seat])(implicit config: Config): GameLogger = new GameLogger {
    override def start():  Unit = snap(Some("start"))
    override def resume(): Unit = snap(Some("resume"))
    override def discard(e: DiscardEvent): Unit = snap(Some(s"player ${e.playerId} discarded ${Models.tileToWire(e.tile)}"))
    override def kong(e: KongEvent):       Unit = snap(Some(s"player ${e.playerId} kong"))
    override def pong(e: PongEvent):       Unit = snap(Some(s"player ${e.playerId} pong"))
    override def chow(e: ChowEvent):       Unit = snap(Some(s"player ${e.playerId} chow"))
    override def end(e: EndEvent):         Unit = snap(Some("end"))
    override def draw(e: DrawEvent):       Unit = snap(Some(s"player ${e.playerId} drew"))

    private def snap(label: Option[String]): Unit = {
      val views = state.players.zipWithIndex.map { case (p, i) =>
        PlayerView(
          seat        = i,
          name        = seats(i).name,
          kind        = seats(i).kind,
          fixedGroups = p.privateInfo.tileGroups.map(TileGroupWire.from),
          handCount   = p.privateInfo.tiles.size,
          handTiles   = Some(p.privateInfo.tiles.map(Models.tileToWire))
        )
      }
      val snap = GameSnapshot(
        roomId         = roomId,
        yourSeat       = None,
        curPlayer      = state.curPlayerId,
        remainingTiles = state.drawer.remainingTiles.size,
        players        = views,
        discards       = state.discards.reverse.map(d => DiscardView(d.playerId, Models.tileToWire(d.tile))),
        lastEvent      = label,
        winners        = state.winnersInfo.toList.flatMap(_.winners.toList.map(w => WinnerView(w.id, w.score))),
        isFinished     = state.winnersInfo.isDefined,
        selfWin        = state.winnersInfo.exists(_.isSelfWin)
      )
      val j = snap.asJson.deepMerge(Json.obj("type" -> Json.fromString("snapshot")))
      _snapshot = Some(j)
      publish(j)
    }
  }

  /** Build a PromptSink that records the most recent prompt for each seat and
    * broadcasts it. Prompts only ever target human seats (only
    * [[WebSocketPlayer]] uses the sink), so every prompt carries a champion
    * coach hint when the model is available (issue #37); hint computation
    * never throws — on any failure the prompt just ships without one. */
  val promptSink: WebSocketPlayer.PromptSink = new WebSocketPlayer.PromptSink {
    private def emit(seat: Int, p: Prompt): Unit = {
      val j = p.asJson.deepMerge(Json.obj("type" -> Json.fromString("prompt")))
      _lastPrompts = _lastPrompts.updated(seat, j)
      publish(j)
    }
    override def selfWin(seat: Int, t: Tile, score: Int, st: CurState): Unit =
      emit(seat, Prompt("self_win", seat, Some(Models.tileToWire(t)), Some(score), None, None, None,
        CoachService.hint(seat, st, Some(t.toTileValue), _.selfWin, CoachService.binaryKeys)))
    override def win(seat: Int, t: Tile, score: Int, st: CurState): Unit =
      emit(seat, Prompt("win", seat, Some(Models.tileToWire(t)), Some(score), None, None, None,
        CoachService.hint(seat, st, Some(t.toTileValue), _.win, CoachService.binaryKeys)))
    override def selfKong(seat: Int, ts: Set[Tile], st: CurState): Unit = {
      // self_kong head: logit 0 = pass, logit t+1 = kong tile t
      val keys = ("pass" -> 0) :: ts.toList.map(_.toTileValue).sorted.map(v => CoachService.tileWire(v) -> (v + 1))
      emit(seat, Prompt("self_kong", seat, None, None, Some(ts.toList.map(Models.tileToWire)), None, None,
        CoachService.hint(seat, st, None, _.selfKong, keys)))
    }
    override def kong(seat: Int, t: Tile, st: CurState): Unit =
      emit(seat, Prompt("kong", seat, Some(Models.tileToWire(t)), None, None, None, None,
        CoachService.hint(seat, st, Some(t.toTileValue), _.kong, CoachService.binaryKeys)))
    override def pong(seat: Int, t: Tile, st: CurState): Unit =
      emit(seat, Prompt("pong", seat, Some(Models.tileToWire(t)), None, None, None, None,
        CoachService.hint(seat, st, Some(t.toTileValue), _.pong, CoachService.binaryKeys)))
    override def chow(seat: Int, t: Tile, ps: Set[ChowPosition], st: CurState): Unit = {
      // chow head: logit 0 = pass, logits 1..3 = LEFT/MIDDLE/RIGHT (id + 1)
      val keys = ("pass" -> 0) :: ps.toList.sortBy(_.id).map(p => p.toString -> (p.id + 1))
      emit(seat, Prompt("chow", seat, Some(Models.tileToWire(t)), None, None, Some(ps.toList.map(_.toString)), None,
        CoachService.hint(seat, st, Some(t.toTileValue), _.chow, keys)))
    }
    override def discard(seat: Int, st: CurState): Unit = {
      val keys = st.myInfo.tiles.map(_.toTileValue).distinct.sorted.map(v => CoachService.tileWire(v) -> v)
      emit(seat, Prompt("discard", seat, None, None, None, None, Some(st.myInfo.tiles.map(Models.tileToWire)),
        CoachService.hint(seat, st, None, _.discard, keys)))
    }

    /** Persist the expired prompt as a non-decision and tell the client the
      * engine is about to move for them, so the turn does not silently look
      * like a choice they made (issue #42). */
    override def timedOut(seat: Int, kind: String): Unit = {
      _onTimeout(seat, kind)
      _lastPrompts = _lastPrompts - seat
      publish(Json.obj(
        "type" -> Json.fromString("timeout"),
        "seat" -> Json.fromInt(seat),
        "kind" -> Json.fromString(kind)
      ))
    }
  }
}

object GameRunner {

  /** Build a runner whose seats map cleanly to engine players. */
  def create(
    roomId:     RoomId,
    seats:      List[Seat],
    seed:       Option[Long],
    topic:      Topic[IO, Json],
    dispatcher: Dispatcher[IO],
    onFinished: IO[Unit] = IO.unit,
    recordRepo: Option[GameRecordRepo] = None
  )(implicit config: Config): GameRunner = {
    require(seats.size == 4, "must have 4 seats")
    require(seats.forall(_.kind != SeatKind.Open), "cannot start with an open seat")

    val drawer: TileDrawer = new RandomTileDrawer(seed)
    // Capture the full wall before dealing: it determines the deal and every draw.
    val wall: Seq[Tile] = drawer.drawerState.shuffledTiles
    val hands: List[List[Tile]] = (0 until 4).map(_ => drawer.popHand()).toList

    val hooks = new GameHooks(roomId, topic, dispatcher, onFinished)
    val webPlayers = scala.collection.mutable.Map.empty[Int, WebSocketPlayer]

    val players: List[Player] = seats.zip(hands).map { case (seat, hand) =>
      seat.kind match {
        case SeatKind.Human =>
          val p = new WebSocketPlayer(seat.index, hand, Nil, hooks.promptSink)
          webPlayers.update(seat.index, p)
          p: Player
        case SeatKind.AiChicken           => new Chicken(seat.index, hand)
        case SeatKind.AiRandom            => new RandomDiscard(seat.index, hand)
        case SeatKind.AiFirstFelix        => new FirstFelix(seat.index, hand, 5)
        case SeatKind.AiThreePointChicken => new ThreePointChicken(seat.index, hand)
        case SeatKind.AiChampion          => new rl.NNPlayer(seat.index, hand, Nil, ChampionService.instance)
        case SeatKind.Open                => new Chicken(seat.index, hand)
      }
    }

    val state = GameState(
      players        = players,
      winnersInfo    = None,
      discards       = Nil,
      curPlayerId    = 0,
      drawer         = drawer
    )

    val recorder = recordRepo.map(r => new GameRecorder(r, dispatcher, roomId, seats, seed, wall))
    recorder.foreach(r => hooks.onTimeout = (seat, kind) => r.decisionTimedOut(seat, kind))

    new GameRunner(roomId, seats, players, state, webPlayers.toMap, hooks, recorder)
  }
}
