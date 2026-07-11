package io.fele.app.mahjong.web

import java.util.concurrent.{ArrayBlockingQueue, ConcurrentHashMap}
import java.util.concurrent.atomic.AtomicInteger

import io.fele.app.mahjong._
import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.player.{Chicken, Player}
import io.fele.app.mahjong.rl.{OnnxPolicyService, PolicyOut, V3Obs}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

import scala.util.{Failure, Success, Try}

/**
  * WebGameServer — persistent JSON-over-stdin/stdout engine host for the
  * multiplayer web game. Same transport pattern as RLGymServer, but supports
  * many concurrent games, each on its own thread, with any mix of remote
  * (human, driven by the Node backend) and bot seats.
  *
  * Node → Scala (one JSON per line):
  *   {"cmd":"new_game","gameId":"g1","seed":123,"dealerSeat":0,
  *    "seats":["remote","chicken","chicken","chicken"]}
  *   {"cmd":"action","gameId":"g1","requestId":7,"action":<bool|int|null>}
  *   {"cmd":"abort_game","gameId":"g1"}
  *   {"cmd":"ping"}
  *
  * Scala → Node (one JSON per line, all tagged with gameId):
  *   {"type":"game_started","gameId":...,"dealerSeat":n,"snapshot":{...}}
  *   {"type":"event","gameId":...,"event":{"kind":...},"snapshot":{...}}
  *   {"type":"decision_request","gameId":...,"requestId":n,"seat":s,
  *    "decision":"discard|self_win|win|self_kong|kong|pong|chow",
  *    "context":{...},"snapshot":{...}}
  *   {"type":"game_over","gameId":...,"winnersInfo":{...}|null,
  *    "balances":[i,i,i,i],"snapshot":{...}}
  *   {"type":"aborted","gameId":...}
  *   {"type":"error","gameId":...,"message":"..."}
  *   {"type":"pong"}
  *
  * Snapshots are unredacted (all four hands visible); the Node backend is
  * responsible for redacting per-seat views before broadcasting to clients.
  */
object WebGameServer extends App {
  // Claim the real stdout for the JSON protocol before any library (logback
  // status output, engine loggers) can write to it; everything else goes to
  // stderr.
  private val protocolOut =
    new java.io.PrintStream(new java.io.FileOutputStream(java.io.FileDescriptor.out), true, "UTF-8")
  System.setOut(System.err)

  implicit val formats: Formats = Serialization.formats(NoTypeHints)
  implicit val config: Config = new Config()

  // Optional coach models: attach each net's decision probabilities (+ value
  // estimate) to every remote seat's decision_request, so the UI can show how
  // trained models would play. Hints for ALL loaded models ride on every
  // request (~1ms each) — the client picks which to display, so switching
  // needs no server state.
  //   -Dweb.coachmodels=name=path,name=path   (ordered; first = UI default)
  //   -Dweb.coachmodel=path                   (legacy single, named "coach")
  // Absent properties ⇒ no coach field, feature off. A model that fails to
  // load is skipped with a warning rather than killing the server.
  private val coachSvcs: List[(String, OnnxPolicyService)] = {
    val multi = Option(System.getProperty("web.coachmodels")).toList
      .flatMap(_.split(',').toList).flatMap { spec =>
        spec.split('=') match {
          case Array(name, path) => Some(name.trim -> path.trim)
          case _ =>
            System.err.println(s"coach: bad spec '$spec' (want name=path)"); None
        }
      }
    val single = Option(System.getProperty("web.coachmodel")).map("coach" -> _)
    (multi ++ single).flatMap { case (name, path) =>
      Try(new OnnxPolicyService(path)) match {
        case Success(svc) =>
          System.err.println(s"coach model loaded: $name = $path")
          Some(name -> svc)
        case Failure(ex) =>
          System.err.println(s"coach model FAILED to load: $name = $path (${ex.getMessage})")
          None
      }
    }
  }

  private val out = new Object // stdout write lock
  private def send(msg: Map[String, Any]): Unit = out.synchronized {
    protocolOut.println(write(msg))
  }

  private val sessions = new ConcurrentHashMap[String, GameSession]()

  // ── Session ────────────────────────────────────────────────────────────────

  private object AbortSignal
  private case class ActionValue(value: Any) // wrapper: BlockingQueue rejects raw nulls

  class GameSession(val gameId: String,
                    seatSpecs: List[String],
                    seed: Option[Long],
                    dealerSeat: Int) {
    private val requestCounter = new AtomicInteger(0)
    private val pending = new ConcurrentHashMap[Int, ArrayBlockingQueue[Any]]()
    @volatile private var aborted = false

    val drawer: TileDrawer = new RandomTileDrawer(seed)
    val players: List[Player] = seatSpecs.zipWithIndex.map {
      case ("remote", i) => new RemotePlayer(i, drawer.popHand(), this)
      case (_, i)        => new Chicken(i, drawer.popHand())
    }
    val state = GameState(players, None, Nil, dealerSeat, drawer)

    // ── snapshot ──
    private def groupToMap(g: TileGroup): Map[String, Any] = g match {
      case PongGroup(t)  => Map("kind" -> "pong", "tiles" -> List.fill(3)(t.toTileValue))
      case KongGroup(t)  => Map("kind" -> "kong", "tiles" -> List.fill(4)(t.toTileValue))
      case ChowGroup(ts) => Map("kind" -> "chow", "tiles" -> ts.toList.map(_.toTileValue).sorted)
    }

    def snapshot(): Map[String, Any] = Map(
      "hands"    -> players.map(_.privateInfo.tiles.map(_.toTileValue).sorted),
      "groups"   -> players.map(_.privateInfo.tileGroups.reverse.map(groupToMap)),
      "discards" -> state.discards.reverse.map(d => Map("seat" -> d.playerId, "tile" -> d.tile.toTileValue)),
      "remaining" -> drawer.remainingTiles.size,
      "curSeat"  -> state.curPlayerId
    )

    def sendEvent(event: Map[String, Any]): Unit =
      send(Map("type" -> "event", "gameId" -> gameId, "event" -> event, "snapshot" -> snapshot()))

    // ── decision plumbing (called from the game thread via RemotePlayer) ──
    def requestDecision(seat: Int, decision: String, context: Map[String, Any],
                        coach: Option[Map[String, Any]] = None): Any = {
      if (aborted) throw new GameAbortedException
      val requestId = requestCounter.incrementAndGet()
      val queue = new ArrayBlockingQueue[Any](1)
      pending.put(requestId, queue)
      // Re-check after registering: an abort() between the check above and the
      // put() would drain pending before our queue existed, blocking forever.
      if (aborted) {
        pending.remove(requestId)
        throw new GameAbortedException
      }
      send(Map(
        "type" -> "decision_request", "gameId" -> gameId, "requestId" -> requestId,
        "seat" -> seat, "decision" -> decision, "context" -> context,
        "snapshot" -> snapshot()
      ) ++ coach.map("coach" -> _))
      val answer = queue.take() // block the game thread until Node answers
      pending.remove(requestId)
      answer match {
        case AbortSignal        => throw new GameAbortedException
        case ActionValue(value) => value
        case other              => other
      }
    }

    def deliver(requestId: Int, action: Any): Unit = {
      val queue = pending.get(requestId)
      if (queue != null) queue.offer(ActionValue(action))
      else send(Map("type" -> "error", "gameId" -> gameId,
        "message" -> s"no pending request $requestId"))
    }

    def abort(): Unit = {
      aborted = true
      pending.values().forEach(q => q.offer(AbortSignal))
    }

    // ── game thread ──
    def run(): Unit = {
      val logger: GameLogger = new GameLogger {
        override def start(): Unit = ()
        override def resume(): Unit = ()
        override def draw(e: DrawEvent): Unit =
          sendEvent(Map("kind" -> "draw", "seat" -> e.playerId, "tile" -> e.tile.toTileValue))
        override def discard(e: DiscardEvent): Unit =
          sendEvent(Map("kind" -> "discard", "seat" -> e.playerId, "tile" -> e.tile.toTileValue))
        override def pong(e: PongEvent): Unit =
          sendEvent(Map("kind" -> "pong", "seat" -> e.playerId, "fromSeat" -> e.sourcePlayerId, "tile" -> e.tile.toTileValue))
        override def kong(e: KongEvent): Unit =
          sendEvent(Map("kind" -> "kong", "seat" -> e.playerId, "fromSeat" -> e.sourcePlayerId, "tile" -> e.tile.toTileValue))
        override def chow(e: ChowEvent): Unit =
          sendEvent(Map("kind" -> "chow", "seat" -> e.playerId, "fromSeat" -> e.sourcePlayerId,
            "tile" -> e.tile.toTileValue, "position" -> e.position.id))
        override def end(e: EndEvent): Unit = () // game_over sent from run() below
      }

      val result = Try {
        val flow = new FlowImpl(state)(logger)
        flow.start()
      }

      sessions.remove(gameId)
      result match {
        case Success(gameResult) =>
          val winnersInfo = gameResult.winnersInfo.map { info =>
            Map(
              "winners" -> info.winners.toList.sortBy(_.id).map(w => Map("seat" -> w.id, "score" -> w.score)),
              "loserSeat" -> info.loserId.map(_.asInstanceOf[Any]).orNull,
              "winningTile" -> info.winningTile.toTileValue,
              "isSelfWin" -> info.isSelfWin
            )
          }
          val balances = gameResult.winnersInfo match {
            case Some(info) => info.winnersBalance.sortBy(_.id).map(_.amount)
            case None       => List(0, 0, 0, 0)
          }
          send(Map("type" -> "game_over", "gameId" -> gameId,
            "winnersInfo" -> winnersInfo.orNull, "balances" -> balances,
            "snapshot" -> snapshot()))
        case Failure(_: GameAbortedException) =>
          send(Map("type" -> "aborted", "gameId" -> gameId))
        case Failure(ex) =>
          send(Map("type" -> "error", "gameId" -> gameId,
            "message" -> s"game crashed: ${ex.getClass.getSimpleName}: ${ex.getMessage}"))
      }
    }
  }

  class GameAbortedException extends RuntimeException("game aborted")

  // ── RemotePlayer ───────────────────────────────────────────────────────────

  class RemotePlayer(id: Int, tiles: List[Tile], session: GameSession)(implicit config: Config)
      extends Player(id, tiles) {

    private def asBool(a: Any): Boolean = a match {
      case b: Boolean => b
      case _          => false
    }
    private def asOptInt(a: Any): Option[Int] = a match {
      case null       => None
      case v: BigInt  => Some(v.toInt)
      case v: Int     => Some(v)
      case v: Long    => Some(v.toInt)
      case v: Double  => Some(v.toInt)
      case _          => None
    }

    // ── coach hint: the net's probabilities over this decision's actions ──

    private def softmaxOver(logits: Array[Float],
                            keys: List[(String, Int)]): Map[String, Double] = {
      val mx   = keys.map { case (_, i) => logits(i) }.max
      val exps = keys.map { case (k, i) => k -> math.exp((logits(i) - mx).toDouble) }
      val sum  = exps.map(_._2).sum
      exps.map { case (k, e) => k -> e / sum }.toMap
    }

    /** Per-model probs over `keys` (label → logit index of the decision's
      * head) plus each net's value estimate, keyed by model name. Never
      * throws — a coach failure must not touch the game; a failing model
      * just omits its hint, and an empty result omits the field. */
    private def coach(cs: CurState, contextTile: Option[Int],
                      head: PolicyOut => Array[Float],
                      keys: List[(String, Int)]): Option[Map[String, Any]] = {
      if (coachSvcs.isEmpty) return None
      val obs = Try(V3Obs.fromCurState(id, cs, contextTile)).getOrElse(return None)
      val hints: Map[String, Any] = coachSvcs.flatMap { case (name, svc) =>
        Try {
          val nnOut = svc.query(obs)
          name -> Map[String, Any]("probs" -> softmaxOver(head(nnOut), keys),
                                   "value" -> nnOut.value.toDouble)
        }.toOption
      }.toMap
      if (hints.isEmpty) None else Some(hints)
    }

    private val binaryKeys = List("pass" -> 0, "accept" -> 1)

    override def decideSelfWin(tile: Tile, score: Int, curState: CurState): Boolean =
      asBool(session.requestDecision(id, "self_win",
        Map("tile" -> tile.toTileValue, "score" -> score),
        coach(curState, Some(tile.toTileValue), _.selfWin, binaryKeys)))

    override def decideWin(tile: Tile, score: Int, curState: CurState): Boolean =
      asBool(session.requestDecision(id, "win",
        Map("tile" -> tile.toTileValue, "score" -> score, "fromSeat" -> curState.curPlayerId),
        coach(curState, Some(tile.toTileValue), _.win, binaryKeys)))

    override def decideSelfKong(selfKongTiles: Set[Tile], curState: CurState): Option[Tile] = {
      val valid = selfKongTiles.map(_.toTileValue)
      // self_kong head: logit 0 = pass, logit t+1 = kong tile t
      val keys = ("pass" -> 0) :: valid.toList.sorted.map(t => t.toString -> (t + 1))
      asOptInt(session.requestDecision(id, "self_kong",
        Map("validTiles" -> valid.toList.sorted),
        coach(curState, None, _.selfKong, keys)))
        .filter(valid.contains).map(Tile.fromValue)
    }

    override def decideKong(tile: Tile, curState: CurState): Boolean =
      asBool(session.requestDecision(id, "kong",
        Map("tile" -> tile.toTileValue, "fromSeat" -> curState.curPlayerId),
        coach(curState, Some(tile.toTileValue), _.kong, binaryKeys)))

    override def decidePong(tile: Tile, curState: CurState): Boolean =
      asBool(session.requestDecision(id, "pong",
        Map("tile" -> tile.toTileValue, "fromSeat" -> curState.curPlayerId),
        coach(curState, Some(tile.toTileValue), _.pong, binaryKeys)))

    override def decideChow(tile: Tile, positions: Set[ChowPosition], curState: CurState): Option[ChowPosition] = {
      val validIds = positions.map(_.id)
      // chow head: logit 0 = pass, logits 1..3 = LEFT/MIDDLE/RIGHT (id + 1)
      val keys = ("pass" -> 0) :: validIds.toList.sorted.map(p => p.toString -> (p + 1))
      asOptInt(session.requestDecision(id, "chow",
        Map("tile" -> tile.toTileValue, "positions" -> validIds.toList.sorted,
          "fromSeat" -> curState.curPlayerId),
        coach(curState, Some(tile.toTileValue), _.chow, keys)))
        .filter(validIds.contains).map(ChowPosition(_))
    }

    override def decideDiscard(curState: CurState): Tile = {
      val valid = hand.dynamicTiles.map(_.toTileValue)
      val keys = valid.distinct.sorted.map(t => t.toString -> t)
      val answer = asOptInt(session.requestDecision(id, "discard",
        Map("validTiles" -> valid.distinct.sorted),
        coach(curState, None, _.discard, keys)))
      answer.filter(valid.contains).map(Tile.fromValue)
        .getOrElse(hand.dynamicTiles.head) // defensive: never crash the game on a bad action
    }

    override def name: String = "Remote"
  }

  // ── stdin dispatch loop ────────────────────────────────────────────────────

  while (true) {
    val line = scala.io.StdIn.readLine()
    if (line == null) sys.exit(0)
    if (line.trim.nonEmpty) {
      Try(parse(line).values.asInstanceOf[Map[String, Any]]) match {
        case Failure(ex) =>
          send(Map("type" -> "error", "message" -> s"bad json: ${ex.getMessage}"))
        case Success(cmd) =>
          val gameId = cmd.getOrElse("gameId", "").toString
          cmd.getOrElse("cmd", "") match {
            case "ping" => send(Map("type" -> "pong"))

            case "new_game" =>
              val seats = cmd.get("seats") match {
                case Some(l: List[_]) => l.map(_.toString)
                case _                => List("remote", "chicken", "chicken", "chicken")
              }
              val seed: Option[Long] = cmd.get("seed").flatMap {
                case v: BigInt => Some(v.toLong)
                case v: Int    => Some(v.toLong)
                case v: Long   => Some(v)
                case v: Double => Some(v.toLong)
                case _         => None
              }
              val dealerSeat = cmd.get("dealerSeat").flatMap {
                case v: BigInt => Some(v.toInt)
                case v: Int    => Some(v)
                case _         => None
              }.getOrElse(0)

              if (seats.size != 4) {
                send(Map("type" -> "error", "gameId" -> gameId, "message" -> "seats must have 4 entries"))
              } else if (sessions.containsKey(gameId)) {
                send(Map("type" -> "error", "gameId" -> gameId, "message" -> "gameId already running"))
              } else {
                Try(new GameSession(gameId, seats, seed, dealerSeat)) match {
                  case Success(session) =>
                    sessions.put(gameId, session)
                    send(Map("type" -> "game_started", "gameId" -> gameId,
                      "dealerSeat" -> dealerSeat, "snapshot" -> session.snapshot()))
                    val t = new Thread(new Runnable { override def run(): Unit = session.run() },
                      s"game-$gameId")
                    t.setDaemon(true)
                    t.start()
                  case Failure(ex) =>
                    send(Map("type" -> "error", "gameId" -> gameId,
                      "message" -> s"failed to create game: ${ex.getMessage}"))
                }
              }

            case "action" =>
              val session = sessions.get(gameId)
              if (session == null) {
                send(Map("type" -> "error", "gameId" -> gameId, "message" -> "unknown gameId"))
              } else {
                val requestId = cmd.get("requestId") match {
                  case Some(v: BigInt) => v.toInt
                  case Some(v: Int)    => v
                  case _               => -1
                }
                session.deliver(requestId, cmd.getOrElse("action", null))
              }

            case "abort_game" =>
              val session = sessions.remove(gameId)
              if (session != null) session.abort()

            case other =>
              send(Map("type" -> "error", "message" -> s"unknown cmd: $other"))
          }
      }
    }
  }
}
