package io.fele.mahjong.server

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto._
import io.fele.app.mahjong.ChowPosition.ChowPosition
import io.fele.app.mahjong.{ChowPosition, Tile, TileGroup, TileValue}
import io.fele.app.mahjong.{KongGroup, PongGroup, ChowGroup}

import java.time.Instant
import java.util.UUID

object Models {

  /* ---------- Identifiers ---------- */
  type RoomId   = String
  type PlayerId = String

  /* ---------- Seats ---------- */

  sealed trait SeatKind
  object SeatKind {
    case object Human                extends SeatKind
    case object AiChicken            extends SeatKind
    case object AiRandom             extends SeatKind
    case object AiFirstFelix         extends SeatKind
    case object AiThreePointChicken  extends SeatKind
    case object AiChampion           extends SeatKind   // champion policy net (greedy NNPlayer)
    case object Open                 extends SeatKind   // unfilled human slot waiting for someone

    val all: List[SeatKind] = List(Human, AiChicken, AiRandom, AiFirstFelix, AiThreePointChicken, AiChampion, Open)

    def fromString(s: String): Option[SeatKind] = s.toLowerCase match {
      case "human"               => Some(Human)
      case "open"                => Some(Open)
      case "ai_chicken"          => Some(AiChicken)
      case "ai_random"           => Some(AiRandom)
      case "ai_first_felix"      => Some(AiFirstFelix)
      case "ai_3point_chicken"   => Some(AiThreePointChicken)
      case "ai_champion"         => Some(AiChampion)
      case _                     => None
    }

    def toWire(s: SeatKind): String = s match {
      case Human                => "human"
      case Open                 => "open"
      case AiChicken            => "ai_chicken"
      case AiRandom             => "ai_random"
      case AiFirstFelix         => "ai_first_felix"
      case AiThreePointChicken  => "ai_3point_chicken"
      case AiChampion           => "ai_champion"
    }

    implicit val enc: Encoder[SeatKind] = Encoder.encodeString.contramap(toWire)
    implicit val dec: Decoder[SeatKind] = Decoder.decodeString.emap(s => fromString(s).toRight(s"unknown seat kind '$s'"))
  }

  case class Seat(
    index:    Int,                    // 0..3
    kind:     SeatKind,
    playerId: Option[PlayerId],       // set for Human seats once a user occupies it
    name:     String                  // display name
  )
  object Seat {
    implicit val enc: Encoder[Seat] = deriveEncoder
    implicit val dec: Decoder[Seat] = deriveDecoder
  }

  /* ---------- Room ---------- */

  sealed trait RoomStatus
  object RoomStatus {
    case object Waiting  extends RoomStatus
    case object Playing  extends RoomStatus
    case object Finished extends RoomStatus

    def toWire(s: RoomStatus): String = s match {
      case Waiting  => "waiting"
      case Playing  => "playing"
      case Finished => "finished"
    }
    def fromString(s: String): Option[RoomStatus] = s match {
      case "waiting"  => Some(Waiting)
      case "playing"  => Some(Playing)
      case "finished" => Some(Finished)
      case _          => None
    }
    implicit val enc: Encoder[RoomStatus] = Encoder.encodeString.contramap(toWire)
    implicit val dec: Decoder[RoomStatus] = Decoder.decodeString.emap(s => fromString(s).toRight(s"unknown room status '$s'"))
  }

  case class Room(
    id:          RoomId,
    name:        String,
    hostId:      PlayerId,
    seats:       List[Seat],          // exactly 4 entries, sorted by index
    status:      RoomStatus,
    createdAt:   Instant,
    code:        String = "",         // short join code people can read out loud
    balances:    List[Int] = List(0, 0, 0, 0), // cumulative money per seat here
    gamesPlayed: Int = 0
  ) {
    def isFull: Boolean = seats.forall(s => s.kind != SeatKind.Open)
    def humanSeats: List[Seat] = seats.filter(_.kind == SeatKind.Human)
  }
  object Room {
    implicit val enc: Encoder[Room] = deriveEncoder
    implicit val dec: Decoder[Room] = deriveDecoder

    def newId(): RoomId = UUID.randomUUID().toString

    /** Unambiguous alphabet: no O/0, I/1, S/5 — these get read out loud. */
    private val CodeAlphabet = "ABCDEFGHJKLMNPQRTUVWXY2346789"
    def newCode(): String =
      (1 to 6).map(_ => CodeAlphabet(scala.util.Random.nextInt(CodeAlphabet.length))).mkString
  }

  /* ---------- Public wire views (issue #51) ----------
   *
   * `hostId` and a seat's `playerId` are the only credentials this server has:
   * holding one lets you run that room or play that seat. They are handed to
   * their owner once, at create/join time, and must never appear in a payload
   * anyone else can read — so every route and socket frame serializes these
   * id-free projections instead of `Room`/`Seat`.
   *
   * `hostSeat` replaces `hostId` for the UI's "who is the host" marker: it is
   * derived from the room, not a secret, and cannot be replayed as a
   * credential. */

  case class SeatView(index: Int, kind: SeatKind, name: String, occupied: Boolean)
  object SeatView {
    implicit val enc: Encoder[SeatView] = deriveEncoder
    implicit val dec: Decoder[SeatView] = deriveDecoder

    def of(s: Seat): SeatView = SeatView(s.index, s.kind, s.name, s.playerId.isDefined)
  }

  case class RoomView(
    id:          RoomId,
    name:        String,
    seats:       List[SeatView],
    status:      RoomStatus,
    createdAt:   Instant,
    code:        String,
    balances:    List[Int],
    gamesPlayed: Int,
    hostSeat:    Int
  )
  object RoomView {
    implicit val enc: Encoder[RoomView] = deriveEncoder
    implicit val dec: Decoder[RoomView] = deriveDecoder

    def of(r: Room): RoomView = RoomView(
      id          = r.id,
      name        = r.name,
      seats       = r.seats.map(SeatView.of),
      status      = r.status,
      createdAt   = r.createdAt,
      code        = r.code,
      balances    = r.balances,
      gamesPlayed = r.gamesPlayed,
      // the host holds hostId; seat 0 is the host's seat and cannot be
      // reassigned (RoomManager.setSeatKind), so the lookup has a sane default
      hostSeat    = r.seats.find(_.playerId.contains(r.hostId)).map(_.index).getOrElse(0)
    )
  }

  /* ---------- Tile JSON ---------- */
  // We expose tiles as their TileValue name, e.g. "D5", "B9", "HW_E", "HD_R".

  def tileToWire(t: Tile): String = TileValue(t.toTileValue).toString

  def tileFromWire(s: String): Either[String, Tile] =
    scala.util.Try(TileValue.withName(s)).toOption
      .map(v => Tile.fromValue(v.id))
      .toRight(s"unknown tile '$s'")

  implicit val tileEnc: Encoder[Tile] = Encoder.encodeString.contramap(tileToWire)
  implicit val tileDec: Decoder[Tile] = Decoder.decodeString.emap(tileFromWire)

  case class TileGroupWire(kind: String, tiles: List[String])
  object TileGroupWire {
    implicit val enc: Encoder[TileGroupWire] = deriveEncoder
    implicit val dec: Decoder[TileGroupWire] = deriveDecoder

    def from(g: TileGroup): TileGroupWire = g match {
      case KongGroup(t)  => TileGroupWire("kong", List.fill(4)(tileToWire(t)))
      case PongGroup(t)  => TileGroupWire("pong", List.fill(3)(tileToWire(t)))
      case ChowGroup(ts) => TileGroupWire("chow", ts.toList.sortBy(_.toTileValue).map(tileToWire))
    }
  }

  /* ---------- Chow position JSON ---------- */
  implicit val chowEnc: Encoder[ChowPosition] = Encoder.encodeString.contramap(_.toString)
  implicit val chowDec: Decoder[ChowPosition] = Decoder.decodeString.emap { s =>
    scala.util.Try(ChowPosition.withName(s.toUpperCase)).toOption.toRight(s"unknown chow position '$s'")
  }

  /* ---------- Public game snapshot pushed to clients ---------- */

  case class PlayerView(
    seat:        Int,
    name:        String,
    kind:        SeatKind,
    fixedGroups: List[TileGroupWire],
    handCount:   Int,                  // for opponents we expose count only
    handTiles:   Option[List[String]]  // present only for the receiving player ("you")
  )
  object PlayerView { implicit val enc: Encoder[PlayerView] = deriveEncoder }

  case class DiscardView(seat: Int, tile: String)
  object DiscardView { implicit val enc: Encoder[DiscardView] = deriveEncoder }

  case class WinnerView(seat: Int, score: Int)
  object WinnerView { implicit val enc: Encoder[WinnerView] = deriveEncoder }

  /** The last thing that happened, structured rather than as a label, so a
    * client can render an event feed, flash the acting seat, and separate a
    * freshly drawn tile. A draw's `tile` is visible only to the drawer —
    * [[GameRunner.annotateForSeat]] strips it for everyone else. */
  case class EventView(
    kind:     String,          // draw | discard | pong | kong | chow | start | resume | end
    seat:     Option[Int],
    tile:     Option[String],
    fromSeat: Option[Int],     // whose discard was claimed
    position: Option[String]   // chow position
  )
  object EventView { implicit val enc: Encoder[EventView] = deriveEncoder }

  /** Which seat the game is currently blocked on, so everyone else can be
    * shown "waiting for X" instead of a dead table. */
  case class WaitingView(seat: Int, decision: String)
  object WaitingView { implicit val enc: Encoder[WaitingView] = deriveEncoder }

  case class GameSnapshot(
    roomId:        RoomId,
    yourSeat:      Option[Int],
    curPlayer:     Int,
    dealerSeat:    Int,
    remainingTiles: Int,
    players:       List[PlayerView],
    discards:      List[DiscardView],
    lastEvent:     Option[String],
    event:         Option[EventView],          // structured form of lastEvent
    pendingDiscard: Option[DiscardView],       // just discarded and still claimable
    balances:      List[Int],                  // cumulative money per seat in this room
    gamesPlayed:   Int,
    winners:       List[WinnerView],
    winningTile:   Option[String],             // what the win was on
    loserSeat:     Option[Int],                // who fed it (None on a self-draw)
    balanceDelta:  List[Int],                  // money this game moved, per seat
    isFinished:    Boolean,
    selfWin:       Boolean
  )
  object GameSnapshot { implicit val enc: Encoder[GameSnapshot] = deriveEncoder }

  /* ---------- Coach hints (issue #37) ---------- */
  // Champion policy attached to a human seat's prompt. `probs` is keyed by the
  // same action keys the client acts with: tile wire names for discard /
  // self_kong, "pass"/"accept" for the yes-no claims, chow positions for chow
  // (plus "pass"). Danger fields come from the v4 danger-head model when it is
  // loaded; they are display-only relative heat (see docs/COACHING.md).

  case class OppTenpaiWire(seat: Int, p: Double)
  object OppTenpaiWire {
    implicit val enc: Encoder[OppTenpaiWire] = deriveEncoder
  }

  case class CoachHint(
    probs:        Map[String, Double],       // action key -> champion probability (legal actions only)
    top:          String,                    // key with the highest probability
    value:        Double,                    // champion value estimate for this seat
    oppTenpai:    Option[List[OppTenpaiWire]],       // per-opponent p(tenpai), absolute seats
    dangerByTile: Option[Map[String, Double]]        // tile wire name -> deal-in risk (relative heat)
  )
  object CoachHint {
    implicit val enc: Encoder[CoachHint] = deriveEncoder
  }

  /* ---------- Decision-prompt protocol ---------- */
  // The server sends a "prompt" to one player when the engine asks for a decision; the client
  // replies with the matching "action".

  case class Prompt(
    kind:        String,                 // "self_win", "win", "self_kong", "kong", "pong", "chow", "discard"
    seat:        Int,
    tile:        Option[String],         // tile under consideration (winning/kongable/pongable)
    score:       Option[Int],            // for win/self_win
    selfKongTiles: Option[List[String]], // for self_kong
    chowPositions: Option[List[String]], // for chow ("LEFT","MIDDLE","RIGHT")
    handTiles:   Option[List[String]],   // for discard
    coach:       Option[CoachHint] = None,
    timeoutMs:   Option[Long] = Some(WebSocketPlayer.DefaultTimeoutMs), // client-side countdown (#42)
    promptId:    Long = 0L    // echoed back in the action so a late or replayed answer is ignored
  )
  object Prompt { implicit val enc: Encoder[Prompt] = deriveEncoder }

  case class Action(
    kind:     String,                    // mirrors the prompt kind
    yes:      Option[Boolean],           // for boolean prompts (win/self_win/kong/pong)
    tile:     Option[String],            // for discard, self_kong
    chowPos:  Option[String],            // "LEFT"/"MIDDLE"/"RIGHT"
    promptId: Option[Long] = None        // which decision this answers
  )
  object Action { implicit val dec: Decoder[Action] = deriveDecoder }

  /* ---------- Chat ---------- */
  case class ChatMessage(seat: Int, name: String, text: String, ts: Long)
  object ChatMessage { implicit val enc: Encoder[ChatMessage] = deriveEncoder }
}
