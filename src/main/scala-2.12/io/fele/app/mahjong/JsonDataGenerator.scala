package io.fele.app.mahjong

import com.typesafe.scalalogging.Logger
import io.fele.app.mahjong.JsonSerializer.serializers
import io.fele.app.mahjong.player.Player
import org.json4s.JsonAST.JString
import org.json4s.JsonDSL._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import org.json4s.{CustomSerializer, Extraction, ShortTypeHints}
import java.io._

import scala.collection.mutable

// we don't need deserializer at the moment
object JsonSerializer {
  import Extraction._
  implicit val formats = serializers
  implicit val config = new Config()
  class TileSerializer extends CustomSerializer[Tile](format => ( Map.empty, {
    case t: Tile => JString(t.toString)
  }))

  class HandSerializer extends CustomSerializer[Hand](format => ( Map.empty, {
    case h: Hand => ("tiles" -> decompose(h.dynamicTiles)) ~
      ("groups" -> decompose(h.fixedTileGroups))
  }))

  class PlayerSerializer extends CustomSerializer[Player](format => ( Map.empty, {
    case p: Player => ("id" -> p.id) ~ ("hand" -> decompose(p.hand))
  }))

  class TileDrawerSerializer extends CustomSerializer[TileDrawer](format => ( Map.empty, {
    case d: TileDrawer => decompose(d.remainingTiles)
  }))

  class WinnersInfoSerializer extends CustomSerializer[WinnersInfo](format => ( Map.empty, {
    case info: WinnersInfo => ("winners" -> decompose(info.winners)) ~
      ("loserId" -> decompose(info.loserId)) ~
      ("winningTile" -> decompose(info.winningTile)) ~
      ("isSelfWin" -> decompose(info.isSelfWin)) ~
      ("winnersBalance" -> decompose(info.winnersBalance))
  }))


  def serializers = Serialization.formats(ShortTypeHints(
    List(classOf[StartEvent],
      classOf[ResumeEvent],
      classOf[DiscardEvent],
      classOf[DiscardEvent],
      classOf[KongEvent],
      classOf[PongEvent],
      classOf[ChowEvent],
      classOf[EndEvent],
      classOf[DrawEvent],
      classOf[KongGroup],
      classOf[PongGroup],
      classOf[ChowGroup]
    ))) + new TileSerializer() + new HandSerializer() + new PlayerSerializer() + new TileDrawerSerializer() + new WinnersInfoSerializer()
}

class JsonDataGenerator(val gameState: GameState, val printWriter: PrintWriter) extends GameLogger {
  implicit val formats = JsonSerializer.serializers
  val logger = Logger("JsonDataGenerator")
  private var str = ""
  private var gameSnapShotStrs = mutable.MutableList.empty[String]

  override def start(): Unit = {
    gameSnapShotStrs = mutable.MutableList.empty[String]
    gameSnapShotStrs += write(GameSnapShot(StartEvent(), gameState))
  }

  override def resume(): Unit = {
    gameSnapShotStrs = mutable.MutableList.empty[String]
    gameSnapShotStrs += write(GameSnapShot(ResumeEvent(), gameState))
  }

  override def discard(discardEvent: DiscardEvent): Unit = {
    gameSnapShotStrs += write(GameSnapShot(discardEvent, gameState))
  }

  override def kong(kongEvent: KongEvent): Unit = {
    gameSnapShotStrs += write(GameSnapShot(kongEvent, gameState))
  }

  override def pong(pongEvent: PongEvent): Unit = {
    gameSnapShotStrs += write(GameSnapShot(pongEvent, gameState))
  }

  override def chow(chowEvent: ChowEvent): Unit = {
    gameSnapShotStrs += write(GameSnapShot(chowEvent, gameState))
  }

  override def end(endEvent: EndEvent): Unit = {
    gameSnapShotStrs += write(GameSnapShot(endEvent, gameState))
    str = SerializeGameLog(endEvent)
    writeToFile(str)
  }

  override def draw(drawEvent: DrawEvent): Unit = {
    gameSnapShotStrs += write(GameSnapShot(drawEvent, gameState))
  }

  private def SerializeGameLog(endEvent: EndEvent) = {
    s"""{"history":[${gameSnapShotStrs.mkString(",")}],"gameResult":${write(endEvent)}}"""
  }

  private def writeToFile(s: String) = printWriter.append(s)
}
