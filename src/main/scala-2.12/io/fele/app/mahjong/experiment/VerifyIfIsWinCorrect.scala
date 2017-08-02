package io.fele.app.mahjong.experiment

import io.fele.app.mahjong._
import org.json4s.JsonAST.{JArray, JString}
import org.json4s.native.JsonMethods._

import scala.io.Source

object VerifyIfIsWinCorrect extends App {
  implicit val config = new Config()
  implicit val formats = JsonSerializer.serializers
  val jStr = Source.fromFile("train_v6_nowins.json").mkString
  println("parsing")
  val j = parse(jStr)
  val nowins: List[List[Tile]] = j.children.map{
    case JArray(l) => l.map{
      case JString(tStr) => Tile(TileValue.withName(tStr))
    }
  }

  println("verifying")
  nowins.foreach{x =>
    if(new Hand(x.tail).canWin(x.head).canWin ) println(x)
  }

  //  private val labelTrainingData = new File("train_v4_nowins.json")
  //  private val printWriter: PrintWriter = new PrintWriter(labelTrainingData)
  //  printWriter.write(compact(render(j \ "nowins")))
  println("done")
}
