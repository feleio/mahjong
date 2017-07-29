package io.fele.app.mahjong.experiment

import java.io.{File, PrintWriter}
import io.fele.app.mahjong.{JsonSerializer, Tile}
import org.json4s.native.Serialization.{read, write}
import scala.io.Source

object Distinct extends App {
  implicit val formats = JsonSerializer.serializers
  val jStr = Source.fromFile("train_v5 copy.json").mkString
  println("parsing")
  val hands = read[List[List[Tile]]](jStr)
  println("de duplicate")
  hands.distinct
  println("finished de duplicate, saving to file")

  private val labelTrainingData = new File("train_v5_dd.json")
  private val printWriter: PrintWriter = new PrintWriter(labelTrainingData)
  printWriter.write(write(hands))
  println("done")
}
