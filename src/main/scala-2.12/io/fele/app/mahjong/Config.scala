package io.fele.app.mahjong

import com.typesafe.config.ConfigFactory
import scala.collection.JavaConverters._

/**
  * Created by felix.ling on 04/01/2017.
  */
class Config(configResource: Option[String] = None) {
  private val c = if (configResource.isDefined)
    ConfigFactory.load(configResource.get)
  else
    ConfigFactory.load()

  val isPauseWhenLog: Boolean = c.getBoolean("isPauseWhenLog")
  val minScore: Int = c.getInt("minScore")
  val maxScore: Int = c.getInt("maxScore")
  val scoreMap = c.getObject("scoreMap").asScala.mapValues(_.unwrapped().asInstanceOf[Int])
}
