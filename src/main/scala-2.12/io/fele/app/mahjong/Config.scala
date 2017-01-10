package io.fele.app.mahjong

import com.typesafe.config.ConfigFactory

/**
  * Created by felix.ling on 04/01/2017.
  */
class Config(configResource: Option[String] = None) {
  private val c = if (configResource.isDefined)
    ConfigFactory.load(configResource.get)
  else
    ConfigFactory.load()

  val isPauseWhenLog = c.getBoolean("isPauseWhenLog")
}
