package io.fele.app.mahjong

import com.typesafe.config.ConfigFactory

/**
  * Created by felix.ling on 04/01/2017.
  */
object Config {
  private val c = ConfigFactory.load()

  val isPauseWhenLog = c.getBoolean("isPauseWhenLog")
}
