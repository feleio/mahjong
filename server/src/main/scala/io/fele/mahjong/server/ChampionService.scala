package io.fele.mahjong.server

import com.typesafe.config.ConfigFactory
import io.fele.app.mahjong.rl.OnnxPolicyService

/** Process-wide ONNX session for the champion policy net (net D).
  *
  * One [[OnnxPolicyService]] serves every AiChampion seat in every room: its
  * internal daemon thread batches concurrent queries, and game threads block
  * on their own futures, so sharing is both safe and the intended usage.
  * Loading is lazy — a server without the model file still runs, it just
  * refuses to start games that seat a champion.
  */
object ChampionService {

  lazy val modelPath: String = ConfigFactory.load().getString("champion.modelPath")

  lazy val service: Either[String, OnnxPolicyService] = {
    val f = new java.io.File(modelPath)
    if (!f.isFile) Left(s"champion model not found at $modelPath (set MAHJONG_CHAMPION_MODEL)")
    else
      try Right(new OnnxPolicyService(f.getAbsolutePath))
      catch { case t: Throwable => Left(s"failed to load champion model $modelPath: ${t.getMessage}") }
  }

  /** Error message when the champion cannot play, None when it can. */
  def unavailableReason: Option[String] = service.left.toOption

  def instance: OnnxPolicyService =
    service.fold(e => throw new IllegalStateException(e), identity)
}
