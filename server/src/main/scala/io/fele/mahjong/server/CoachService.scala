package io.fele.mahjong.server

import com.typesafe.config.ConfigFactory
import io.fele.app.mahjong.{CurState, Tile}
import io.fele.app.mahjong.rl.{OnnxPolicyService, PolicyOut, V3Obs, V4Obs}
import io.fele.mahjong.server.Models.{CoachHint, OppTenpaiWire}

import scala.util.{Failure, Success, Try}

/** Champion-as-teacher hints for human seats (issue #37, port of the legacy
  * WebGameServer.coach()).
  *
  * The policy/value hint comes from the shared [[ChampionService]] session;
  * the danger overlay (opponent tenpai + per-tile deal-in heat) comes from an
  * optional second, v4-obs model with the danger heads (issue #23). Every
  * entry point is failure-proof: a hint that cannot be computed is simply
  * omitted — coaching must never touch the game.
  */
object CoachService {

  lazy val dangerModelPath: String = ConfigFactory.load().getString("coach.dangerModelPath")

  /** v4 danger-head session, or None (missing file / load failure / not a v4
    * model). The input-dim check doubles as the "has danger heads" check: only
    * v4 exports (dim 1443) carry them. */
  lazy val dangerService: Option[OnnxPolicyService] = {
    val f = new java.io.File(dangerModelPath)
    if (!f.isFile) {
      println(s"coach: danger model not found at $dangerModelPath — danger overlay disabled (set MAHJONG_DANGER_MODEL)")
      None
    } else Try(new OnnxPolicyService(f.getAbsolutePath)) match {
      case Success(svc) if svc.inputDim == V4Obs.DIM =>
        println(s"coach: danger model loaded ($dangerModelPath)")
        Some(svc)
      case Success(_) =>
        println(s"coach: $dangerModelPath is not a v4 model (input dim != ${V4Obs.DIM}) — danger overlay disabled")
        None
      case Failure(t) =>
        println(s"WARN: coach danger model failed to load ($dangerModelPath): ${t.getMessage}")
        None
    }
  }

  /** One-line status for /api/health. */
  def dangerStatus: String = if (dangerService.isDefined) "ok" else s"unavailable ($dangerModelPath)"

  val binaryKeys: List[(String, Int)] = List("pass" -> 0, "accept" -> 1)

  def tileWire(tileValue: Int): String = Models.tileToWire(Tile.fromValue(tileValue))

  private def softmaxOver(logits: Array[Float], keys: List[(String, Int)]): Map[String, Double] = {
    val mx   = keys.map { case (_, i) => logits(i) }.max
    val exps = keys.map { case (k, i) => k -> math.exp((logits(i) - mx).toDouble) }
    val sum  = exps.map(_._2).sum
    exps.map { case (k, e) => k -> e / sum }.toMap
  }

  private def obsFor(svc: OnnxPolicyService, seat: Int, cs: CurState, contextTile: Option[Int]): Array[Float] =
    if (svc.inputDim == V4Obs.DIM) V4Obs.fromCurState(seat, cs, contextTile)
    else V3Obs.fromCurState(seat, cs, contextTile)

  /** Champion probabilities over this decision's legal actions plus the value
    * estimate; danger fields when the v4 model is loaded. `keys` maps action
    * labels to logit indices of the decision's head (same conventions as the
    * legacy coach: binary pass=0/accept=1, self-kong pass=0/tile t=t+1, chow
    * pass=0/position id+1, discard tile t=t). */
  def hint(
    seat:        Int,
    cs:          CurState,
    contextTile: Option[Int],
    head:        PolicyOut => Array[Float],
    keys:        List[(String, Int)]
  ): Option[CoachHint] =
    ChampionService.service.toOption.flatMap { champ =>
      Try {
        val out   = champ.query(obsFor(champ, seat, cs, contextTile))
        val probs = softmaxOver(head(out), keys)
        val top   = probs.maxBy(_._2)._1
        val (tenpai, dangerTiles) = dangerFields(seat, cs, contextTile)
        CoachHint(probs, top, out.value.toDouble, tenpai, dangerTiles)
      }.toOption
    }

  private def dangerFields(
    seat:        Int,
    cs:          CurState,
    contextTile: Option[Int]
  ): (Option[List[OppTenpaiWire]], Option[Map[String, Double]]) = {
    val fields = dangerService.flatMap { svc =>
      Try {
        val out = svc.query(obsFor(svc, seat, cs, contextTile))
        (out.oppTenpai, out.oppWaits) match {
          case (Some(tp), Some(wt)) =>
            // DangerLabels order: opponent i = seat (myId + 1 + i) % 4
            val tenpai = (0 until 3).map(i => OppTenpaiWire((seat + 1 + i) % 4, tp(i).toDouble)).toList
            val danger = (0 until 34).map { t =>
              tileWire(t) -> (0 until 3).map(i => (tp(i) * wt(i)(t)).toDouble).max
            }.toMap
            Some((tenpai, danger))
          case _ => None
        }
      }.toOption.flatten
    }
    (fields.map(_._1), fields.map(_._2))
  }
}
