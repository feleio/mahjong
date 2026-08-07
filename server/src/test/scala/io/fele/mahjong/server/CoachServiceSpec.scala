package io.fele.mahjong.server

import io.fele.app.mahjong.{Config => EngineConfig, _}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Coach hints from the real champion (and danger) ONNX sessions. Skipped
  * when the model files are absent (e.g. a checkout without checkpoints). */
class CoachServiceSpec extends AnyFlatSpec with Matchers {

  implicit val engineConfig: EngineConfig = new EngineConfig()

  private def sampleState(): CurState = {
    // 14 concealed tiles (discard turn) from a fixed seed, plus a short
    // discard history so the v4 obs has a discard sequence to encode.
    val drawer = new RandomTileDrawer(Some(99L))
    val mine   = drawer.popHand() ++ drawer.pop().toList
    val d1     = drawer.pop().get
    val d2     = drawer.pop().get
    CurState(
      myInfo        = PrivateState(mine, Nil),
      otherInfos    = List(PublicState(Nil), PublicState(Nil), PublicState(Nil)),
      discards      = List(DiscardInfo(2, d2), DiscardInfo(1, d1)),
      curPlayerId   = 0,
      remainTileNum = drawer.remainingTiles.size
    )
  }

  "CoachService.hint" should "return a probability distribution over the legal discards" in {
    assume(ChampionService.unavailableReason.isEmpty,
      s"champion model unavailable: ${ChampionService.unavailableReason.getOrElse("")}")
    val cs   = sampleState()
    val keys = cs.myInfo.tiles.map(_.toTileValue).distinct.sorted.map(v => CoachService.tileWire(v) -> v)
    val hint = CoachService.hint(0, cs, None, _.discard, keys)

    hint shouldBe defined
    val h = hint.get
    h.probs.keySet shouldBe keys.map(_._1).toSet
    h.probs.values.sum shouldBe 1.0 +- 1e-6
    h.probs.values.foreach(p => p should (be >= 0.0 and be <= 1.0))
    h.probs(h.top) shouldBe h.probs.values.max
  }

  it should "attach danger fields iff the v4 danger model is loaded" in {
    assume(ChampionService.unavailableReason.isEmpty, "champion model unavailable")
    val cs   = sampleState()
    val keys = cs.myInfo.tiles.map(_.toTileValue).distinct.sorted.map(v => CoachService.tileWire(v) -> v)
    val h    = CoachService.hint(0, cs, None, _.discard, keys).get

    if (CoachService.dangerService.isDefined) {
      val tenpai = h.oppTenpai.get
      tenpai.map(_.seat).sorted shouldBe List(1, 2, 3) // my seat 0 → opponents 1,2,3
      tenpai.foreach(t => t.p should (be >= 0.0 and be <= 1.0))
      val danger = h.dangerByTile.get
      danger should have size 34
      danger.values.foreach(d => d should (be >= 0.0 and be <= 1.0))
    } else {
      h.oppTenpai shouldBe None
      h.dangerByTile shouldBe None
    }
  }

  it should "answer binary claim decisions with pass/accept" in {
    assume(ChampionService.unavailableReason.isEmpty, "champion model unavailable")
    val cs = sampleState()
    val contextTile = cs.discards.head.tile.toTileValue
    val h = CoachService.hint(0, cs, Some(contextTile), _.pong, CoachService.binaryKeys)
    h shouldBe defined
    h.get.probs.keySet shouldBe Set("pass", "accept")
    h.get.probs.values.sum shouldBe 1.0 +- 1e-6
  }
}
