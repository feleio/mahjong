package io.fele.mahjong.server

import io.fele.app.mahjong._
import io.fele.mahjong.server.Models.CoachHint
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** The explainer must only say things it can ground in engine facts (#44). */
class DecisionExplainerSpec extends AnyFlatSpec with Matchers {

  implicit val engineConfig: Config = new Config()

  private def tile(v: String): Tile = Models.tileFromWire(v).toOption.get

  private def state(hand: List[String], discards: List[(Int, String)] = Nil): CurState =
    CurState(
      myInfo        = PrivateState(hand.map(tile), Nil),
      otherInfos    = List(PublicState(Nil), PublicState(Nil), PublicState(Nil)),
      discards      = discards.reverse.map { case (p, t) => DiscardInfo(p, tile(t)) },
      curPlayerId   = 0,
      remainTileNum = 60
    )

  private def hint(probs: Map[String, Double], danger: Option[Map[String, Double]] = None): CoachHint =
    CoachHint(probs, probs.maxBy(_._2)._1, 0.0, None, danger)

  "explain" should "say nothing when the player agreed with the champion" in {
    val cs = state(List("D1", "D2", "D3", "B1", "B2", "B3", "C1", "C2", "C3", "D5", "D5", "B7", "B8", "HW_E"))
    DecisionExplainer.explain("discard", cs, "HW_E", "HW_E", hint(Map("HW_E" -> 1.0))) shouldBe None
  }

  it should "call out shape when the champion's discard keeps the hand closer to ready" in {
    // Complete sets plus a pair and one isolated honour: discarding a set tile
    // costs shanten, discarding the honour does not.
    val cs = state(List("D1", "D2", "D3", "B1", "B2", "B3", "C1", "C2", "C3", "D5", "D5", "B7", "B8", "HW_E"))
    val e = DecisionExplainer.explain("discard", cs, "D1", "HW_E", hint(Map("D1" -> 0.1, "HW_E" -> 0.6)))
    e shouldBe defined
    e.get.bucket shouldBe "shape"
    // spelled out, not wire codes — the player never sees "HW_E" anywhere else
    e.get.text should include("East wind")
    e.get.text should include("1 dot")
    e.get.text should not include "HW_E"
    // and it must advise DISCARDING the champion's tile, never keeping it:
    // shAfter(t) is the shanten after discarding t, so "keeping" inverts the advice
    e.get.text should startWith("Discarding East wind")
    e.get.text should not include "Keeping"
  }

  it should "spell tiles out in plain language" in {
    DecisionExplainer.tileName("HW_E") shouldBe "East wind"
    DecisionExplainer.tileName("HD_R") shouldBe "Red dragon"
    DecisionExplainer.tileName("D1")   shouldBe "1 dot"
    DecisionExplainer.tileName("D5")   shouldBe "5 dots"
    DecisionExplainer.tileName("B9")   shouldBe "9 bamboo"
    DecisionExplainer.tileName("C1")   shouldBe "1 character"
    DecisionExplainer.tileName("C7")   shouldBe "7 characters"
  }

  it should "fall back to safety when shape is equal but one tile is far more dangerous" in {
    // Two interchangeable isolated honours: identical shape, different danger.
    val cs = state(List("D1", "D2", "D3", "B1", "B2", "B3", "C1", "C2", "C3", "D5", "D5", "B7", "HW_E", "HW_S"))
    val danger = Map("HW_E" -> 0.30, "HW_S" -> 0.02)
    val e = DecisionExplainer.explain("discard", cs, "HW_E", "HW_S",
      hint(Map("HW_E" -> 0.2, "HW_S" -> 0.5), Some(danger)))
    e shouldBe defined
    e.get.bucket shouldBe "safety"
    e.get.text should include("dangerous")
  }

  it should "never invent a reason for a tile the player does not hold" in {
    val cs = state(List("D1", "D2", "D3", "B1", "B2", "B3", "C1", "C2", "C3", "D5", "D5", "B7", "B8", "HW_E"))
    DecisionExplainer.explain("discard", cs, "HD_R", "HW_E", hint(Map("HW_E" -> 1.0))) shouldBe None
  }

  it should "explain claim decisions in both directions" in {
    val cs = state(List("D1", "D1", "D3", "B1", "B2", "B3", "C1", "C2", "C3", "D5", "D5", "B7", "B8"))
    val declined = DecisionExplainer.explain("pong", cs, "accept", "pass", hint(Map("pass" -> 0.9, "accept" -> 0.1)))
    declined.get.bucket shouldBe "accept"
    declined.get.text should include("declined")

    val taken = DecisionExplainer.explain("pong", cs, "pass", "accept", hint(Map("pass" -> 0.1, "accept" -> 0.9)))
    taken.get.bucket shouldBe "accept"
    taken.get.text should include("taken")
  }
}
