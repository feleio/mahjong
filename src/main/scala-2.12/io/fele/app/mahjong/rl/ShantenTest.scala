package io.fele.app.mahjong.rl

import scala.util.Random

/** Sanity tests + benchmark for Shanten. Run:
 *  java -cp <jar> io.fele.app.mahjong.rl.ShantenTest
 */
object ShantenTest extends App {

  private def counts(tiles: Int*): Array[Int] = {
    val c = Array.fill(34)(0)
    tiles.foreach(t => c(t) += 1)
    c
  }

  private var failures = 0
  private def check(name: String, got: Int, want: Int): Unit = {
    val ok = got == want
    if (!ok) failures += 1
    println(f"  [${if (ok) "PASS" else "FAIL"}] $name%-55s got=$got want=$want")
  }

  // Suit A = 0-8 (1-9), suit B = 9-17, suit C = 18-26, honors = 27-33

  // Win (14 tiles): 111 222 333 444 55
  check("win: 111222333444 55",
    Shanten.shanten(counts(0,0,0, 1,1,1, 2,2,2, 3,3,3, 4,4), 0), -1)

  // Tenpai (13): 111 222 333 44 67  (waiting 5/8)
  check("tenpai: 111222333 44 67",
    Shanten.shanten(counts(0,0,0, 1,1,1, 2,2,2, 3,3, 5,6), 0), 0)

  // 1-shanten (13): 111 222 333 44 5 9
  check("1-shanten: 111222333 44 5 9",
    Shanten.shanten(counts(0,0,0, 1,1,1, 2,2,2, 3,3, 4,8), 0), 1)

  // Tenpai on pair (13): 111 222 333 444 9  (waiting 9)
  check("tenpai single wait: 111222333444 9",
    Shanten.shanten(counts(0,0,0, 1,1,1, 2,2,2, 3,3,3, 8), 0), 0)

  // Junk (13): 13 isolated tiles, no pairs/partials → max standard shanten = 8
  check("junk terminals+honors",
    Shanten.shanten(counts(0,8, 9,17, 18,26, 27,28,29,30,31,32,33), 0), 8)

  // Fixed groups: F=2, dynamic 111 22 45 (7 tiles) → tenpai (3/6 completes 45)
  check("F=2 tenpai: 111 22 45",
    Shanten.shanten(counts(0,0,0, 1,1, 3,4), 2), 0)

  // F=3, dynamic 11 2 3 → tenpai? sets: none; pair 11; partial 23 → 2*(1-0)-1-1=0
  check("F=3 tenpai: 11 23",
    Shanten.shanten(counts(0,0, 1,2), 3), 0)

  // F=4, dynamic 5 (1 tile) → pair wait, tenpai
  check("F=4 pair wait: 5",
    Shanten.shanten(counts(4), 4), 0)

  // F=4, dynamic 55 → win
  check("F=4 win: 55",
    Shanten.shanten(counts(4,4), 4), -1)

  // Two pairs, F=3: 22 33 → tenpai (either pongs)
  check("F=3 shanpon tenpai: 22 33",
    Shanten.shanten(counts(1,1, 2,2), 3), 0)

  // improveTiles: tenpai hand accepts exactly 5 and 8 (suit A idx 4,7)
  val unseenAll = Array.fill(34)(4)
  val (sh0, imp0) = Shanten.improveTiles(
    counts(0,0,0, 1,1,1, 2,2,2, 3,3, 5,6), 0, unseenAll)
  check("improveTiles base shanten", sh0, 0)
  check("improveTiles accepts idx4 (5)", if (imp0(4) > 0) 1 else 0, 1)
  check("improveTiles accepts idx7 (8)", if (imp0(7) > 0) 1 else 0, 1)
  check("improveTiles count", imp0.count(_ > 0), 2)

  // discardFeatures: 14-tile hand 111222333 44 5 67: discarding 5 keeps tenpai
  val (shA, ukA) = Shanten.discardFeatures(
    counts(0,0,0, 1,1,1, 2,2,2, 3,3, 4, 5,6), 0, unseenAll)
  check("discard 5 (idx4) → tenpai", shA(4), 0)
  // discarding 7 leaves 56 partial + pair 44 → still tenpai (waits 4/7)
  check("discard 7 (idx6) → tenpai via 56 partial", shA(6), 0)
  check("not-in-hand marker", shA(20), 9)
  check("ukeire positive after discarding 5", if (ukA(4) > 0) 1 else 0, 1)

  // ── Benchmark ────────────────────────────────────────────────────────────
  val rng = new Random(42)
  val hands = (0 until 200).map { _ =>
    val pool = rng.shuffle((0 until 34).flatMap(t => Seq.fill(4)(t)).toList)
    val c = Array.fill(34)(0)
    pool.take(14).foreach(t => c(t) += 1)
    c
  }
  // Warmup
  hands.take(50).foreach(h => Shanten.discardFeatures(h, 0, unseenAll))
  val t0 = System.nanoTime()
  hands.foreach(h => Shanten.discardFeatures(h, 0, unseenAll))
  val perDecisionMs = (System.nanoTime() - t0) / 1e6 / hands.size
  println(f"\n  benchmark: discardFeatures (full 14-tile decision) = $perDecisionMs%.2f ms")

  println(if (failures == 0) "\nALL PASS" else s"\n$failures FAILURES")
  if (failures > 0) sys.exit(1)
}
