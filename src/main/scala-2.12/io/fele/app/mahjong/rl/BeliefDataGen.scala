package io.fele.app.mahjong.rl

import java.io.{BufferedWriter, FileWriter}

import io.fele.app.mahjong._
import io.fele.app.mahjong.player.{Chicken, FirstFelix, Player}
import org.json4s._
import org.json4s.native.Serialization
import org.json4s.native.Serialization.write

import scala.util.{Random, Try}

/**
 * BeliefDataGen — training data for the opponent hand-belief model.
 *
 * Plays full games with a fixed opponent policy (default FirstFelix, the policy
 * our search uses in rollouts) and, at every discard, records one row from the
 * PUBLIC view of the discarding player plus their TRUE hidden hand as the label:
 *
 *   { "pid":         seat id,
 *     "hand_size":   dynamic tiles held (13 - 3*melds),
 *     "remaining":   tiles left in the wall,
 *     "disc":        [34] this player's own discards so far (incl. this tile),
 *     "pong":[34], "kong":[34], "chow":[34]  this player's melds,
 *     "hand":        [34] TRUE dynamic hand counts (the label to predict) }
 *
 * The tile just discarded is already out of hand at the logger callback, so
 * `hand` is the post-discard 13-tile state — exactly what the search must guess
 * for an opponent between their turns.
 *
 * Usage:
 *   java -Drl.beliefout=belief.jsonl -Drl.beliefgames=20000 \
 *        -Drl.beliefopp=firstfelix -Drl.beliefseed=0 \
 *        -cp <jar> io.fele.app.mahjong.rl.BeliefDataGen
 */
object BeliefDataGen extends App {

  implicit val formats: Formats = Serialization.formats(NoTypeHints)
  implicit val config: Config = new Config()

  private val outPath = System.getProperty("rl.beliefout", "belief.jsonl")
  private val nGames  = Integer.getInteger("rl.beliefgames", 10000)
  private val oppType = System.getProperty("rl.beliefopp", "firstfelix").toLowerCase
  private val seed0   = Integer.getInteger("rl.beliefseed", 0).toLong

  // NN opponent policy (loaded lazily from -Drl.beliefnnmodel=<path.onnx>), so the
  // belief model can be trained against the hand-discard patterns of a strong net
  // rather than FirstFelix — the regime the league teacher actually faces.
  private lazy val nnService: OnnxPolicyService = {
    val path = System.getProperty("rl.beliefnnmodel")
    require(path != null, "beliefopp 'nn' needs -Drl.beliefnnmodel=<path.onnx>")
    new OnnxPolicyService(path)
  }

  private def counts(tiles: Seq[Tile]): Array[Int] = {
    val a = Array.fill(34)(0)
    tiles.foreach(t => a(t.toTileValue) += 1)
    a
  }

  private def meldPlanes(groups: List[TileGroup]): (Array[Int], Array[Int], Array[Int]) = {
    val pong = Array.fill(34)(0); val kong = Array.fill(34)(0); val chow = Array.fill(34)(0)
    groups.foreach {
      case PongGroup(t)  => pong(t.toTileValue) = 1
      case KongGroup(t)  => kong(t.toTileValue) = 1
      case ChowGroup(ts) => ts.foreach(t => chow(t.toTileValue) = 1)
    }
    (pong, kong, chow)
  }

  /** GameLogger that snapshots a belief-training row at every discard. */
  private class BeliefLogger(gameState: GameState, out: BufferedWriter) extends GameLogger {
    override def start(): Unit = ()
    override def resume(): Unit = ()
    override def kong(e: KongEvent): Unit = ()
    override def pong(e: PongEvent): Unit = ()
    override def chow(e: ChowEvent): Unit = ()
    override def draw(e: DrawEvent): Unit = ()
    override def end(e: EndEvent): Unit = ()

    override def discard(e: DiscardEvent): Unit = {
      val p = gameState.players(e.playerId)
      val dynamic = p.hand.dynamicTiles           // already post-discard (13-3*melds)
      // this player's own discards so far + the tile just thrown
      val myDisc = counts(
        gameState.discards.filter(_.playerId == e.playerId).map(_.tile))
      myDisc(e.tile.toTileValue) += 1
      val (pong, kong, chow) = meldPlanes(p.hand.fixedTileGroups)
      val row = Map(
        "pid"       -> e.playerId,
        "hand_size" -> dynamic.size,
        "remaining" -> gameState.drawer.remainingTiles.size,
        "disc"      -> myDisc.toList,
        "pong"      -> pong.toList,
        "kong"      -> kong.toList,
        "chow"      -> chow.toList,
        "hand"      -> counts(dynamic).toList
      )
      out.write(write(row)); out.write("\n")
    }
  }

  private def makePlayer(id: Int, tiles: List[Tile]): Player = oppType match {
    case "chicken" => new Chicken(id, tiles)
    case "nn"      => new NNPlayer(id, tiles, List.empty[TileGroup], nnService)
    case _         => new FirstFelix(id, tiles, 5)
  }

  val out = new BufferedWriter(new FileWriter(outPath))
  var rows = 0L
  var gi = 0
  while (gi < nGames) {
    val seed = seed0 + gi
    val drawer: TileDrawer = new RandomTileDrawer(Some(seed))
    val players: List[Player] = (0 to 3).map(i => makePlayer(i, drawer.popHand())).toList
    val state = GameState(players, None, Nil, (seed % 4).toInt, drawer)
    val logger: GameLogger = new BeliefLogger(state, out)
    val flow = new FlowImpl(state)(logger)
    Try(flow.start())
    gi += 1
    if (gi % 2000 == 0) { out.flush(); System.err.println(s"[belief] $gi/$nGames games") }
  }
  out.flush(); out.close()
  System.err.println(s"[belief] DONE $nGames games -> $outPath")
}
