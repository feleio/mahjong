package io.fele.mahjong.server

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global
import io.fele.app.mahjong.{Config => EngineConfig}
import org.http4s._
import org.http4s.implicits._
import org.http4s.server.websocket.WebSocketBuilder2
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.typelevel.ci._

import scala.concurrent.ExecutionContext

/** Origin handling, REST and websocket (issue #52).
  *
  * The websocket half is the one that was missing: a browser opens a socket
  * cross-site with no preflight, so the CORS middleware on the REST routes
  * never sees it. Any page a logged-in player visited could open a socket to
  * their room and play their hand. */
class OriginPolicySpec extends AnyFlatSpec with Matchers {
  import TestDb.xa

  implicit val engineConfig: EngineConfig = new EngineConfig()
  implicit val ec: ExecutionContext = ExecutionContext.global

  private val allowed = "http://192.168.1.42:3000"
  private val evil    = "http://evil.example"
  private val policy  = OriginPolicy.fromConfig(allowed)

  /* ---- parsing ---- */

  "A `*` allowlist" should "accept anything, so a dev checkout just works" in {
    OriginPolicy.fromConfig("*").allowsAll shouldBe true
    OriginPolicy.fromConfig("*").permits(evil) shouldBe true
  }

  it should "win over any other entry rather than half-applying a list" in {
    OriginPolicy.fromConfig("http://a.example, *").allowsAll shouldBe true
  }

  "An empty allowlist" should "be treated as unset, not as deny-everything" in {
    // a blank env var must not silently lock every browser out
    OriginPolicy.fromConfig("").allowsAll shouldBe true
    OriginPolicy.fromConfig("  ,  ").allowsAll shouldBe true
  }

  "A configured allowlist" should "match on the whole origin and reject the rest" in {
    policy.allowsAll shouldBe false
    policy.permits(allowed) shouldBe true
    policy.permits(evil) shouldBe false
  }

  it should "not let a different scheme, host or port through" in {
    policy.permits("https://192.168.1.42:3000") shouldBe false // scheme
    policy.permits("http://192.168.1.43:3000")  shouldBe false // host
    policy.permits("http://192.168.1.42:3001")  shouldBe false // port
    policy.permits("http://192.168.1.42")       shouldBe false // no port
  }

  it should "tolerate the case and trailing slash people write in config" in {
    OriginPolicy.fromConfig("HTTP://Example.COM/").permits("http://example.com") shouldBe true
  }

  it should "accept any of several configured origins" in {
    val multi = OriginPolicy.fromConfig(s"$allowed, http://localhost:3000")
    multi.permits(allowed) shouldBe true
    multi.permits("http://localhost:3000") shouldBe true
    multi.permits(evil) shouldBe false
  }

  /* ---- REST ---- */

  private def restResponse(p: OriginPolicy, origin: Option[String]): Response[IO] =
    Dispatcher.parallel[IO].use { d =>
      for {
        rm  <- RoomManager.create(new RoomRepo(xa), d)
        req  = origin.foldLeft(Request[IO](Method.GET, uri"/api/health"))((r, o) =>
                 r.putHeaders(Header.Raw(ci"Origin", o)))
        res <- Routes.withCors(p)(Routes.routes(rm)).orNotFound.run(req)
      } yield res
    }.unsafeRunSync()

  private def allowOrigin(r: Response[IO]): Option[String] =
    r.headers.get(ci"Access-Control-Allow-Origin").map(_.head.value)

  "The API" should "echo an allowed origin rather than a blanket wildcard" in {
    allowOrigin(restResponse(policy, Some(allowed))) shouldBe Some(allowed)
  }

  it should "send no allow-origin header to a disallowed origin" in {
    // the request still runs; the browser is what enforces the missing header
    allowOrigin(restResponse(policy, Some(evil))) shouldBe None
  }

  it should "still answer non-browser clients that send no Origin at all" in {
    val res = restResponse(policy, None)
    res.status should (be(Status.Ok) or be(Status.ServiceUnavailable)) // depends on DB
  }

  /* ---- websocket ---- */

  private def wsStatus(p: OriginPolicy, origin: Option[String]): Status =
    Dispatcher.parallel[IO].use { d =>
      for {
        rm  <- RoomManager.create(new RoomRepo(xa), d)
        wsb <- WebSocketBuilder2[IO]
        req  = origin.foldLeft(Request[IO](Method.GET, uri"/ws/rooms/does-not-exist"))((r, o) =>
                 r.putHeaders(Header.Raw(ci"Origin", o)))
        res <- WsRoutes.routes(rm, wsb, p).orNotFound.run(req)
      } yield res.status
    }.unsafeRunSync()

  "A game socket" should "refuse an upgrade from a disallowed origin" in {
    wsStatus(policy, Some(evil)) shouldBe Status.Forbidden
  }

  it should "reject the origin before it reveals whether the room exists" in {
    // a 404-vs-403 difference would let any site enumerate live rooms
    wsStatus(policy, Some(evil)) should not be Status.NotFound
  }

  it should "let the deployed web app through" in {
    // reaches the room lookup, which is the point: origin is no longer the blocker
    wsStatus(policy, Some(allowed)) shouldBe Status.NotFound
  }

  it should "allow a client that sends no Origin (curl, native apps, tests)" in {
    wsStatus(policy, None) shouldBe Status.NotFound
  }

  it should "allow anything when no allowlist is configured" in {
    wsStatus(OriginPolicy.allowAll, Some(evil)) shouldBe Status.NotFound
  }
}
