package io.fele.mahjong.server

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.effect.std.Dispatcher
import cats.syntax.all._
import com.comcast.ip4s._
import com.typesafe.config.ConfigFactory
import doobie.hikari.HikariTransactor
import io.fele.app.mahjong.{Config => EngineConfig}
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.Router

import scala.concurrent.ExecutionContext

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    val tcCfg = ConfigFactory.load()
    implicit val engineConfig: EngineConfig = new EngineConfig()
    implicit val ec: ExecutionContext = scala.concurrent.ExecutionContext.global

    val host  = Host.fromString(tcCfg.getString("server.host")).getOrElse(host"0.0.0.0")
    val port  = Port.fromInt(tcCfg.getInt("server.port")).getOrElse(port"8080")

    val dbUrl  = tcCfg.getString("db.url")
    val dbUser = tcCfg.getString("db.user")
    val dbPass = tcCfg.getString("db.password")
    val driver = tcCfg.getString("db.driver")
    val poolSz = tcCfg.getInt("db.poolSize")

    val resources: Resource[IO, RoomManager] = for {
      ce         <- ExecutionContexts.fixedThreadPool[IO](poolSz)
      xa         <- HikariTransactor.newHikariTransactor[IO](driver, dbUrl, dbUser, dbPass, ce)
      dispatcher <- Dispatcher.parallel[IO]
      repo        = new RoomRepo(xa)
      _          <- Resource.eval(IO(println(s"Connecting to Postgres at $dbUrl")))
      _          <- Resource.eval(repo.init.handleErrorWith { t =>
                      IO(println(s"WARN: db init failed (${t.getMessage}); rooms will not persist"))
                    })
      rm         <- Resource.eval(RoomManager.create(repo, dispatcher))
      _          <- Resource.eval(rm.restoreFromDb.handleErrorWith { t =>
                      IO(println(s"WARN: db restore failed: ${t.getMessage}"))
                    })
    } yield rm

    resources.flatMap { rm =>
      EmberServerBuilder.default[IO]
        .withHost(host)
        .withPort(port)
        .withHttpWebSocketApp { wsb =>
          val combined: org.http4s.HttpRoutes[IO] = org.http4s.HttpRoutes[IO] { req =>
            Routes.withCors(Routes.routes(rm)).run(req)
              .orElse(WsRoutes.routes(rm, wsb).run(req))
          }
          Router("/" -> combined).orNotFound
        }
        .build
    }.use { _ =>
      IO(println(s"Mahjong server listening on $host:$port")) *> IO.never
    }
  }
}

/** Tiny helper to avoid pulling in the doobie utility. */
private object ExecutionContexts {
  def fixedThreadPool[F[_]: cats.effect.Sync](size: Int): Resource[F, ExecutionContext] =
    Resource
      .make(cats.effect.Sync[F].delay {
        java.util.concurrent.Executors.newFixedThreadPool(size)
      })(es => cats.effect.Sync[F].delay { es.shutdown(); () })
      .map(scala.concurrent.ExecutionContext.fromExecutorService)
}
