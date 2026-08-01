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
import scala.concurrent.duration._

object Main extends IOApp {

  private val DbInitAttempts = 30
  private val DbInitDelay    = 2.seconds

  private def withDbRetry[A](what: String, attemptsLeft: Int = DbInitAttempts)(io: IO[A]): IO[A] =
    io.handleErrorWith { t =>
      if (attemptsLeft <= 1) IO.raiseError(t)
      else
        IO(println(s"$what: Postgres not ready (${t.getMessage}); retrying in $DbInitDelay ($attemptsLeft attempts left)")) *>
          IO.sleep(DbInitDelay) *> withDbRetry(what, attemptsLeft - 1)(io)
    }

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
      gameRepo    = new GameRecordRepo(xa)
      _          <- Resource.eval(IO(println(s"Connecting to Postgres at $dbUrl")))
      // Docker restart policies don't honor depends_on ordering after a daemon
      // restart, so Postgres may come up after us — retry before degrading.
      _          <- Resource.eval(withDbRetry("db init")(repo.init).handleErrorWith { t =>
                      IO(println(s"WARN: db init failed (${t.getMessage}); rooms will not persist"))
                    })
      gameRecording <- Resource.eval(
                      withDbRetry("game-record init")(gameRepo.init *> gameRepo.abortStale).flatMap { n =>
                        IO(println(s"Game recording enabled ($n stale in-progress games marked aborted)"))
                          .as(Option(gameRepo))
                      }.handleErrorWith { t =>
                        IO(println(s"WARN: game-record init failed (${t.getMessage}); games will not be recorded"))
                          .as(None: Option[GameRecordRepo])
                      })
      _          <- Resource.eval(IO(ChampionService.unavailableReason match {
                      case None    => println(s"Champion bot enabled (model: ${ChampionService.modelPath})")
                      case Some(e) => println(s"WARN: champion bot unavailable: $e")
                    }))
      rm         <- Resource.eval(RoomManager.create(repo, dispatcher, gameRecording))
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
