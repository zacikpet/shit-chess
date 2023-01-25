package chess
package client

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.comcast.ip4s._
import com.monovore.decline._

object ClientApp extends IOApp:
  private val argsParser: Command[SocketAddress[IpAddress]] =
    Command("fs2chat-client", "FS2 Chat Client") {
      (
        Opts
          .option[String]("address", "Address of chat server")
          .withDefault("127.0.0.1")
          .mapValidated(p =>
            IpAddress.fromString(p).toValidNel("Invalid IP address")
          ),
        Opts
          .option[Int]("port", "Port of chat server")
          .withDefault(5555)
          .mapValidated(p => Port.fromInt(p).toValidNel("Invalid port number"))
      ).mapN { case (ip, port) => SocketAddress(ip, port) }
    }

  def run(args: List[String]): IO[ExitCode] =
    argsParser.parse(args) match
      case Left(help) => IO(System.err.println(help)).as(ExitCode.Error)
      case Right(address) =>
        Console
          .create[IO]
          .flatMap { implicit console =>
            Client
              .start[IO](address)
              .compile
              .drain
          }
          .as(ExitCode.Success)
