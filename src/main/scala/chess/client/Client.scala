package chess
package client

import cats.ApplicativeError
import cats.effect.{Concurrent, Ref, Temporal}
import com.comcast.ip4s.{IpAddress, SocketAddress}
import fs2.{RaiseThrowable, Stream}
import fs2.io.net.Network
import java.net.ConnectException
import scala.concurrent.duration._
import chess.game.Board
import chess.game.DefaultLayout
import cats.implicits._
import chess.game.TerminalUI
import chess.game.Move
import chess.game.Side
import chess.game.White
import cats.MonadError
import org.jline.utils.AttributedStringBuilder
import org.jline.utils.AttributedStyle

object Client:
  case class State[F[_]](val board: Board, val side: Side)

  class StateRef[F[_]: Concurrent](state: Ref[F, State[F]]):
    def get: F[State[F]] = state.get

    def set(board: Board, side: Side): F[Unit] = state.set(State(board, side))

    def applyMove(move: Move): F[Unit] =
      state.update(s => State(s.board.applyMove(move, s.side), s.side))

    def applyOpponentMove(move: Move): F[Unit] =
      state.update(s => State(s.board.applyMove(move, s.side.opposite), s.side))

    def updateSide(side: Side): F[Unit] = state.update(s => State(s.board, side))

    def inCheckMate: F[Boolean] = state.get.map(s => s.board.inCheckMate(s.side))

    def turn: F[Unit] = state.update(s =>
      State(
        Board(s.board.x, s.board.y, s.board.pieces, s.board.turn.opposite),
        s.side
      )
    )

  object StateRef:
    def apply[F[_]: Concurrent]: F[StateRef[F]] =
      Ref[F]
        .of(State(Board(8, 8, DefaultLayout.pieces, White), White))
        .map(ref => new StateRef(ref))

  def start[F[_]: Temporal: Network: Console](
      address: SocketAddress[IpAddress]
  ): Stream[F, Unit] =
    connect(address).handleErrorWith {
      case _: ConnectException =>
        val retryDelay = 5.seconds
        Stream.exec(Console[F].errorln(s"Failed to connect. Retrying in $retryDelay.")) ++
          start(address)
            .delayBy(retryDelay)
      case _: UserQuit => Stream.empty
      case t           => Stream.raiseError(t)
    }

  def connect[F[_]: Concurrent: Network: Console](
      address: SocketAddress[IpAddress]
  ): Stream[F, Unit] =
    Stream.exec(Console[F].info(s"Connecting to server $address")) ++
      Stream
        .resource(Network[F].client(address))
        .flatMap { socket =>
          Stream.exec(Console[F].info("Connected!")) ++
            Stream
              .eval(
                MessageSocket(
                  socket,
                  Protocol.ServerCommand.codec,
                  Protocol.ClientCommand.codec,
                  128
                )
              )
              .flatMap { socket =>
                Stream.exec(
                  socket.write1(Protocol.ClientCommand.Join(""))
                ) ++
                  Stream.eval(StateRef[F]).flatMap { state =>
                    processIncoming(socket, state).concurrently(
                      processOutgoing(socket, state)
                    )
                  }
              }
        }

  def processIncoming[F[_]: Console](
      messageSocket: MessageSocket[F, Protocol.ServerCommand, Protocol.ClientCommand],
      state: StateRef[F]
  )(implicit F: MonadError[F, Throwable]): Stream[F, Unit] =
    messageSocket.read.evalMap {
      case Protocol.ServerCommand.Alert(txt) =>
        Console[F].alert(txt)

      case Protocol.ServerCommand.Message(username, txt) =>
        Console[F].println(s"$username> $txt")

      case Protocol.ServerCommand.SetSide(side) =>
        state.updateSide(side) *>
          Console[F].alert("Assigned side: " + side)

      case Protocol.ServerCommand.Disconnect =>
        F.raiseError[Unit](new UserQuit)

      case Protocol.ServerCommand.SendMove(move) =>
        Console[F].info(s"> $move") *>
          state.applyOpponentMove(Move.fromString(move)) *>
          state.get.map(_.board).map(TerminalUI.draw) *>
          state.inCheckMate.flatMap {
            _ match
              case true  => messageSocket.write1(Protocol.ClientCommand.CheckMate)
              case false => state.turn
          }

      case Protocol.ServerCommand.GameStart =>
        state.get.map(_.board).map(TerminalUI.draw)

    }

  def processOutgoing[F[_]: RaiseThrowable: Console](
      socket: MessageSocket[F, Protocol.ServerCommand, Protocol.ClientCommand],
      state: StateRef[F]
  )(implicit F: MonadError[F, Throwable]): Stream[F, Unit] =
    Stream
      .repeatEval(Console[F].readLine("> "))
      .flatMap {
        case Some(txt) => Stream(txt)
        case None      => Stream.raiseError[F](new UserQuit)
      }
      .map { txt =>
        val move = Move.fromString(txt)
        (state.applyMove(move), Protocol.ClientCommand.MakeMove(txt))
      }
      .evalMap { (a, b) =>
        a *> state.get.map(_.board).map(TerminalUI.draw) *> socket.write1(b)
      }
      .handleErrorWith(e =>
        println(
          new AttributedStringBuilder()
            .style(AttributedStyle.DEFAULT.foreground(AttributedStyle.RED))
            .append("❌ " + e.getMessage)
            .toAnsi
        )

        processOutgoing(socket, state)
      )
      .drain
