package chess
package server

import cats.{FlatMap, MonadError}
import cats.effect.{Async, Concurrent, Ref, Sync}
import cats.effect.std.UUIDGen
import cats.implicits._
import com.comcast.ip4s.Port
import fs2.Stream
import fs2.io.net.{Network, Socket}
import java.util.UUID

import chess.game.{Black, Side, White}
import chess.game.DefaultLayout
import chess.game.Board
import chess.game.Move

object Server:
  case class ConnectedClient[F[_]](
      id: UUID,
      side: Option[Side],
      socket: MessageSocket[F, Protocol.ClientCommand, Protocol.ServerCommand]
  )

  object ConnectedClient:
    def apply[F[_]: Concurrent: UUIDGen](socket: Socket[F]): F[ConnectedClient[F]] =
      for {
        id <- UUIDGen[F].randomUUID
        socket <- MessageSocket(
          socket,
          Protocol.ClientCommand.codec,
          Protocol.ServerCommand.codec,
          1024
        )
      } yield ConnectedClient(id, None, socket)

  case class State[F[_]](val side: Side, val clients: Map[UUID, ConnectedClient[F]])

  class StateRef[F[_]: Concurrent](state: Ref[F, State[F]]):

    def get(id: UUID): F[Option[ConnectedClient[F]]] =
      state.get.map(_.clients.get(id))

    def all: F[List[ConnectedClient[F]]] = state.get.map(_.clients.values.toList)

    def allExcept(id: UUID): F[List[ConnectedClient[F]]] =
      state.get.map(_.clients.values.filter(_.id != id).toList)

    def register(client: ConnectedClient[F]): F[Unit] =
      state.update(old => State(old.side, old.clients + (client.id -> client)))

    def unregister(id: UUID): F[Option[ConnectedClient[F]]] =
      state.modify(old => (State(old.side, old.clients - id), old.clients.get(id)))

    def setSide(clientId: UUID): F[Side] =
      state.modify { state =>
        val sideToSet = determineSide(state.clients - clientId)

        val updatedClient = state.clients.get(clientId).map(_.copy(side = Some(sideToSet)))

        val updatedClients = updatedClient
          .map(c => state.clients + (clientId -> c))
          .getOrElse(state.clients)

        (State(state.side, updatedClients), sideToSet)
      }

    def broadcast(cmd: Protocol.ServerCommand): F[Unit] =
      all.flatMap(_.traverse_(_.socket.write1(cmd)))

    def broadcastExcept(cmd: Protocol.ServerCommand, id: UUID): F[Unit] =
      allExcept(id).flatMap(_.traverse_(_.socket.write1(cmd)))

    def turn(): F[Unit] =
      state.update(old => State(old.side.opposite, old.clients))

    def onTurn(side: Side): F[Boolean] =
      state.get.map(_.side == side)

  object StateRef:
    def apply[F[_]: Concurrent]: F[StateRef[F]] =
      Ref[F]
        .of(State(White, Map.empty[UUID, ConnectedClient[F]]))
        .map(ref => new StateRef(ref))

  def start[F[_]: Async: Network: Console](port: Port) =
    Stream.exec(Console[F].info(s"Starting server on port $port")) ++
      Stream
        .eval(StateRef[F])
        .flatMap { clients =>
          Network[F].server(port = Some(port)).map { clientSocket =>

            def unregisterClient(client: ConnectedClient[F]) =
              clients.unregister(client.id).flatMap { client =>
                client
                  .flatMap(_.side)
                  .traverse_(side =>
                    clients.broadcast(Protocol.ServerCommand.Alert(s"$side disconnected."))
                  )
              } *> Console[F].info(s"Unregistered client ${client.id}")

            Stream
              .bracket(ConnectedClient[F](clientSocket).flatTap(clients.register))(
                unregisterClient
              )
              .flatMap(client => handleClient[F](clients, client, clientSocket))
              .scope
          }
        }
        .parJoin(2)

  private def handleClient[F[_]: Concurrent: Console](
      state: StateRef[F],
      client: ConnectedClient[F],
      socket: Socket[F]
  ): Stream[F, Nothing] =
    logClient(client, socket)
      ++ Stream.exec(client.socket.write1(Protocol.ServerCommand.Alert("Welcome to Chess")))
      ++ processIncoming(state, client.id, client.socket)

  def logClient[F[_]: FlatMap: Console](client: ConnectedClient[F], socket: Socket[F]) =
    Stream.exec(socket.remoteAddress.flatMap { clientAddress =>
      Console[F].info(s"Accepted client ${client.id} on $clientAddress")
    })

  def processIncoming[F[_]](
      state: StateRef[F],
      clientId: UUID,
      socket: MessageSocket[F, Protocol.ClientCommand, Protocol.ServerCommand]
  )(implicit F: MonadError[F, Throwable]): Stream[F, Nothing] =
    socket.read.evalMap {

      case Protocol.ClientCommand.Join(_) =>
        state.setSide(clientId).flatMap { newSide =>
          socket.write1(Protocol.ServerCommand.SetSide(newSide)) *> {
            newSide match
              case White => F.unit
              case Black => state.broadcast(Protocol.ServerCommand.GameStart)
          }
        }

      case Protocol.ClientCommand.MakeMove(message) =>
        state.get(clientId).flatMap {
          case Some(client) =>
            client.side match
              case None => F.unit
              case Some(side) =>
                state.onTurn(side).flatMap { isOnTurn =>
                  try {
                    val move = Move.fromString(message)
                    if (isOnTurn) {
                      val cmd = Protocol.ServerCommand.SendMove(move.toString)
                      val nextTurn = Protocol.ServerCommand.Alert(s"${side.opposite}'s turn")
                      state.turn() *> state.broadcastExcept(cmd, client.id) *> state
                        .broadcast(nextTurn)
                    } else {
                      socket.write1(Protocol.ServerCommand.Alert("Not your turn"))
                    }
                  } catch {
                    case e => socket.write1(Protocol.ServerCommand.Alert(e.getMessage))
                  }
                }
          case None => F.unit
        }

      case Protocol.ClientCommand.Nothing => F.unit
    }.drain

  def determineSide[F[_]](clients: Map[UUID, ConnectedClient[F]]): Side =
    if (clients.isEmpty) White else Black
