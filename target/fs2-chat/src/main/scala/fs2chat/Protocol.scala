package chess

import scodec.Codec
import scodec.codecs._

import chess.game.Side
import chess.game.Board
object Protocol:

  private val side: Codec[Side] = utf8_32.as[Side]

  enum ClientCommand:
    case MakeMove(move: String)
    case Join(password: String)
    case Nothing

  object ClientCommand:
    val codec: Codec[ClientCommand] = discriminated[ClientCommand]
      .by(uint8)
      .typecase(1, utf8_32.as[Join])
      .typecase(2, utf8_32.as[MakeMove])
      .typecase(3, provide(Nothing))

  enum ServerCommand:
    case SetSide(side: Side)
    case Alert(text: String)
    case Message(side: Side, text: String)
    case SendMove(move: String)
    case GameStart
    case Disconnect

  object ServerCommand:
    val codec: Codec[ServerCommand] = discriminated[ServerCommand]
      .by(uint8)
      .typecase(129, side.as[SetSide])
      .typecase(130, utf8_32.as[Alert])
      .typecase(131, (side :: utf8_32).as[Message])
      .typecase(132, utf8_32.as[SendMove])
      .typecase(133, provide(GameStart))
      .typecase(134, provide(Disconnect))
