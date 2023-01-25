package chess
package game

trait Player {
  def turn(board: Board, side: Side)(using ui: UI): Board
}

case class HumanPlayer() extends Player {

  override def turn(board: Board, side: Side)(using ui: UI): Board =
    select(board, side)

  def select(board: Board, side: Side, message: Option[String] = None)(using ui: UI): Board = {
    ui.draw(board)

    try {
      if (message.isDefined) {
        ui.showMessage(message.get)
      }

      val piece = ui.readPiece()

      board.verifySideAt(piece, side)

      val newBoard = board.generateOptions(piece)

      if (newBoard.highlight.isEmpty) throw SelectException("No valid moves")

      place(newBoard, piece, side)
    } catch {
      case e: SelectException => select(board, side, Some(e.getMessage))
    }
  }

  def place(board: Board, from: Position, side: Side, message: Option[String] = None)(using
      ui: UI
  ): Board = {
    ui.draw(board)

    if (message.isDefined) {
      ui.showMessage(message.get)
    }

    val target = ui.readTarget()

    try board.applyMove(Move(from, target), side)
    catch {
      case e: InvalidMoveException => select(board.clearHighlight, side, Some(e.getMessage))
    }
  }

}

case class AIPlayer(side: Side, depth: Int) extends Player {

  override def turn(board: Board, side: Side)(using ui: UI): Board = {
    val moves = board.pieces.filter(_.side == side).toList.flatMap(piece => piece.validMoves(board))

    println(value(board))

    board.applyMove(moves(0), side)
  }

  def value(board: Board): Int =
    board.pieces.map(value).sum

  def value(piece: Piece) = {
    val points = piece.role match {
      case Pawn()   => 1
      case Knight() => 3
      case Bishop() => 3
      case Rook()   => 5
      case Queen()  => 9
      case King()   => 100
    }

    if (piece.side == side) points else -points
  }
}

case class NetworkPlayer() extends Player {

  override def turn(board: Board, side: Side)(using ui: UI): Board =
    board

}
