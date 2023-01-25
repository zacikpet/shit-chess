package chess
package game

class Board(
    val x: Int,
    val y: Int,
    val pieces: Set[Piece],
    val turn: Side,
    val highlight: Set[Position] = Set[Position]()
) {

  def applyMove(move: Move, side: Side): Board = {
    val optPiece = at(move.from)

    if (optPiece.isEmpty) {
      throw new NoPieceException(move.from)
    }

    val piece = optPiece.get

    if (piece.side != side) {
      throw new InvalidMoveException(move.to)
    }

    if (piece.side != turn) {
      throw new TurnException(turn)
    }

    val movedPiece = piece.move(move.to, this)

    val newPieces =
      pieces.filter(p => p != piece && p.position != move.to) + movedPiece

    println("Board turn was " + turn)
    println("Board turn is now " + turn.opposite)

    Board(x, y, newPieces, turn.opposite)
  }

  def applyUncheckedMove(move: Move): Board = {
    val piece = at(move.from).get

    val movedPiece = piece.uncheckedMove(move.to)

    val newPieces =
      pieces.filter(p => p != piece && p.position != move.to) + movedPiece

    Board(x, y, newPieces, turn.opposite)
  }

  def generateOptions(from: Position): Board = {
    val piece = at(from)

    if (piece.isEmpty) {
      throw new NoPieceException(from)
    }

    val newHighlight = for {
      i <- 1 to x
      j <- 1 to y
      if piece.get.canMove(Position(i, j), this)
    } yield Position(i, j)

    Board(x, y, pieces, turn, newHighlight.toSet)
  }

  def at(position: Position): Option[Piece] =
    pieces.find(_.position == position)

  def occupied(position: Position): Boolean =
    at(position).isDefined

  def occupied(position: Position, side: Side): Boolean =
    at(position).exists(_.side == side)

  def clearHighlight: Board =
    Board(x, y, pieces, turn)

  def verifySideAt(position: Position, side: Side): Unit =
    if (!occupied(position, side)) {
      throw new TurnException(side)
    }

  def obstructed(from: Position, to: Position, role: Role): Boolean = {
    if (!role.isObstructible) return false

    val between = Position.between(from, to)

    between.map(occupied).exists(_ == true)
  }

  def inCheck(side: Side): Boolean = {
    val king = pieces.find(p => p.side == side && p.role.isKing)

    pieces.exists(p =>
      p.side == side.opposite && p.canTakeKing(king.get.position, this)
    )
  }

  def inCheckMate(side: Side): Boolean =
    inCheck(side) && pieces
      .filter(_.side == side)
      .forall(p => p.validMoves(this).isEmpty)

}
