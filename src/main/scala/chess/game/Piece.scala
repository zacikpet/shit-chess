package chess
package game

case class Piece(val side: Side, val role: Role, val position: Position) {
  def move(target: Position, board: Board): Piece = {

    if (!canMove(target, board)) {
      throw new InvalidMoveException(target)
    }

    Piece(side, role, target)
  }

  def uncheckedMove(target: Position): Piece =
    Piece(side, role, target)

  def canMove(target: Position, board: Board): Boolean = {
    lazy val move =
      role.canMove(position, target, side, board) && !board.occupied(target)

    lazy val take =
      role.canTake(position, target, side, board) && board.occupied(
        target,
        side.opposite
      )

    lazy val obstructed = board.obstructed(position, target, role)

    lazy val willBeInCheck =
      board.applyUncheckedMove(Move(position, target)).inCheck(side)

    (move || take) && !obstructed && !willBeInCheck
  }

  def canTakeKing(target: Position, board: Board): Boolean = {
    lazy val take =
      role.canTake(position, target, side, board) && board.occupied(
        target,
        side.opposite
      )

    lazy val obstructed = board.obstructed(position, target, role)

    take && !obstructed
  }

  def validMoves(board: Board): List[Move] = {
    val seq = for {
      x <- 1 to board.x
      y <- 1 to board.y
      if canMove(Position(x, y), board)
    } yield Move(position, Position(x, y))

    seq.toList
  }
}
