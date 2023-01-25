package chess
package game

import Math.abs

trait Role {
  def canMove(from: Position, to: Position, side: Side, board: Board): Boolean

  def canTake(from: Position, to: Position, side: Side, board: Board) =
    canMove(from, to, side, board)

  def step(from: Int, to: Int) = if (from < to) 1 else -1

  val isObstructible = false

  val isKing = false
}

trait Obstructible extends Role {
  override val isObstructible = true
}

case class Pawn() extends Role with Obstructible {

  override def canMove(from: Position, to: Position, side: Side, board: Board) = {
    val start = side match {
      case White => 2
      case Black => board.y - 1
    }

    val canMoveOne = from + Step(0, side.direction) == to

    val canMoveTwo = from.y == start && from + Step(0, 2 * side.direction) == to

    canMoveOne || canMoveTwo
  }

  override def canTake(from: Position, to: Position, side: Side, board: Board): Boolean =
    from.y + side.direction == to.y && abs(from.x - to.x) == 1
}

case class Rook() extends Role with Obstructible {
  override def canMove(from: Position, to: Position, side: Side, board: Board) =
    from.x == to.x || from.y == to.y
}

case class Knight() extends Role {
  override def canMove(from: Position, to: Position, side: Side, board: Board) =
    abs(from.x - to.x) == 1 && abs(from.y - to.y) == 2
      || abs(from.x - to.x) == 2 && abs(from.y - to.y) == 1
}

case class Bishop() extends Role with Obstructible {
  override def canMove(from: Position, to: Position, side: Side, board: Board) =
    from.x + from.y == to.x + to.y || from.x - from.y == to.x - to.y
}

case class Queen() extends Role with Obstructible {
  override def canMove(from: Position, to: Position, side: Side, board: Board) =
    Rook().canMove(from, to, side, board) || Bishop().canMove(from, to, side, board)
}

case class King() extends Role with Obstructible {
  override def canMove(from: Position, to: Position, side: Side, board: Board) =
    from.nextTo(to)

  override val isKing = true
}
