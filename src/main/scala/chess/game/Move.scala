package chess
package game

class Move(val from: Position, val to: Position) {
  override def toString: String =
    from.toString + ":" + to.toString
}

object Move {
  def fromString(str: String): Move = {
    if (str.length != 5)
      throw new IllegalArgumentException("Move must be 5 characters long")

    if (str(2) != ':')
      throw new IllegalArgumentException("Move must be in format 'a1-a2'")

    val from = Position.fromString(str.substring(0, 2))
    val to = Position.fromString(str.substring(3, 5))

    Move(from, to)
  }
}
