package chess
package game

import Math.abs

case class Position(val x: Int, val y: Int) {
  override def equals(that: Any): Boolean =
    that match
      case Position(this.x, this.y) => true
      case _                        => false

  def nextTo(that: Position): Boolean =
    abs(this.x - that.x) <= 1 && abs(this.y - that.y) <= 1

  def +(step: Step): Position =
    Position(x + step.x, y + step.y)

  override def toString: String =
    (x + 96).toChar.toString + y.toString
}

object Position {

  def min(a: Int, b: Int): Int = if (a < b) a else b

  def max(a: Int, b: Int): Int = if (a > b) a else b

  def step(from: Int, to: Int): Int = if (from < to) 1 else -1

  def between(from: Position, to: Position): List[Position] =
    (from, to) match
      case (Position(x1, y1), Position(x2, y2)) if x1 == x2 =>
        (min(y1, y2) + 1 until max(y1, y2)).map(y => Position(x1, y)).toList

      case (Position(x1, y1), Position(x2, y2)) if y1 == y2 =>
        (min(x1, x2) + 1 until max(x1, x2)).map(x => Position(x, y1)).toList

      case (Position(x1, y1), Position(x2, y2))
          if abs(x1 - x2) == abs(y1 - y2) =>
        (1 until abs(x1 - x2))
          .map(i => Position(x1 + i * step(x1, x2), y1 + i * step(y1, y2)))
          .toList

      case _ => Nil

  def fromString(str: String): Position = {
    if (str.length != 2)
      throw new IllegalArgumentException("Position must be 2 characters long")

    val x = str(0).toInt - 96
    val y = str(1).toInt - 48

    if (x < 1 || x > 8 || y < 1 || y > 8)
      throw new IllegalArgumentException("Position must be between a1 and h8")

    Position(x, y)
  }
}

case class Step(val x: Int, val y: Int)
