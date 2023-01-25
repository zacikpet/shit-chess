package chess
package game

import cats.Eq

case class Side(value: String) extends Ordered[Side]:
  def compare(that: Side): Int = value.compare(that.value)
  override def toString: String = value

  def opposite: Side = this match
    case White => Black
    case Black => White

  def direction: Int = this match
    case White => 1
    case Black => -1

object Side:
  implicit val eqInstance: Eq[Side] = Eq.fromUniversalEquals[Side]

object White extends Side("White")
object Black extends Side("Black")
