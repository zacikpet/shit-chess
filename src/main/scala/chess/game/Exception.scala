package chess
package game

class SelectException(message: String) extends Exception(message)

class NoPieceException(position: Position)
    extends SelectException("No piece at position " + position)

class TurnException(side: Side)
    extends SelectException("It's " + side + "'s turn")

class InvalidMoveException(position: Position)
    extends Exception("Piece cannot move to this position")
