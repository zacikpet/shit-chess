package chess
package game

import scala.Console

trait UI {
  def draw(board: Board): Unit

  def readPiece(): Position

  def readTarget(): Position

  def showMessage(message: String): Unit
}

object TerminalUI extends UI {
  override def draw(board: Board): Unit = {
    print("")
    for (i <- 1 to board.x)
      print(" " + (i + 96).toChar + " ")
    println()

    for (i <- board.y to 1 by -1) {
      print(i + " ")

      for (j <- 1 to board.x) {
        if (board.highlight.contains(Position(j, i))) {
          print(Console.GREEN_B)
        } else if ((i + j) % 2 == 0) {
          print(Console.WHITE_B)
        } else {
          print(Console.BLACK_B)
        }

        board.at(Position(j, i)) match {
          case Some(piece) =>
            print(" ")
            drawPiece(piece)
            print(" ")
          case None => print("   ")
        }
      }
      println(Console.RESET + " " + i)
    }

    print("  ")
    for (i <- 1 to board.x)
      print(" " + (i + 96).toChar + " ")
    println()
  }

  def drawPiece(piece: Piece) =
    print(getSideColor(piece.side) + getRoleSymbol(piece.role))

  override def readPiece(): Position = readPosition("Select piece")

  override def readTarget(): Position = readPosition("Select target")

  override def showMessage(message: String): Unit =
    println(message)

  private def getSideColor(side: Side): String =
    side match
      case White => Console.BOLD + Console.BLUE
      case Black => Console.BOLD + Console.RED

  private def getRoleSymbol(role: Role): String =
    role match
      case Pawn()   => "P"
      case Rook()   => "R"
      case Knight() => "K"
      case Bishop() => "B"
      case Queen()  => "Q"
      case King()   => "X"

  private def readPosition(text: String): Position = {
    print(text + ": ")

    val input = scala.io.StdIn.readLine()

    if (input.length != 2) {
      println("Invalid input")
      return readPosition(text)
    }

    val x = input(0).abs.toInt - 96
    val y = input(1).asDigit

    Position(x, y)
  }
}
