package chess
package game

object Game {
  def start(players: (Player, Player))(using layout: Layout, ui: UI) = {
    val board = Board(layout.x, layout.y, layout.pieces, White)

    turn(board, White, Stream.continually(players.toList).flatten.iterator)
  }

  def turn(board: Board, side: Side, playerIterator: Iterator[Player])(using
      ui: UI
  ): Board = {
    val player = playerIterator.next()

    val newBoard = player.turn(board, side)

    if (newBoard.inCheckMate(side.opposite)) {
      ui.showMessage(s"Checkmate! ${side} wins!")
      return newBoard
    }

    turn(newBoard, side.opposite, playerIterator)
  }
}
