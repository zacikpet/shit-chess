package chess
package game

trait Layout {
  val pieces: Set[Piece]

  val x: Int
  val y: Int
}

object DefaultLayout extends Layout {
  val x = 8
  val y = 8

  val pieces = Set(
    Piece(White, Rook(), Position(1, 1)),
    Piece(White, Knight(), Position(2, 1)),
    Piece(White, Bishop(), Position(3, 1)),
    Piece(White, Queen(), Position(4, 1)),
    Piece(White, King(), Position(5, 1)),
    Piece(White, Bishop(), Position(6, 1)),
    Piece(White, Knight(), Position(7, 1)),
    Piece(White, Rook(), Position(8, 1)),
    Piece(White, Pawn(), Position(1, 2)),
    Piece(White, Pawn(), Position(2, 2)),
    Piece(White, Pawn(), Position(3, 2)),
    Piece(White, Pawn(), Position(4, 2)),
    Piece(White, Pawn(), Position(5, 2)),
    Piece(White, Pawn(), Position(6, 2)),
    Piece(White, Pawn(), Position(7, 2)),
    Piece(White, Pawn(), Position(8, 2)),
    Piece(Black, Pawn(), Position(1, 7)),
    Piece(Black, Pawn(), Position(2, 7)),
    Piece(Black, Pawn(), Position(3, 7)),
    Piece(Black, Pawn(), Position(4, 7)),
    Piece(Black, Pawn(), Position(5, 7)),
    Piece(Black, Pawn(), Position(6, 7)),
    Piece(Black, Pawn(), Position(7, 7)),
    Piece(Black, Pawn(), Position(8, 7)),
    Piece(Black, Rook(), Position(1, 8)),
    Piece(Black, Knight(), Position(2, 8)),
    Piece(Black, Bishop(), Position(3, 8)),
    Piece(Black, Queen(), Position(4, 8)),
    Piece(Black, King(), Position(5, 8)),
    Piece(Black, Bishop(), Position(6, 8)),
    Piece(Black, Knight(), Position(7, 8)),
    Piece(Black, Rook(), Position(8, 8))
  )
}
