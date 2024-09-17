package isometry

import game.Board

case class CentralSymmetry() extends Isometry {

  override def apply(board: Board, sector: (Int, Int, Int, Int)): Board = {
    val (topLeftRow, topLeftCol, bottomRightRow, bottomRightCol) = sector

    val centerRow = (topLeftRow + bottomRightRow) / 2
    val centerCol = (topLeftCol + bottomRightCol) / 2

    val sectorCells = board.getSector(topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)

    val newBoard = board.copy()

    for (r <- sectorCells.indices; c <- sectorCells(r).indices) {
      val currentRow = topLeftRow + r
      val currentCol = topLeftCol + c

      val symmetricRow = 2 * centerRow - currentRow
      val symmetricCol = 2 * centerCol - currentCol

      if (symmetricRow >= topLeftRow && symmetricRow <= bottomRightRow &&
        symmetricCol >= topLeftCol && symmetricCol <= bottomRightCol) {
        val temp = board.getCell(currentRow, currentCol)
        newBoard.setCell(currentRow, currentCol, board.getCell(symmetricRow,symmetricCol).copy())
        newBoard.setCell(symmetricRow, symmetricCol, temp.copy())
      }
    }

    newBoard
  }

  override def inverse: Isometry = CentralSymmetry()
}
