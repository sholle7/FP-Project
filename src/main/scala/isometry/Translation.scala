package isometry

import game.{Board, Cell}
import isometry.MapExpander.expandMap

case class Translation(deltaX: Int, deltaY: Int, isExtendable: Boolean) extends Isometry {

  override def apply(board: Board, sector: (Int, Int, Int, Int)): Board = {
    val (topLeftRow, topLeftCol, bottomRightRow, bottomRightCol) = sector
    val sectorCells = board.getSector(topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)

    var newBoard = board.copy()

    if (isExtendable) {
      val newBottomRow = bottomRightRow + deltaY
      val newBottomCol = bottomRightCol + deltaX
      val newTopRow = topLeftRow + deltaY
      val newTopCol = topLeftCol + deltaX

      if (newBottomCol >= board.cols || newTopCol < 0 || newBottomRow >= board.rows || newTopRow < 0) {
        val newCols = Math.max(board.cols, newBottomCol + 1)
        val newRows = Math.max(board.rows, newBottomRow + 1)
        newBoard = expandMap(board, board.copy(), newCols, newRows)
      }
    }

    for (r <- sectorCells.indices) {
      for (c <- sectorCells(r).indices) {
        newBoard.setCell(r ,  c, new Cell())
      }
    }

    for (r <- sectorCells.indices) {
      for (c <- sectorCells(r).indices) {
        val originalRow = topLeftRow + r
        val originalCol = topLeftCol + c
        val newRow = originalRow + deltaY
        val newCol = originalCol + deltaX

        if (isExtendable || (newRow >= 0 && newRow < newBoard.rows && newCol >= 0 && newCol < newBoard.cols)) {
          newBoard.setCell(newRow, newCol, sectorCells(r)(c))
        }

      }
    }

    newBoard
  }

  override def inverse: Isometry = {
    Translation(-deltaX, -deltaY, isExtendable)
  }
}
