package isometry

import game.Board

object MapExpander extends Expandability {
  override def expandMap(original: Board, transformed: Board, newCols: Int, newRows: Int): Board = {
    val originalRows = original.getBoardMap.length
    val originalCols = if (originalRows > 0) original.getBoardMap(0).length else 0

    val newBoard = new Board(newRows, newCols, original.mines)

    for (row <- 0 until originalRows) {
      for (col <- 0 until originalCols) {
        newBoard.setCell(row, col, original.getCell(row, col).copy())
      }
    }

    for (row <- 0 until transformed.getBoardMap.length) {
      for (col <- 0 until transformed.getBoardMap(row).length) {
        val newRow = row
        val newCol = col
        if (newRow < newRows && newCol < newCols) {
          newBoard.setCell(newRow, newCol, transformed.getCell(row, col).copy())
        }
      }
    }

    newBoard
  }
}
