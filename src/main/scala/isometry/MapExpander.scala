package isometry

import game.{Board}

object MapExpander extends Expandability {
  override def expandMapIfNeeded(original: Board, transformed: Board, startX: Int, startY: Int): Board = {
    val originalRows = original.getBoardMap.length
    val originalCols = if (originalRows > 0) original.getBoardMap(0).length else 0
    val transformedRows = transformed.getBoardMap.length
    val transformedCols = if (transformedRows > 0) transformed.getBoardMap(0).length else 0

    val newRows = Math.max(originalRows, startY + transformedRows)
    val newCols = Math.max(originalCols, startX + transformedCols)

    if (newRows > originalRows || newCols > originalCols) {
      val newBoard = original.copy()

      for (row <- original.getBoardMap.indices; col <- original.getBoardMap(row).indices) {
        newBoard.getBoardMap(row)(col) = original.getBoardMap(row)(col).copy()
      }

      for (row <- transformed.getBoardMap.indices; col <- transformed.getBoardMap(row).indices) {
        val newRow = startY + row
        val newCol = startX + col
        if (newRow < newRows && newCol < newCols) {
          newBoard.getBoardMap(newRow)(newCol) = transformed.getBoardMap(row)(col).copy()
        }
      }

      newBoard
    } else {
      original
    }
  }
}
