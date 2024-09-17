case class Translation(deltaX: Int, deltaY: Int) extends Isometry {
  override def apply(board: Board, sector: (Int, Int, Int, Int)): Board = {
    val (topLeftRow, topLeftCol, bottomRightRow, bottomRightCol) = sector
    val sectorCells = board.getSector(topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)

    val newBoard = board.copy()

    for (r <- sectorCells.indices) {
      for (c <- sectorCells(r).indices) {
        val newRow = topLeftRow + r + deltaY
        val newCol = topLeftCol + c + deltaX
        if (newRow >= 0 && newRow < newBoard.rows && newCol >= 0 && newCol < newBoard.cols) {
          newBoard.setCell(newRow, newCol, sectorCells(r)(c))
        }
      }
    }

    for (r <- sectorCells.indices) {
      for (c <- sectorCells(r).indices) {
        newBoard.setCell(topLeftRow + r, topLeftCol + c, new Cell())
      }
    }

    newBoard
  }

  override def inverse: Isometry = {
    Translation(-deltaX, -deltaY)
  }
}