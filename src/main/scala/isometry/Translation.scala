package isometry

import game.{Board, Cell}
import isometry.MapExpander.expandMap

case class Translation(deltaX: Int, deltaY: Int, isExtendable: Boolean) extends Isometry {

  override def apply(board: Board, sector: (Int, Int, Int, Int)): Board = {
    val (topLeftRow, topLeftCol, bottomRightRow, bottomRightCol) = sector
    val sectorCells = board.getSector(topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)

    var newBoard = board.copy()

    if (isExtendable){
      val expandedBoard = expandMap(board, board.copy(), topLeftCol + deltaX, topLeftRow + deltaY)
      newBoard = expandedBoard.copy()
    }

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
    Translation(-deltaX, -deltaY, isExtendable)
  }

}
