package isometry

import game.{Board, Cell}
import isometry.MapExpander.expandMap

case class Translation(deltaX: Int, deltaY: Int, isExtendable: Boolean) extends Isometry {

  override def apply(board: Board, sector: (Int, Int, Int, Int)): Board = {
    val (topLeftRow, topLeftCol, bottomRightRow, bottomRightCol) = sector
    val sectorCells = board.getSector(topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)

    var newBoard = board.copy()

    if (isExtendable){
      if (bottomRightCol + deltaX > board.cols || topLeftCol + deltaX < 0 || bottomRightRow + deltaY > board.rows || topLeftRow + deltaY < 0) {
        newBoard = expandMap(board, board.copy(), bottomRightCol + deltaX, bottomRightRow + deltaY)
      }
    }

    for (r <- sectorCells.indices) {
      for (c <- sectorCells(r).indices) {
        val newRow = topLeftRow + r + deltaY
        val newCol = topLeftCol + c + deltaX
        if (isExtendable) {
          newBoard.setCell(newRow, newCol, sectorCells(r)(c))
        }
        else {
          if (newRow >= 0 && newRow < newBoard.rows && newCol >= 0 && newCol < newBoard.cols) {
            if (!newBoard.getCell(newRow, newCol).isMine) {
              newBoard.setCell(newRow, newCol, sectorCells(r)(c))
            }
          }
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
