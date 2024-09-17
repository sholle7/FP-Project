package isometry

import game.{Board, Cell}

case class Rotation(angle: Int, clockwise: Boolean = true) extends Isometry {
  override def apply(board: Board, sector: (Int, Int, Int, Int)): Board = {
    val (topLeftRow, topLeftCol, bottomRightRow, bottomRightCol) = sector
    val sectorCells = board.getSector(topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)

    val normalizedAngle = if (angle == 90) 90 else if (angle == -90) -90 else throw new IllegalArgumentException("Invalid rotation angle")

    val rotatedCells = normalizedAngle match {
      case 90 => rotate90(sectorCells)
      case -90 => rotate90CounterClockwise(sectorCells)
    }

    val newBoard = board.copy()
    newBoard.applyTransformation(topLeftRow, topLeftCol, rotatedCells)
    newBoard
  }

  override def inverse: Isometry = {
    Rotation(-angle, !clockwise)
  }

  private def rotate90(cells: Array[Array[Cell]]): Array[Array[Cell]] = {
    val numRows = cells.length
    val numCols = cells(0).length
    val rotated = Array.ofDim[Cell](numCols, numRows)

    for (r <- 0 until numRows) {
      for (c <- 0 until numCols) {
        rotated(c)(numRows - 1 - r) = cells(r)(c)
      }
    }

    rotated
  }

  private def rotate90CounterClockwise(cells: Array[Array[Cell]]): Array[Array[Cell]] = {
    val numRows = cells.length
    val numCols = cells(0).length
    val rotated = Array.ofDim[Cell](numCols, numRows)

    for (r <- 0 until numRows) {
      for (c <- 0 until numCols) {
        rotated(numCols - 1 - c)(r) = cells(r)(c)
      }
    }

    rotated
  }
}
