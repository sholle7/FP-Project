import scala.reflect.ClassTag

case class Reflection(horizontal: Boolean = false, vertical: Boolean = false, leftDiagonal: Boolean = false, rightDiagonal: Boolean = false) extends Isometry {
  override def apply(board: Board, sector: (Int, Int, Int, Int)): Board = {
    val (topLeftRow, topLeftCol, bottomRightRow, bottomRightCol) = sector
    val sectorCells = board.getSector(topLeftRow, topLeftCol, bottomRightRow, bottomRightCol)

    val reflectedCells = if (horizontal) {
      sectorCells.reverse
    } else if (vertical) {
      sectorCells.map(_.reverse)
    } else if (leftDiagonal) {
      transpose(sectorCells)
    } else if (rightDiagonal) {
      transpose(sectorCells.map(_.reverse)).reverse
    } else {
      sectorCells
    }

    val newBoard = board.copy()
    newBoard.applyTransformation(topLeftRow, topLeftCol, reflectedCells)
    newBoard
  }

  private def transpose[A: ClassTag](matrix: Array[Array[A]]): Array[Array[A]] = {
    val rows = matrix.length
    val cols = matrix(0).length
    val transposed = Array.ofDim[A](cols, rows)
    for (i <- 0 until rows) {
      for (j <- 0 until cols) {
        transposed(j)(i) = matrix(i)(j)
      }
    }
    transposed
  }

  override def inverse: Isometry = {
    if (horizontal) {
      Reflection(horizontal = true)
    } else if (vertical) {
      Reflection(vertical = true)
    } else if (leftDiagonal) {
      Reflection(rightDiagonal = true)
    } else if (rightDiagonal) {
      Reflection(leftDiagonal = true)
    } else {
      this
    }
  }
}