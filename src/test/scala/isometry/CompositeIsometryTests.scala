package isometry

import game.{Board, Cell}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CompositeIsometryTests extends AnyFlatSpec with Matchers {

  def createTestBoard(rows: Int, cols: Int): Board = {
    val board = new Board(rows, cols, rows * cols / 2)
    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        val cell = new Cell(isMine = (r + c) % 2 == 0, neighborMines = (r + c) % 3)
        board.setCell(r, c, cell)
      }
    }
    board
  }

  "A CompositeIsometry" should "apply multiple isometries in sequence" in {
    val board = createTestBoard(4, 4)
    val sector = (0, 0, 3, 3)

    val reflection = Reflection(horizontal = true)
    val symmetry = CentralSymmetry()
    val composite = CompositeIsometry(List(reflection, symmetry))

    val transformedBoard = composite(board, sector)

    transformedBoard.getCell(0, 0).isMine shouldEqual transformedBoard.getCell(3, 3).isMine
    transformedBoard.getCell(1, 1).isMine shouldEqual transformedBoard.getCell(2, 2).isMine
  }

  it should "apply isometries in the correct order" in {
    val board = createTestBoard(4, 4)
    val sector = (0, 0, 3, 3)

    val reflection = Reflection(horizontal = true)
    val symmetry = CentralSymmetry()
    val composite = CompositeIsometry(List(reflection, symmetry))

    val transformedBoard = composite(board, sector)

    transformedBoard.getCell(0, 1).isMine shouldEqual transformedBoard.getCell(3, 2).isMine
  }

  it should "compute the inverse correctly" in {
    val board = createTestBoard(4, 4)
    val sector = (0, 0, 3, 3)

    val reflection = Reflection(horizontal = true)
    val symmetry = CentralSymmetry()
    val composite = CompositeIsometry(List(reflection, symmetry))

    val transformedBoard = composite(board, sector)
    val inverseComposite = composite.inverse

    val restoredBoard = inverseComposite(transformedBoard, sector)

    for (r <- 0 until board.rows) {
      for (c <- 0 until board.cols) {
        restoredBoard.getCell(r, c).isMine shouldEqual board.getCell(r, c).isMine
      }
    }
  }

  it should "handle an empty composite correctly" in {
    val board = createTestBoard(4, 4)
    val sector = (0, 0, 3, 3)

    val composite = CompositeIsometry(List())

    val transformedBoard = composite(board, sector)

    transformedBoard shouldEqual board
  }
}