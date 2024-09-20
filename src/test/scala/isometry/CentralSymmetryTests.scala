package isometry

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import game.{Board, Cell}

class CentralSymmetryTests extends AnyFlatSpec with Matchers {

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

  "A CentralSymmetry" should "correctly apply central symmetry transformation" in {
    val board = createTestBoard(4, 4)
    val sector = (0, 0, 3, 3)

    val symmetry = CentralSymmetry()
    val transformedBoard = symmetry(board, sector)

    transformedBoard.getCell(0, 0).isMine shouldEqual board.getCell(3, 3).isMine
    transformedBoard.getCell(0, 1).isMine shouldEqual board.getCell(3, 2).isMine
    transformedBoard.getCell(1, 0).isMine shouldEqual board.getCell(2, 3).isMine
    transformedBoard.getCell(1, 1).isMine shouldEqual board.getCell(2, 2).isMine
    transformedBoard.getCell(2, 0).isMine shouldEqual board.getCell(1, 3).isMine
    transformedBoard.getCell(2, 1).isMine shouldEqual board.getCell(1, 2).isMine
    transformedBoard.getCell(3, 0).isMine shouldEqual board.getCell(0, 3).isMine
    transformedBoard.getCell(3, 1).isMine shouldEqual board.getCell(0, 2).isMine
  }

  it should "handle central symmetry on smaller sectors" in {
    val board = createTestBoard(4, 4)
    val sector = (1, 1, 2, 2)

    val symmetry = CentralSymmetry()
    val transformedBoard = symmetry(board, sector)

    transformedBoard.getCell(1, 1).isMine shouldEqual board.getCell(2, 2).isMine
    transformedBoard.getCell(1, 2).isMine shouldEqual board.getCell(2, 1).isMine
    transformedBoard.getCell(2, 1).isMine shouldEqual board.getCell(1, 2).isMine
    transformedBoard.getCell(2, 2).isMine shouldEqual board.getCell(1, 1).isMine
  }

  it should "not modify cells outside the sector" in {
    val board = createTestBoard(4, 4)
    val sector = (1, 1, 2, 2)

    val symmetry = CentralSymmetry()
    val transformedBoard = symmetry(board, sector)

    transformedBoard.getCell(0, 0).isMine shouldEqual board.getCell(0, 0).isMine
    transformedBoard.getCell(0, 1).isMine shouldEqual board.getCell(0, 1).isMine
    transformedBoard.getCell(3, 3).isMine shouldEqual board.getCell(3, 3).isMine
  }
}
