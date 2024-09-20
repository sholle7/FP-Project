package isometry

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import game.{Board, Cell}

class ReflectionTests extends AnyFlatSpec with Matchers {

  def createTestBoard(rows: Int, cols: Int): Board = {
    val board = new Board(rows, cols, rows * cols / 2)
    for (r <- 0 until rows) {
      for (c <- 0 until cols) {
        val cell = new Cell(isMine = (r * cols + c + 1) % 2 == 0, neighborMines = (r * cols + c + 1) % 3)
        board.setCell(r, c, cell)
      }
    }
    board
  }

  "A Reflection" should "correctly apply horizontal reflection" in {
    val board = createTestBoard(3, 3)
    val sector = (0, 0, 2, 2)
    val reflection = Reflection(horizontal = true)
    val reflectedBoard = reflection(board, sector)

    reflectedBoard.getCell(0, 0).isMine shouldEqual board.getCell(2, 0).isMine
    reflectedBoard.getCell(1, 0).neighborMines shouldEqual board.getCell(1, 0).neighborMines
    reflectedBoard.getCell(2, 0).isMine shouldEqual board.getCell(0, 0).isMine
  }

  it should "correctly apply vertical reflection" in {
    val board = createTestBoard(3, 3)
    val sector = (0, 0, 2, 2)  // Full board
    val reflection = Reflection(vertical = true)
    val reflectedBoard = reflection(board, sector)

    reflectedBoard.getCell(0, 0).isMine shouldEqual board.getCell(0, 2).isMine
    reflectedBoard.getCell(0, 1).neighborMines shouldEqual board.getCell(0, 1).neighborMines
    reflectedBoard.getCell(0, 2).isMine shouldEqual board.getCell(0, 0).isMine
  }

  it should "correctly apply left diagonal reflection" in {
    val board = createTestBoard(3, 3)
    val sector = (0, 0, 2, 2)
    val reflection = Reflection(leftDiagonal = true)
    val reflectedBoard = reflection(board, sector)

    reflectedBoard.getCell(0, 0).isMine shouldEqual board.getCell(0, 0).isMine
    reflectedBoard.getCell(0, 1).neighborMines shouldEqual board.getCell(1, 0).neighborMines
    reflectedBoard.getCell(0, 2).isMine shouldEqual board.getCell(2, 0).isMine
  }

  it should "correctly apply right diagonal reflection" in {
    val board = createTestBoard(3, 3)
    val sector = (0, 0, 2, 2)
    val reflection = Reflection(rightDiagonal = true)
    val reflectedBoard = reflection(board, sector)

    reflectedBoard.getCell(0, 0).isMine shouldEqual board.getCell(2, 2).isMine
    reflectedBoard.getCell(0, 2).isMine shouldEqual board.getCell(0, 0).isMine
  }
}