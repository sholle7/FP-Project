package isometry

import game.{Board, Cell}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RotationTests extends AnyFlatSpec with Matchers {

  "Rotation" should "rotate a sector 90 degrees clockwise" in {
    val board = new Board(3, 3, 1)
    board.setCell(0, 0, new Cell(isMine = true))
    board.setCell(0, 1, new Cell())
    board.setCell(0, 2, new Cell())
    board.setCell(1, 0, new Cell())
    board.setCell(1, 1, new Cell())
    board.setCell(1, 2, new Cell())
    board.setCell(2, 0, new Cell())
    board.setCell(2, 1, new Cell())
    board.setCell(2, 2, new Cell())

    val rotation = Rotation(90)
    val newBoard = rotation.apply(board, (0, 0, 2, 2))

    newBoard.getCell(0, 2).isMine shouldBe true
    newBoard.getCell(0, 1).isMine shouldBe false
    newBoard.getCell(0, 0).isMine shouldBe false
  }

  it should "rotate a sector 90 degrees counterclockwise" in {
    val board = new Board(3, 3, 1)
    board.setCell(0, 0, new Cell(isMine = true))
    board.setCell(0, 1, new Cell())
    board.setCell(0, 2, new Cell())
    board.setCell(1, 0, new Cell())
    board.setCell(1, 1, new Cell())
    board.setCell(1, 2, new Cell())
    board.setCell(2, 0, new Cell())
    board.setCell(2, 1, new Cell())
    board.setCell(2, 2, new Cell())

    val rotation = Rotation(-90)
    val newBoard = rotation.apply(board, (0, 0, 2, 2))

    newBoard.getCell(2, 0).isMine shouldBe true
    newBoard.getCell(1, 0).isMine shouldBe false
    newBoard.getCell(0, 0).isMine shouldBe false
  }

  it should "throw an error for invalid rotation angle" in {
    val board = new Board(3, 3, 1)
    val rotation = Rotation(45)

    an [IllegalArgumentException] should be thrownBy {
      rotation.apply(board, (0, 0, 2, 2))
    }
  }
}