package isometry

import game.{Board, Cell}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TranslationTests extends AnyFlatSpec with Matchers {

  "Translation" should "translate a sector correctly when extendable" in {
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

    val translation = Translation(1, 1, isExtendable = true)
    val newBoard = translation.apply(board, (0, 0, 2, 2))

    newBoard.getCell(0, 0).isMine shouldBe false
    newBoard.getCell(1, 1).isMine shouldBe true
  }

  it should "translate a sector correctly when not extendable" in {
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

    val translation = Translation(1, 1, isExtendable = false)
    val newBoard = translation.apply(board, (0, 0, 2, 2))

    newBoard.getCell(1, 1).isMine shouldBe true
    newBoard.getCell(0, 0).isMine shouldBe false
  }

  it should "clear the original sector cells after translation" in {
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

    val translation = Translation(1, 1, isExtendable = true)
    val newBoard = translation.apply(board, (0, 0, 2, 2))

    newBoard.getCell(0, 0).isMine shouldBe false
    newBoard.getCell(0, 1).isMine shouldBe false
    newBoard.getCell(0, 2).isMine shouldBe false
  }

  it should "return the inverse translation correctly" in {
    val translation = Translation(1, 1, isExtendable = true)
    val inverseTranslation = translation.inverse.asInstanceOf[Translation]

    inverseTranslation.deltaX shouldBe -1
    inverseTranslation.deltaY shouldBe -1
    inverseTranslation.isExtendable shouldBe true
  }
}