import game.Board
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardTests extends AnyFlatSpec with Matchers {

  "A game.Board" should "correctly load level from a string" in {
    val board = new Board(5, 5, 2)
    val level =
      """-----
        |--#--
        |-----
        |-#---
        |-----
        |""".stripMargin

    board.loadLevel(level)

    board.isMine(1, 2).shouldEqual(true)
    board.isMine(3, 1).shouldEqual(true)
    board.isMine(0, 0).shouldEqual(false)
  }

  it should "correctly calculate the number of neighboring mines" in {
    val board = new Board(3, 3, 3)
    val level =
      """-#-
        |---
        |#-#
        |""".stripMargin

    board.loadLevel(level)
    board.calculateNeighborMines()

    board.getCell(1, 1).neighborMines.shouldEqual(3)
    board.getCell(0, 0).neighborMines.shouldEqual(1)
    board.getCell(2, 1).neighborMines.shouldEqual(2)
  }

  it should "place random mines correctly" in {
    val board = new Board(5, 5, 3)
    board.placeRandomMines()

    board.getBoardMap.flatten.count(_.isMine).shouldEqual(3)
  }

  it should "reveal mines after a mine is hit" in {
    val board = new Board(3, 3, 0)
    val level =
      """#-#
        |---
        |#-#
        |""".stripMargin

    board.loadLevel(level)
    board.revealMines()

    board.isRevealed(0, 0).shouldEqual(true)
    board.isRevealed(2, 2).shouldEqual(true)
  }

  it should "reveal cells correctly without recursion issues" in {
    val board = new Board(3, 3, 0)
    val level =
      """---
        |---
        |---
        |""".stripMargin

    board.loadLevel(level)
    board.revealCell(1, 1)

    board.isRevealed(0, 0).shouldEqual(true)
    board.isRevealed(2, 2).shouldEqual(true)
  }

  it should "toggle flag on a cell" in {
    val board = new Board(3, 3, 0)
    board.flagCell(1, 1)
    board.isFlagged(1, 1).shouldEqual(true)

    board.flagCell(1, 1)
    board.isFlagged(1, 1).shouldEqual(false)
  }

  it should "detect when the game is finished" in {
    val board = new Board(2, 2, 1)
    val level =
      """-#
        |--
        |""".stripMargin

    board.loadLevel(level)

    board.revealCell(0, 0)
    board.revealCell(1, 0)
    board.revealCell(1, 1)

    board.isGameFinished.shouldEqual(true)
  }

  it should "give a correct hint for an unrevealed, unflagged non-mine cell" in {
    val board = new Board(3, 3, 1)
    val level =
      """---
        |-#-
        |---
        |""".stripMargin

    board.loadLevel(level)
    val hint = board.getHint

    board.isMine(hint._1, hint._2).shouldEqual(false)
    board.isRevealed(hint._1, hint._2).shouldEqual(false)
    board.isFlagged(hint._1, hint._2).shouldEqual(false)
  }
}