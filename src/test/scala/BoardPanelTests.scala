import game.{Board, BoardPanel, Cell}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BoardPanelTests extends AnyFlatSpec with Matchers {

  "game.BoardPanel" should "handle left click correctly by revealing a cell" in {
    val board = new Board(2, 2, 2)
    val panel = new BoardPanel(board, 2, 2)

    panel.handleLeftClick(0, 0)

    board.isRevealed(0, 0).shouldBe(true)
  }

  it should "handle left click on a mine and show game over dialog" in {
    val board = new Board(2, 2, 2)
    board.getBoardMap(0)(0) = new Cell(isMine = true)
    val panel = new BoardPanel(board, 2, 2)

    panel.handleLeftClick(0, 0)

    board.isRevealed(0, 0).shouldBe(false)
    panel.score.shouldBe(0)
  }

  it should "handle right click correctly by flagging a cell" in {
    val board = new Board(2, 2, 2)
    val panel = new BoardPanel(board, 2, 2)

    panel.handleRightClick(0, 0)

    board.isFlagged(0, 0).shouldBe(true)
  }

  it should "play moves correctly from a loaded sequence" in {
    val board = new Board(2, 2, 2)
    val level =
      """-#
        |-#
        |""".stripMargin

    board.loadLevel(level)
    
    val panel = new BoardPanel(board, 2, 2)

    val sequence = Seq("L(1,1)", "D(2,2)")
    panel.playMoves(sequence)

    board.isRevealed(0, 0).shouldBe(true)
    board.isFlagged(1, 1).shouldBe(true)
  }

  it should "provide a hint and decrease score" in {
    val board = new Board(2, 2, 2)
    val panel = new BoardPanel(board, 2, 2)
    panel.resetCounters()

    panel.provideHint()

    panel.score.shouldBe(-10)
  }

  it should "update the board correctly based on the board map state" in {
    val board = new Board(2, 2, 2)
    board.getBoardMap(0)(0) = new Cell(isRevealed = true, neighborMines = 1)
    val panel = new BoardPanel(board, 2, 2)
    panel.updateBoard()

    panel.cellButtons(0)(0).text.shouldBe("1")
  }

  it should "reset game correctly and reset counters" in {
    val board = new Board(2, 2, 2)
    val panel = new BoardPanel(board, 2, 2)
    panel.resetCounters()

    panel.resetGame()

    board.isGameFinished.shouldBe(false)
    panel.clickCount.shouldBe(0)
  }
}