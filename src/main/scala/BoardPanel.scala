import java.awt.Color
import scala.swing.*
import scala.swing.event.*

class BoardPanel(board: Board, rows: Int, cols: Int) extends GridPanel(rows, cols) {
  background = Color.white
  hGap = 2
  vGap = 2

  private val buttons = Array.fill(rows, cols) {
    val btn = new Button {
      preferredSize = new Dimension(30, 30)
      background = Color.gray
    }
    listenTo(btn.mouse.clicks)
    btn
  }

  for {
    row <- 0 until rows
    col <- 0 until cols
  } {
    contents += buttons(row)(col)
  }

  reactions += {
    case e: MouseClicked =>
      for {
        row <- 0 until rows
        col <- 0 until cols
        if buttons(row)(col) == e.source
      } {
        if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1) {
          handleLeftClick(row, col)
        } else if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3) {
          handleRightClick(row, col)
        }
      }
  }

  private def handleLeftClick(row: Int, col: Int): Unit = {
    if (!board.isRevealed(row, col)) {
      board.revealCell(row, col)
      updateBoard()
      if (board.isMine(row, col)) {
        Dialog.showMessage(null, "You hit a mine!", "Game Over")
      } else if (board.isGameFinished) {
        Dialog.showMessage(null, "You won!", "Victory")
      }
    }
  }

  private def handleRightClick(row: Int, col: Int): Unit = {
    if (!board.isRevealed(row, col)) {
      board.revealCell(row, col)
      updateBoard()
    }
  }

  def updateBoard(): Unit = {
    for {
      row <- 0 until rows
      col <- 0 until cols
    } {
      if (board.isRevealed(row, col)) {
        buttons(row)(col).text = board.getAdjacentMines(row, col).toString
        buttons(row)(col).background = Color.white
      } else if (board.isFlagged(row, col)) {
        buttons(row)(col).text = "F"
        buttons(row)(col).background = Color.orange
      } else {
        buttons(row)(col).text = ""
        buttons(row)(col).background = Color.gray
      }
    }

    revalidate()
    repaint()
  }
}