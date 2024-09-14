import java.awt.Color
import java.time.Instant
import scala.swing.*
import scala.swing.event.*

class BoardPanel(board: Board, rows: Int, cols: Int) extends GridPanel(rows, cols) {
  private var startTime: Option[Instant] = None
  private var clickCount: Int = 0

  background = Color.white
  hGap = 2
  vGap = 2

  private val buttons = Array.fill(rows, cols) {
    val btn = new Button {
      preferredSize = new Dimension(20, 20)
      background = new Color(189, 189, 189)
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
    if (board.isFlagged(row, col)) {
      return
    }

    if (!board.isRevealed(row, col)) {
      clickCount += 1

      if (board.isMine(row, col)) {
        Dialog.showMessage(null, "You hit a mine!", "Game Over")
        resetGame()
      }
      else {
        board.revealCell(row, col)
        updateBoard()
      }

      if (board.isGameFinished) {
        val endTime = Instant.now()
        val duration = java.time.Duration.between(startTime.get, endTime).getSeconds
        val score = calculateScore(duration, clickCount)

        HighScoresController.saveHighScore(score)

        Dialog.showMessage(null, "You won! Score: " + score, "Victory")
        resetGame()
      }
    }
  }

  private def handleRightClick(row: Int, col: Int): Unit = {
    if (!board.isRevealed(row, col)) {
      clickCount += 1
      board.flagCell(row, col)
      updateBoard()
    }
  }

  def updateBoard(): Unit = {
    for {
      row <- 0 until rows
      col <- 0 until cols
    } {
      if (board.isRevealed(row, col)) {
        buttons(row)(col).text = board.getBoardMap(row)(col).neighborMines.toString
        buttons(row)(col).background = new Color(131, 131, 131)
      } else if (board.isFlagged(row, col)) {
        buttons(row)(col).text = "F"
        buttons(row)(col).background = Color.orange
      } else {
        buttons(row)(col).text = ""
        buttons(row)(col).background = new Color(189, 189, 189)
      }

      buttons(row)(col).opaque = true
      buttons(row)(col).revalidate()
      buttons(row)(col).repaint()
    }

    revalidate()
    repaint()
  }

  def resetCounters(): Unit = {
    clickCount = 0

    startTime = Some(Instant.now())
  }

  private def resetGame(): Unit = {
    board.resetGame()
    startTime = Some(Instant.now())
    clickCount = 0
    updateBoard()
  }

  private def calculateScore(duration: Long, clicks: Int): Long = {
    duration + clicks
  }
}