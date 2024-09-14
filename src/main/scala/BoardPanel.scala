import java.awt.Color
import java.io.File
import java.time.Instant
import scala.swing.*
import scala.swing.event.*

class BoardPanel(board: Board, rows: Int, cols: Int) extends BorderPanel {
  private var startTime: Option[Instant] = None
  private var clickCount: Int = 0
  private var score: Long = 0

  background = Color.white

  val loadSequenceButton: Button = new Button {
    text = "Load Sequence"
  }

  private val cellButtons = Array.fill(rows, cols) {
    val btn = new Button {
      preferredSize = new Dimension(20, 20)
      background = new Color(189, 189, 189)
    }
    listenTo(btn.mouse.clicks)
    btn
  }

  private val hintButton = new Button {
    text = "Hint"
  }

  private val saveButton = new Button {
    text = "Save"
  }

  private val topButtonPanel = new FlowPanel {
    contents += loadSequenceButton
    contents += hintButton
    contents += saveButton
  }

  private val gridPanel = new GridPanel(rows, cols) {
    hGap = 2
    vGap = 2
    background = Color.white

    for {
      row <- 0 until this.rows
      col <- 0 until cols
    } {
      contents += cellButtons(row)(col)
    }
  }

  layout(topButtonPanel) = BorderPanel.Position.North
  layout(gridPanel) = BorderPanel.Position.Center

  listenTo(loadSequenceButton, hintButton, saveButton)

  reactions += {
    case e: MouseClicked =>
      for {
        row <- 0 until rows
        col <- 0 until cols
        if cellButtons(row)(col) == e.source
      } {
        if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON1) {
          handleLeftClick(row, col)
        } else if (e.peer.getButton == java.awt.event.MouseEvent.BUTTON3) {
          handleRightClick(row, col)
        }
      }

    case ButtonClicked(`loadSequenceButton`) =>
      loadSequence()
    case ButtonClicked(`hintButton`) =>
      provideHint()
    case ButtonClicked(`saveButton`) =>
      saveGame()
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
        score += calculateScore(duration, clickCount)

        FileController.saveHighScore(score)

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

  private def loadSequence(): Unit = {
    val chooser = new FileChooser(new File("./src/saves/sequences"))

    if (chooser.showOpenDialog(null) == FileChooser.Result.Approve) {
      val selectedFile: File = chooser.selectedFile

      val sequence = FileController.loadSequence(selectedFile)
      playMoves(sequence)
    }
  }

  private def playMoves(sequence: Seq[String]): Unit = {
    for (line <- sequence) {
      val movePattern = """([LD])\((\d+),(\d+)\)""".r

      line match {
        case movePattern(action, rowStr, colStr) =>
          val row = rowStr.toInt - 1
          val col = colStr.toInt - 1

          action match {
            case "L" =>
              handleLeftClick(row, col)
            case "D" =>
              handleRightClick(row, col)
          }
        case _ =>
          println(s"Invalid move format: $line")
      }
    }
  }

  private def provideHint(): Unit = {
    val (hintRow, hintCol) = board.getHint

    if (hintRow != -1 && hintCol != -1) {
      Dialog.showMessage(null, s"Hint: Consider cell ($hintRow, $hintCol)", "Hint")
      score -= 10
    } else {
      Dialog.showMessage(null, "No hints available.", "Hint")
    }
  }

  private def saveGame(): Unit = {
    FileController.saveMap(board.getBoardMap)
  }

  def updateBoard(): Unit = {
    for {
      row <- 0 until rows
      col <- 0 until cols
    } {
      if (board.isRevealed(row, col)) {
        cellButtons(row)(col).text = board.getBoardMap(row)(col).neighborMines.toString
        cellButtons(row)(col).background = new Color(131, 131, 131)
      } else if (board.isFlagged(row, col)) {
        cellButtons(row)(col).text = "F"
        cellButtons(row)(col).background = Color.orange
      } else {
        cellButtons(row)(col).text = ""
        cellButtons(row)(col).background = new Color(189, 189, 189)
      }

      cellButtons(row)(col).opaque = true
      cellButtons(row)(col).revalidate()
      cellButtons(row)(col).repaint()
    }

    revalidate()
    repaint()
  }

  def resetCounters(): Unit = {
    clickCount = 0
    score = 0

    startTime = Some(Instant.now())
  }

  private def resetGame(): Unit = {
    board.resetGame()
    startTime = Some(Instant.now())
    clickCount = 0
    score = 0
    updateBoard()
  }

  private def calculateScore(duration: Long, clicks: Int): Long = {
    duration + clicks
  }
}