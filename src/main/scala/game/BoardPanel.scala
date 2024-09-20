package game

import java.awt.Color
import java.io.File
import java.time.Instant
import javax.swing.Timer
import scala.swing.*
import scala.swing.event.*

class BoardPanel(board: Board, rows: Int, cols: Int, canDialogBeShown: Boolean = true) extends BorderPanel {
  private var startTime: Option[Instant] = None
  var clickCount: Int = 0
  var hintScore: Int = 0
  private var flagCount: Int = board.mines
  private var timer: Timer = _
  private var elapsedSeconds: Int = 0

  background = Color.white

  private val flagCountLabel: Label = new Label(s"$flagCount")
  private val timerLabel: Label = new Label("0")

  private val smileyButton = new Button("☺") {
    reactions += {
      case ButtonClicked(_) => resetGame()
    }
  }

  private val topPanel = new FlowPanel {
    contents += flagCountLabel
    contents += smileyButton
    contents += timerLabel
  }

  private val loadSequenceButton: Button = new Button {
    text = "Load Sequence"
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

  val cellButtons: Array[Array[Button]] = Array.fill(rows, cols) {
    val btn = new Button {
      preferredSize = new Dimension(20, 20)
      background = new Color(189, 189, 189)
    }
    listenTo(btn.mouse.clicks)
    btn
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

  layout(topPanel) = BorderPanel.Position.North
  layout(topButtonPanel) = BorderPanel.Position.South
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

  def handleLeftClick(row: Int, col: Int): Unit = {
    if (board.isFlagged(row, col)) return

    if (!board.isRevealed(row, col)) {
      if (clickCount == 0) startTimer()

      clickCount += 1

      if (board.isMine(row, col)) {
        stopTimer()

        if (canDialogBeShown) {
          Dialog.showMessage(null, "You hit a mine!", "Game Over")
        }

        resetGame()
      } else {
        board.revealCell(row, col)
        updateBoard()
      }

      if (board.isGameFinished) {
        stopTimer()
        val score = calculateScore()
        FileController.saveHighScore(score)

        if (canDialogBeShown) {
          Dialog.showMessage(null, "You won! Score: " + score + " seconds", "Victory")
        }

        resetGame()
      }
    }
  }

  def handleRightClick(row: Int, col: Int): Unit = {
    if (!board.isRevealed(row, col)) {
      board.flagCell(row, col)
      flagCount -= 1
      flagCountLabel.text = s"$flagCount"
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

  def playMoves(sequence: Seq[String]): Unit = {
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

  def provideHint(): Unit = {
    val (hintRow, hintCol) = board.getHint

    if (hintRow != -1 && hintCol != -1) {
      hintScore += 2

      if (canDialogBeShown) {
        Dialog.showMessage(null, s"Hint: Consider cell ($hintRow, $hintCol)", "Hint")
      }

    } else {
      if (canDialogBeShown) {
        Dialog.showMessage(null, "No hints available.", "Hint")
      }
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
    flagCount = board.mines
    elapsedSeconds = 0
    flagCountLabel.text = s"$flagCount"
    timerLabel.text = s"$elapsedSeconds"
    stopTimer()
  }

  def resetGame(): Unit = {
    board.resetGame()
    resetCounters()
    updateBoard()
  }

  private def startTimer(): Unit = {
    startTime = Some(Instant.now())
    timer = new Timer(1000, Swing.ActionListener(_ => {
      elapsedSeconds += 1
      timerLabel.text = s"$elapsedSeconds"
    }))
    timer.start()
  }

  private def stopTimer(): Unit = {
    if (timer != null) {
      timer.stop()
    }
  }

  def calculateScore(): Long = {
    elapsedSeconds + clickCount + hintScore
  }
}