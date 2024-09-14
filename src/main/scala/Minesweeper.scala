import java.io.File
import scala.swing.*
import scala.swing.event.*

object Minesweeper extends SimpleSwingApplication {
  private var board: Option[Board] = None

  private val backgroundColor = new Color(173, 216, 230)

  def top: Frame = new MainFrame {
    title = "Game Menu"

    val startNewGameButton: Button = new Button {
      text = "Start New Game"
    }

    val loadSavedGameButton: Button = new Button {
      text = "Load Saved Game"
    }

    val createNewLevelButton: Button = new Button {
      text = "Create New Level"
    }

    val viewHighScoresButton: Button = new Button {
      text = "View High Scores"
    }

    val exitButton: Button = new Button {
      text = "Exit"
    }

    val buttonPanel: GridBagPanel = new GridBagPanel {
      background = backgroundColor
      val c = new Constraints
      c.fill = GridBagPanel.Fill.Horizontal
      c.insets = new Insets(10, 10, 10, 10)
      c.gridx = 0
      c.gridy = 0
      layout(startNewGameButton) = c

      c.gridy = 1
      layout(loadSavedGameButton) = c

      c.gridy = 2
      layout(createNewLevelButton) = c

      c.gridy = 3
      layout(viewHighScoresButton) = c

      c.gridy = 4
      layout(exitButton) = c
    }

    val mainPanel: BorderPanel = new BorderPanel {
      layout(buttonPanel) = BorderPanel.Position.Center
    }

    contents = mainPanel

    listenTo(startNewGameButton, loadSavedGameButton, createNewLevelButton, viewHighScoresButton, exitButton)

    reactions += {
      case ButtonClicked(`startNewGameButton`) =>
        startNewGame()

      case ButtonClicked(`loadSavedGameButton`) =>
        loadSavedGame()

      case ButtonClicked(`createNewLevelButton`) =>
        createNewLevel()

      case ButtonClicked(`viewHighScoresButton`) =>
        viewHighScores()

      case ButtonClicked(`exitButton`) =>
        exitGame()
    }

    size = new Dimension(1200, 800)

    centerOnScreen()

    private def startNewGame(): Unit = {
      val difficulties = Seq("Beginner", "Intermediate", "Expert")

      val selectedDifficulty: Option[String] = Dialog.showInput(
        parent = top,
        message = "Select the game level:",
        title = "New Game",
        entries = difficulties,
        initial = difficulties.head
      )

      selectedDifficulty match {
        case Some(difficulty) =>
          difficulty match {
            case "Beginner" => selectMap(difficulty, 9, 9, 10)
            case "Intermediate" => selectMap(difficulty, 16, 16, 40)
            case "Expert" => selectMap(difficulty, 16, 30, 99)
          }
        case _ => println("No difficulty selected.")
      }
    }

    private def selectMap(difficulty: String, rows: Int, cols: Int, mines: Int): Unit = {
      val maps = Seq("Level1", "Level2", "Level3", "Random map")

      val selectedMap: Option[String] = Dialog.showInput(
        parent = top,
        message = "Select the map:",
        title = "Map Selection",
        entries = maps,
        initial = maps.head
      )

      selectedMap match {
        case Some("Random map") =>
          startRandomMap(difficulty, rows, cols, mines)
        case Some(map) if maps.contains(map) =>
          startGameWithDifficulty(difficulty, rows, cols, mines, map)
        case _ =>
          println("No map selected.")
      }
    }

    private def startRandomMap(difficulty: String, rows: Int, cols: Int, mines: Int): Unit = {
      board = Some(new Board(rows, cols, mines))
      board.get.placeRandomMines()

      updateMainPanel()
    }

    private def startGameWithDifficulty(difficulty: String, rows: Int, cols: Int, mines: Int, level: String): Unit = {
      val baseDirectory = new File("src/levels")
      val difficultyDirectory = new File(baseDirectory, difficulty.toLowerCase())
      val filePath = new File(difficultyDirectory, level.toLowerCase()).getPath
      val source = _root_.scala.io.Source.fromFile(filePath)
      val fileContent = try source.mkString finally source.close()

      board = Some(new Board(rows, cols, mines))

      println(s"File content of ${level.toLowerCase()}:\n$fileContent")

      board.get.loadLevel(fileContent)

      updateMainPanel()
    }

    private def updateMainPanel(): Unit = {
      val boardPanel = new BoardPanel(board.get, board.get.rows, board.get.cols)

      boardPanel.resetCounters()
      boardPanel.updateBoard()

      contents = new BorderPanel {
        layout(boardPanel) = BorderPanel.Position.Center
      }

      size = new Dimension(1200, 800)

      mainPanel.revalidate()
      mainPanel.repaint()
    }

    private def loadSavedGame(): Unit = {
      val savedMap: Array[Array[Cell]] = FileController.loadSavedMap()
      val mineCount = savedMap.flatten.count(_.isMine)

      board = Some(new Board(savedMap.length, savedMap(0).length, mineCount))
      board.get.setBoardMap(savedMap)

      updateMainPanel()
    }

    private def createNewLevel(): Unit = {
      Dialog.showMessage(null, "Creating a new level...", "Create Level")
    }

    private def viewHighScores(): Unit = {
      val highScores = FileController.loadHighScores()

      val highScoresText = highScores.zipWithIndex.map { case (score, index) =>
        s"${index + 1}) $score points"
      }.mkString("\n")

      val highScoresTitleTextArea = new TextArea {
        text = s"Top 5 High Scores:\n\n"
        editable = false
        background = backgroundColor
        font = new Font("Arial", Font.Bold.id, 20)
      }

      val highScoresTextArea = new TextArea {
        text = s"$highScoresText"
        editable = false
        background = backgroundColor
        font = new Font("Arial", Font.Plain.id, 20)
      }

      val centeredPanel: GridBagPanel = new GridBagPanel {
        background = backgroundColor
        val c = new Constraints
        c.fill = GridBagPanel.Fill.Horizontal
        c.insets = new Insets(10, 10, 10, 10)
        c.gridx = 0
        c.gridy = 0
        layout(highScoresTitleTextArea) = c

        c.gridy = 1
        layout(highScoresTextArea) = c
      }

      contents = new BorderPanel {
        background = backgroundColor
        layout(centeredPanel) = BorderPanel.Position.Center
      }

      size = new Dimension(1200, 800)
      mainPanel.revalidate()
      mainPanel.repaint()
    }

    private def exitGame(): Unit = {
      sys.exit(0)
    }
  }
}