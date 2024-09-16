import scala.io.Source
import scala.swing.*
import scala.swing.event.*
import java.io.File

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
      val chooser = new FileChooser(new File("./src/saves/savedMaps"))

      if (chooser.showOpenDialog(null) == FileChooser.Result.Approve) {
        val selectedFile: File = chooser.selectedFile
        loadMapFromFile(selectedFile)

        updateMainPanel()
      }
    }

    private def loadMapFromFile(selectedFile: File): Unit = {
      val savedMap: Array[Array[Cell]] = FileController.loadSavedMap(selectedFile)
      val mineCount = savedMap.flatten.count(_.isMine)

      board = Some(new Board(savedMap.length, savedMap(0).length, mineCount))
      board.get.setBoardMap(savedMap)
    }

    private def createNewLevel(): Unit = {
      val chooser = new FileChooser(new File("./src/saves/savedMaps"))

      if (chooser.showOpenDialog(null) == FileChooser.Result.Approve) {
        val selectedFile: File = chooser.selectedFile
        loadMapFromFile(selectedFile)

        showCreateLevelPanel()
      }
    }

    private def showCreateLevelPanel(): Unit = {
      Swing.onEDT {
        val boardPanel = new GridPanel(board.get.rows, board.get.cols) {
          for {
            row <- 0 until board.get.rows
            col <- 0 until board.get.cols
          } {
            val cell = board.get.getCell(row, col)
            val label = new Label(if (cell.isMine) "#" else "-")
            contents += label
          }
        }

        val beginnerRadioButton = new RadioButton("Beginner")
        val intermediateRadioButton = new RadioButton("Intermediate")
        val expertRadioButton = new RadioButton("Expert")
        val difficultyGroup = new ButtonGroup(beginnerRadioButton, intermediateRadioButton, expertRadioButton)

        val difficulty = board.get.getDifficulty
        difficulty match {
          case "Beginner" => beginnerRadioButton.selected = true
          case "Intermediate" => intermediateRadioButton.selected = true
          case "Expert" => expertRadioButton.selected = true
        }

        val difficultyPanel = new BoxPanel(Orientation.Vertical) {
          contents += beginnerRadioButton
          contents += intermediateRadioButton
          contents += expertRadioButton
        }

        val addFirstRowButton = new Button("Add First Row")
        val addLastRowButton = new Button("Add Last Row")

        val removeFirstRowButton = new Button("Remove First Row")
        val removeLastRowButton = new Button("Remove Last Row")

        val addFirstColButton = new Button("Add First Column")
        val addLastColButton = new Button("Add Last Column")

        val removeFirstColButton = new Button("Remove First Column")
        val removeLastColButton = new Button("Remove Last Column")

        val toggleCellButton = new Button("Toggle Cell")
        val clearSectorButton = new Button("Clear Sector")
        val saveMapButton = new Button("Save Map")

        val controlPanel: GridBagPanel = new GridBagPanel {
          val c = new Constraints
          c.fill = GridBagPanel.Fill.Horizontal
          c.insets = new Insets(5, 5, 5, 5)

          c.gridx = 0
          c.gridy = 0
          layout(addFirstRowButton) = c

          c.gridy = 1
          layout(addLastRowButton) = c

          c.gridy = 2
          layout(addFirstColButton) = c

          c.gridy = 3
          layout(addLastColButton) = c

          c.gridy = 4
          layout(removeFirstRowButton) = c

          c.gridy = 5
          layout(removeLastRowButton) = c

          c.gridy = 6
          layout(removeFirstColButton) = c

          c.gridy = 7
          layout(removeLastColButton) = c

          c.gridy = 8
          layout(toggleCellButton) = c

          c.gridy = 9
          layout(clearSectorButton) = c

          c.gridy = 10
          layout(saveMapButton) = c
        }

        val createLevelPanel = new BorderPanel {
          layout(boardPanel) = BorderPanel.Position.Center
          layout(controlPanel) = BorderPanel.Position.East
          layout(difficultyPanel) = BorderPanel.Position.West
        }

        contents = createLevelPanel

        size = new Dimension(1200, 800)

        listenTo(addFirstRowButton, addLastRowButton, addFirstColButton, addLastColButton,
          removeFirstRowButton, removeLastRowButton, removeFirstColButton, removeLastColButton,
          toggleCellButton, clearSectorButton, saveMapButton)

        reactions += {
          case ButtonClicked(`addFirstRowButton`) =>
            board.get.addFirstRow()
            showCreateLevelPanel()

          case ButtonClicked(`addLastRowButton`) =>
            board.get.addLastRow()
            showCreateLevelPanel()

          case ButtonClicked(`addFirstColButton`) =>
            board.get.addFirstCol()
            showCreateLevelPanel()

          case ButtonClicked(`addLastColButton`) =>
            board.get.addLastCol()
            showCreateLevelPanel()

          case ButtonClicked(`removeFirstRowButton`) =>
            board.get.removeFirstRow()
            showCreateLevelPanel()

          case ButtonClicked(`removeLastRowButton`) =>
            board.get.removeLastRow()
            showCreateLevelPanel()

          case ButtonClicked(`removeFirstColButton`) =>
            board.get.removeFirstCol()
            showCreateLevelPanel()

          case ButtonClicked(`removeLastColButton`) =>
            board.get.removeLastCol()
            showCreateLevelPanel()

          case ButtonClicked(`toggleCellButton`) =>
            val form = new CoordinateInputForm("Enter row and column", Seq("Row:", "Column:"), Seq("0", "0"))
            form.show() match {
              case Some(Seq(rowStr, colStr)) =>
                val (row, col) = (rowStr.toInt, colStr.toInt)
                board.get.toggleCellType(row, col)
                showCreateLevelPanel()
              case None =>
                Dialog.showMessage(contents.head, "No input provided.")
            }

          case ButtonClicked(`clearSectorButton`) =>
            val form = new CoordinateInputForm(
              "Enter sector coordinates",
              Seq("Top-left row:", "Top-left column:", "Bottom-right row:", "Bottom-right column:"),
              Seq("0", "0", "0", "0")
            )
            form.show() match {
              case Some(Seq(topLeftRowStr, topLeftColStr, bottomRightRowStr, bottomRightColStr)) =>
                board.get.clearSector(topLeftRowStr.toInt, topLeftColStr.toInt, bottomRightRowStr.toInt, bottomRightColStr.toInt)
                showCreateLevelPanel()
              case None =>
                Dialog.showMessage(contents.head, "No input provided.")
            }

          case ButtonClicked(`saveMapButton`) =>
            val selectedDifficulty = if (beginnerRadioButton.selected) {
              "Beginner"
            } else if (intermediateRadioButton.selected) {
              "Intermediate"
            } else {
              "Expert"
            }

            if (board.get.isValid(selectedDifficulty)) {
              println(s"Saving map with difficulty: $selectedDifficulty")
              // TODO - save map
            } else {
              Dialog.showMessage(contents.head, "The map is not valid!", title = "Validation Error")
            }
        }
      }
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