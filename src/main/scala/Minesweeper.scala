import scala.swing.event.*
import scala.swing.*

object Minesweeper extends SimpleSwingApplication {
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

    contents = new GridBagPanel {
      background = new Color(173, 216, 230)
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

    size = new Dimension(400, 300)

    centerOnScreen()
  }

  private def startNewGame(): Unit = {
    val levels = Seq("Beginner", "Intermediate", "Expert")

    val selectedLevel: Option[String] = Dialog.showInput(
      parent = top,
      message = "Select the game level:",
      title = "New Game",
      entries = levels,
      initial = levels.head
    )

    selectedLevel match {
      case Some("Beginner") => startGameWithLevelDifficulty("Beginner", rows = 9, cols = 9, mines = 10)
      case Some("Intermediate") => startGameWithLevelDifficulty("Intermediate", rows = 16, cols = 16, mines = 40)
      case Some("Expert") => startGameWithLevelDifficulty("Expert", rows = 30, cols = 16, mines = 99)
      case _ => println("No difficulty selected.")
    }
  }

  private def startGameWithLevelDifficulty(level: String, rows: Int, cols: Int, mines: Int): Unit = {
    println(s"New game started: '$level' with $rows x $cols table and $mines mines.")
    Dialog.showMessage(
      parent = top,
      message = s"Starting game on '$level' level with $rows x $cols grid and $mines mines.",
      title = "New Game"
    )
  }

  private def loadSavedGame(): Unit = {
    Dialog.showMessage(null, "Loading saved game...", "Load Game")
  }

  private def createNewLevel(): Unit = {
    Dialog.showMessage(null, "Creating a new level...", "Create Level")
  }

  private def viewHighScores(): Unit = {
    Dialog.showMessage(null, "Viewing high scores...", "High Scores")
  }

  private def exitGame(): Unit = {
    sys.exit(0)
  }
}