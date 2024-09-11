import scala.swing._
import scala.swing.event._

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
        quit()
    }

    size = new Dimension(400, 300)

    centerOnScreen()
  }

  private def startNewGame(): Unit = {
    Dialog.showMessage(null, "Starting a new game...", "New Game")
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
}