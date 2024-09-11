import scala.swing._
import scala.swing.event._

object Minesweeper extends SimpleSwingApplication {
  def top: Frame = new MainFrame {
    title = "Minesweeper"

    val button1: Button = new Button {
      text = "Button 1"
    }

    val button2: Button = new Button {
      text = "Button 2"
    }

    contents = new BoxPanel(Orientation.Vertical) {
      contents += button1
      contents += button2
      border = Swing.EmptyBorder(30, 30, 30, 30)
    }

    listenTo(button1, button2)

    reactions += {
      case ButtonClicked(`button1`) =>
        println("Button 1 clicked!")

      case ButtonClicked(`button2`) =>
        println("Button 2 clicked!")
    }

    size = new Dimension(300, 200)
  }
}