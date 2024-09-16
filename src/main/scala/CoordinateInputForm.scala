import scala.swing._
import scala.swing.event._

class CoordinateInputForm(title: String, labelNames: Seq[String], initialValues: Seq[String]) {

  private val textFields: Seq[TextField] = initialValues.map { initialValue =>
    new TextField {
      text = initialValue
    }
  }

  private val formPanel = new GridPanel(labelNames.size, 2) {
    contents ++= labelNames.zip(textFields).flatMap { case (label, textField) =>
      Seq(new Label(label), textField)
    }
  }

  def show(): Option[Seq[String]] = {
    val result = Dialog.showConfirmation(
      parent = null,
      message = formPanel.peer,
      optionType = Dialog.Options.OkCancel,
      title = title
    )

    if (result == Dialog.Result.Ok) {
      Some(textFields.map(_.text))
    } else {
      None
    }
  }
}