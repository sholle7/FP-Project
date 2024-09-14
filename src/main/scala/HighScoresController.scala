import java.io.{File, PrintWriter}
import scala.io.Source

object HighScoresController {
  private val highScoresFile = new File("src/saves/scores/high_scores.txt")

  def saveHighScore(newScore: Long): Unit = {
    val highScores = loadHighScores() :+ newScore
    val topScores = highScores.sorted.take(5)
    val writer = new PrintWriter(highScoresFile)
    try {
      topScores.foreach(score => writer.println(score))
    } finally {
      writer.close()
    }
  }

  def loadHighScores(): Seq[Long] = {
    if (highScoresFile.exists()) {
      val source = Source.fromFile(highScoresFile)
      try {
        source.getLines().map(_.toLong).toSeq
      } finally {
        source.close()
      }
    } else {
      Seq.empty[Long]
    }
  }
}