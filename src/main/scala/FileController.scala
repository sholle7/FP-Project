import java.io.{File, PrintWriter}
import scala.io.Source

object FileController {
  private val highScoresFile = new File("src/saves/scores/high_scores.txt")
  private val savedMapsFile = new File("src/saves/savedMaps/saved_map.txt")

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

  def saveMap(boardMap: Array[Array[Cell]]): Unit = {
    val writer = new PrintWriter(savedMapsFile)
    try {
      for {
        row <- boardMap.indices
        col <- boardMap(row).indices
      } {
        val cell = boardMap(row)(col)
        writer.println(s"$row,$col,${cell.isMine},${cell.isRevealed},${cell.isFlagged},${cell.neighborMines}")
      }
    } finally {
      writer.close()
    }
  }

  def loadSavedMap(): Array[Array[Cell]] = {
    if (savedMapsFile.exists()) {
      val source = Source.fromFile(savedMapsFile)
      try {
        val lines = source.getLines().toSeq
        val cells = lines.map { line =>
          val parts = line.split(",")
          val row = parts(0).toInt
          val col = parts(1).toInt
          val isMine = parts(2).toBoolean
          val isRevealed = parts(3).toBoolean
          val isFlagged = parts(4).toBoolean
          val neighborMines = parts(5).toInt
          (row, col, new Cell(isMine, isRevealed, isFlagged, neighborMines))
        }

        val rows = cells.map(_._1).max + 1
        val cols = cells.map(_._2).max + 1
        val boardMap = Array.ofDim[Cell](rows, cols)

        cells.foreach { case (row, col, cell) =>
          boardMap(row)(col) = cell
        }

        boardMap
      } finally {
        source.close()
      }
    } else {
      Array.empty[Array[Cell]]
    }
  }
}