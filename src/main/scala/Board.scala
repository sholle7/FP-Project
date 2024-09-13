import scala.util.Random

class Board(var rows: Int, var cols: Int, var mines: Int) {
  private val finishedGame = false
  private val boardMap: Array[Array[Cell]] = Array.ofDim[Cell](rows, cols)

  for (row <- 0 until rows) {
    for (col <- 0 until cols) {
      boardMap(row)(col) = new Cell(isMine = false)
    }
  }

  def loadLevel(fileContent: String): Unit = {
    val lines = fileContent.split("\n").filter(_.nonEmpty)

    if (lines.length != rows) {
      throw new IllegalArgumentException(s"Invalid level data. Expected $rows rows but got ${lines.length}")
    }

    for (row <- 0 until rows) {
      val line = lines(row)
      if (line.length != cols) {
        throw new IllegalArgumentException(s"Invalid level data. Expected $cols columns but got ${line.length} in row $row")
      }

      for (col <- 0 until cols) {
        boardMap(row)(col) = line(col) match {
          case '-' => new Cell(isMine = false, neighborMines = numberOfNeighborMines(row, col))
          case '#' => new Cell(isMine = true)
          case _ => throw new IllegalArgumentException(s"Unexpected character at row $row, col $col")
        }
      }
    }
  }

  def getCellNeighbors(row: Int, col: Int): List[Cell] = {
    val neighbors = for {
      i <- -1 to 1
      j <- -1 to 1
      if i != 0 || j != 0
      if row + i >= 0 && row + i < rows
      if col + j >= 0 && col + j < cols
    } yield boardMap(row + i)(col + j)

    neighbors.toList
  }

  def placeRandomMines(): Unit = {
    val random = new Random
    var minesPlaced = 0

    while (minesPlaced < mines) {
      val randomRow = random.nextInt(rows)
      val randomCol = random.nextInt(cols)

      if (!boardMap(randomRow)(randomCol).isMine) {
        boardMap(randomRow)(randomCol).isMine = true
        minesPlaced += 1
      }
    }
  }

  def numberOfNeighborMines(row: Int, col: Int): Int = {
    val neighbors = getCellNeighbors(row, col)
    neighbors.count(_.isMine)
  }

  def revealMines(): Unit = {
    for {
      row <- 0 until rows
      col <- 0 until cols
    } {
      val cell = boardMap(row)(col)
      if (cell.isMine) {
        cell.isRevealed = true
      }
    }
  }

  def revealCell(row: Int, col: Int): Unit = {
    val cell = boardMap(row)(col)
    if (!cell.isRevealed) {
      cell.isRevealed = true
    }
  }

  def getCell(row: Int, col: Int): Cell = boardMap(row)(col)
  def getBoardMap: Array[Array[Cell]] = boardMap
  def isMine(row: Int, col: Int): Boolean = boardMap(row)(col).isMine
  def isRevealed(row: Int, col: Int): Boolean = boardMap(row)(col).isRevealed
  def isFlagged(row: Int, col: Int): Boolean = boardMap(row)(col).isFlagged
  def isGameFinished: Boolean = finishedGame
}