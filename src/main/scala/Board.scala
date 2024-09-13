import scala.util.Random

class Board(rows: Int, cols: Int, mines: Int) {
  private val board = Array.fill(rows, cols)(new Cell)

  def getCell(row: Int, col: Int): Cell = board(row)(col)

  def getRows: Int = rows

  def getCols: Int = cols

  def getMines: Int = mines

  def getBoard: Array[Array[Cell]] = board

  def loadLevel(fileContent: String): Unit = {
    val rows: Array[String] = fileContent.split("\n")
    val map: Array[Array[Char]] = rows.map(_.toCharArray)

    for (y <- map.indices; x <- map(y).indices) {
      if (map(y)(x) == '#') {
        this.board(y)(x).isMine = true
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
    } yield board(row + i)(col + j)

    neighbors.toList
  }

  def placeRandomMines(): Unit = {
    val random = new Random
    var minesPlaced = 0

    while (minesPlaced < mines) {
      val randomRow = random.nextInt(rows)
      val randomCol = random.nextInt(cols)

      if (!board(randomRow)(randomCol).isMine) {
        board(randomRow)(randomCol).isMine = true
        minesPlaced += 1
      }
    }
  }

  def calculateNeighborMines(): Unit = {
    for {
      row <- 0 until rows
      col <- 0 until cols
    } {
      val cell = board(row)(col)
      val neighbors = getCellNeighbors(row, col)
      val neighborMines = neighbors.count(_.isMine)
      cell.neighborMines = neighborMines
    }
  }

  def revealMines(): Unit = {
    for {
      row <- 0 until rows
      col <- 0 until cols
    } {
      val cell = board(row)(col)
      if (cell.isMine) {
        cell.isRevealed = true
      }
    }
  }

  def revealCell(row: Int, col: Int): Unit = {
    val cell = board(row)(col)
    if (!cell.isRevealed) {
      cell.isRevealed = true
    }
  }
}