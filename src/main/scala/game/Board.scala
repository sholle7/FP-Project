package game

import isometry.Isometry

import scala.util.boundary.break
import scala.util.{Random, boundary}

class Board(var rows: Int, var cols: Int, var mines: Int) {
  private var boardMap: Array[Array[Cell]] = Array.ofDim[Cell](rows, cols)

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
          case '-' => new Cell(isMine = false)
          case '#' => new Cell(isMine = true)
          case _ => throw new IllegalArgumentException(s"Unexpected character at row $row, col $col")
        }
      }
    }

    calculateNeighborMines()
  }

  def calculateNeighborMines(): Unit = {
    for {
      row <- 0 until rows
      col <- 0 until cols
      if !boardMap(row)(col).isMine
    } {
      val cell = boardMap(row)(col)
      cell.neighborMines = numberOfNeighborMines(row, col)
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

      if (cell.neighborMines == 0) {
        for {
          i <- -1 to 1
          j <- -1 to 1
          if i != 0 || j != 0
          newRow = row + i
          newCol = col + j
          if newRow >= 0 && newRow < rows
          if newCol >= 0 && newCol < cols
        } {
          revealCell(newRow, newCol)
        }
      }
    }
  }

  def flagCell(row: Int, col: Int): Unit = {
    val cell = boardMap(row)(col)
    cell.isFlagged = !cell.isFlagged
  }

  def isGameFinished: Boolean = {
    val allNonMinesRevealed = boardMap.flatten.count(cell => !cell.isMine && cell.isRevealed) == (rows * cols - mines)
    val allMinesFlagged = boardMap.flatten.count(cell => cell.isMine && cell.isFlagged) == mines

    allNonMinesRevealed || allMinesFlagged
  }

  def resetGame(): Unit = {
    for {
      row <- 0 until rows
      col <- 0 until cols
    } {
      boardMap(row)(col).isRevealed = false
      boardMap(row)(col).isFlagged = false
    }
  }

  def getHint: (Int, Int) = {
    boundary:
      for {
        row <- 0 until rows
        col <- 0 until cols
      } {
        val cell = boardMap(row)(col)
        if (!cell.isMine && !cell.isRevealed && !cell.isFlagged) {
          break ((row, col))
        }
      }
      (-1, -1)
  }

  def setBoardMap(newBoardMap: Array[Array[Cell]]): Unit = {
    for {
      row <- 0 until rows
      col <- 0 until cols
    } {
      boardMap(row)(col) = newBoardMap(row)(col)
    }
  }

  def addFirstRow(): Unit = {
    rows += 1
    val newRow = Array.fill(cols)(new Cell(isMine = false))
    boardMap = newRow +: boardMap
  }

  def addLastRow(): Unit = {
    rows += 1
    val newRow = Array.fill(cols)(new Cell(isMine = false))
    boardMap = boardMap :+ newRow
  }

  def removeFirstRow(): Unit = {
    if (rows > 1) {
      rows -= 1
      boardMap = boardMap.tail
    }
  }

  def removeLastRow(): Unit = {
    if (rows > 1) {
      rows -= 1
      boardMap = boardMap.dropRight(1)
    }
  }

  def addFirstCol(): Unit = {
    cols += 1
    boardMap = boardMap.map { row =>
      Array(new Cell(isMine = false)) ++ row
    }
  }

  def addLastCol(): Unit = {
    cols += 1
    boardMap = boardMap.map { row =>
      row :+ new Cell(isMine = false)
    }
  }

  def removeFirstCol(): Unit = {
    if (cols > 1) {
      cols -= 1
      boardMap = boardMap.map(_.tail)
    }
  }

  def removeLastCol(): Unit = {
    if (cols > 1) {
      cols -= 1
      boardMap = boardMap.map(_.dropRight(1))
    }
  }

  def toggleCellType(row: Int, col: Int): Unit = {
    boardMap(row)(col).isMine = !boardMap(row)(col).isMine
  }

  def clearSector(topLeftRow: Int, topLeftCol: Int, bottomRightRow: Int, bottomRightCol: Int): Unit = {
    for {
      row <- topLeftRow to bottomRightRow
      col <- topLeftCol to bottomRightCol
    } {
      boardMap(row)(col).isMine = false
    }
  }

  def isValid(difficulty: String): Boolean = {
    if (difficulty == "Beginner") {
      if (rows != 9 || cols != 9 || mines != 10) {
        return false
      }
    } else if (difficulty == "Intermediate") {
      if (rows != 16 || cols != 16 || mines != 40) {
        return false
      }
    } else if (difficulty == "Expert") {
      if (rows != 16 || cols != 30 || mines != 99) {
        return false
      }
    } else {
      return false
    }

    true
  }

  def getDifficulty: String = {
    if (rows == 9 && cols == 9 && mines == 10) {
      "Beginner"
    } else if (rows == 16 && cols == 16 && mines == 40) {
      "Intermediate"
    } else if (rows == 16 && cols == 30 && mines == 99) {
      "Expert"
    } else {
      "Custom"
    }
  }

  def applyIsometry(isometry: Isometry, sector: (Int, Int, Int, Int)): Unit = {
    boardMap = isometry.apply(this, sector).boardMap
  }

  def applyTransformation(topLeftRow: Int, topLeftCol: Int, transformedSector: Array[Array[Cell]]): Unit = {
    for {
      row <- transformedSector.indices
      col <- transformedSector(row).indices
    } {
      boardMap(topLeftRow + row)(topLeftCol + col) = transformedSector(row)(col)
    }
  }

  def getSector(topLeftRow: Int, topLeftCol: Int, bottomRightRow: Int, bottomRightCol: Int): Array[Array[Cell]] = {
    if (topLeftRow < 0 || topLeftCol < 0 || bottomRightRow >= rows || bottomRightCol >= cols ||
      topLeftRow > bottomRightRow || topLeftCol > bottomRightCol) {
      throw new IllegalArgumentException("Invalid sector coordinates")
    }

    val sectorRows = bottomRightRow - topLeftRow + 1
    val sectorCols = bottomRightCol - topLeftCol + 1

    val sector: Array[Array[Cell]] = Array.ofDim[Cell](sectorRows, sectorCols)

    for (i <- 0 until sectorRows; j <- 0 until sectorCols) {
      sector(i)(j) = boardMap(topLeftRow + i)(topLeftCol + j)
    }

    sector
  }

  /*
    * Returns a deep copy of the board.
   */
  def copy(): Board = {
    val newBoard = new Board(rows, cols, mines)
    val newBoardMap = Array.ofDim[Cell](rows, cols)

    for {
      row <- 0 until rows
      col <- 0 until cols
    } {
      newBoardMap(row)(col) = boardMap(row)(col).copy()
    }

    newBoard.setBoardMap(newBoardMap)
    newBoard
  }

  def copy(newRows: Int, newCols: Int): Board = {
    val newBoard = new Board(newRows, newCols, mines)

    for (row <- 0 until Math.min(rows, newRows)) {
      for (col <- 0 until Math.min(cols, newCols)) {
        newBoard.setCell(row, col, getCell(row, col).copy())
      }
    }

    newBoard
  }

  def setCell(row: Int, col: Int, cell: Cell): Unit = {
    boardMap(row)(col) = cell
  }

  def getBoardMap: Array[Array[Cell]] = boardMap
  def getCell(row: Int, col: Int): Cell = boardMap(row)(col)
  def isMine(row: Int, col: Int): Boolean = boardMap(row)(col).isMine
  def isRevealed(row: Int, col: Int): Boolean = boardMap(row)(col).isRevealed
  def isFlagged(row: Int, col: Int): Boolean = boardMap(row)(col).isFlagged
}