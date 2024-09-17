package game

class Cell(var isMine: Boolean = false, var isRevealed: Boolean = false, var isFlagged: Boolean = false, var neighborMines: Int = 0) {
  def copy(): Cell = {
    new Cell(isMine, isRevealed, isFlagged, neighborMines)
  }
}