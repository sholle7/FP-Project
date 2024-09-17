package isometry

import game.{Board, Cell}

trait Transparency {
  def applyTransparency(original: Array[Array[Cell]], transformed: Array[Array[Cell]]): Array[Array[Cell]]
}

trait Expandability {
  def expandMapIfNeeded(original: Board, transformed: Board, startX: Int, startY: Int): Board
}

trait Isometry {
  def apply(board: Board, sector: (Int, Int, Int, Int)): Board
  def inverse: Isometry
}