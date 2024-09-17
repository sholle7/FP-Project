case class CompositeIsometry(isometries: List[Isometry]) extends Isometry {
  override def apply(board: Board, sector: (Int, Int, Int, Int)): Board = {
    isometries.foldLeft(board)((currentBoard, isometry) => isometry.apply(currentBoard, sector))
  }

  override def inverse: Isometry = CompositeIsometry(isometries.reverse.map(_.inverse))
}