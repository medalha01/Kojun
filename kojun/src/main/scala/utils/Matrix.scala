package utils

class Matrix[A](private val rows: List[List[A]]) {
  def numRows: Int = rows.length

  def numCols: Int = if (rows.isEmpty) 0 else rows.head.length

  def matrixToList: List[List[A]] = rows

  def createMatrix(rows: Int, cols: Int, defaultValue: A): Matrix[A] = {
    val matrix = List.fill(rows)(List.fill(cols)(defaultValue))
    new Matrix(matrix)
  }

  def getMatrixValue(position: (Int, Int)): A = {
    val (row, col) = position
    if (isValidPosition(position)) {
      rows(row)(col)
    } else {
      throw new IllegalArgumentException(
        s"Invalid position in the matrix: $position"
      )
    }
  }

  def setMatrixValue(position: (Int, Int), value: A): Matrix[A] = {
    val (row, col) = position
    if (isValidPosition(position)) {
      val updatedRow = rows(row).updated(col, value)
      new Matrix(rows.updated(row, updatedRow))
    } else {
      throw new IllegalArgumentException(
        "Invalid position for setting value in the matrix"
      )
    }
  }

  def printMatrix(): Unit = {
    rows.foreach(row => println(row.mkString(" ")))
  }

  def isValidPosition(position: (Int, Int)): Boolean = {
    val (row, col) = position
    row >= 0 && row < numRows && col >= 0 && col < numCols
  }
}
