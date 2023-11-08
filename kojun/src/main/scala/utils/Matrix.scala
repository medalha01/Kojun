package utils

class Matrix[A](private val rows: List[List[A]]) {

  /** Returns the number of rows in the matrix. */
  def numRows: Int = rows.length

  /** Returns the number of columns in the matrix. */
  def numCols: Int = rows.headOption.map(_.length).getOrElse(0)

  /** Converts the matrix to a list of lists. */
  def toList: List[List[A]] = rows

  def createMatrix(rows: Int, cols: Int, defaultValue: A): Matrix[A] = {
    val matrix = List.fill(rows)(List.fill(cols)(defaultValue))
    new Matrix(matrix)
  }

  /** Gets the value at a given position in the matrix. */
  def getValueAt(position: (Int, Int)): A = {
    require(
      isValidPosition(position),
      s"Invalid position in the matrix: $position"
    )
    val (row, col) = position
    rows(row)(col)
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

  /** Checks if a given position is valid within the matrix. */
  def isValidPosition(position: (Int, Int)): Boolean = {
    val (row, col) = position
    row >= 0 && row < numRows && col >= 0 && col < numCols
  }
}
