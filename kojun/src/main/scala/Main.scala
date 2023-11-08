import scala.io.Source
import solver.MyKojunSolver
import utils.Matrix

object Main {
  def main(args: Array[String]): Unit = {
    // Read files containing
    // The size N of the matrix (N x N) on the first line
    // The value matrix (occupying N lines)
    // The region matrix (occupying N lines)
    val filePath = "src/inputs/10x10/kojun_10.txt" // Relative path to the file

    // Open and read the file
    val fileContents = readFromFile(filePath)
    var valueMatrix: List[List[Int]] = List()
    var regionMatrix: List[List[String]] = List()

    fileContents match {
      case Some(contents) =>
        val matrixSize = contents.head.toInt
        // Read the value matrix (integers) and region matrix (characters)
        valueMatrix = contents
          .slice(1, matrixSize + 1)
          .map(line => line.split(" ").map(_.toInt).toList)
        regionMatrix = contents
          .slice(matrixSize + 1, matrixSize * 2 + 1)
          .map(line => line.split(" ").map(filterAlphabeticCharacters).toList)

      case None =>
        println("An error occurred while opening the file.")
        return
    }

    // Create matrices for values and regions
    val valueMatrixGrid = new Matrix[Int](valueMatrix)
    val regionMatrixGrid = new Matrix[String](regionMatrix)

    // Solve the puzzle
    val solver = new MyKojunSolver(valueMatrixGrid, regionMatrixGrid)
    val solution = solver.solve()

    // Print the solution
    solution match {
      case Some(solutionMatrix) =>
        println("\nSolution:")
        solutionMatrix.printMatrix()
      case None =>
        println("No solution found.")
    }
  }

  // Function to filter alphabetic characters from a string
  private def filterAlphabeticCharacters(str: String): String = {
    str.filter(_.isLetter)
  }

  private def readFromFile(filePath: String): Option[List[String]] = {
    try {
      val source = Source.fromFile(filePath)
      val lines = source.getLines.toList
      source.close()
      Some(lines)
    } catch {
      case e: Exception =>
        println(s"An error occurred: ${e.getMessage}")
        None
    }
  }
}
