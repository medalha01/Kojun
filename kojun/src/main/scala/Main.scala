import scala.io.Source
import solver.MyKojunSolver
import utils.Matrix
import utils.TabuleirosPre

object Main {
  def main(args: Array[String]): Unit = {
    println("Enter the size (6, 8, or 10):")
    val size = scala.io.StdIn.readInt()
    // Create matrices for values and regions
    TabuleirosPre.getMatrices(size) match {
      case Some((valueMatrix, regionMatrix)) =>
        val valueMatrixGrid = new Matrix[Int](valueMatrix)
        val regionMatrixGrid = new Matrix[String](regionMatrix)
        val solver = new MyKojunSolver(valueMatrixGrid, regionMatrixGrid)
        val solution = solver.solve()

        // Print the solution
        solution match {
          case Some(solutionMatrix) =>
            println("\nSolution:")
            solutionMatrix.printMatrix()
          case None =>
            println("No solution found.")
          case None =>
            println("Invalid size. Please enter 6, 8, or 10.")
        }

      // Solve the puzzle

    }
  }

}
