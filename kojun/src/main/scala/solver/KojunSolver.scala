package solver

import utils.Matrix

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.language.postfixOps

class MyKojunSolver(valueMatrix: Matrix[Int], regionMatrix: Matrix[String]) {
  private type Position = (Int, Int)

  private val regionMapping: mutable.Map[String, List[(Int, Int)]] =
    createRegionMapping

  private def createRegionMapping: mutable.Map[String, List[(Int, Int)]] = {
    val regionMapping = mutable.Map[String, ListBuffer[(Int, Int)]]()

    for {
      row <- 0 until regionMatrix.numRows
      col <- 0 until regionMatrix.numCols
    } {
      val regionValue = regionMatrix.getMatrixValue((row, col))
      regionMapping.getOrElseUpdate(regionValue, ListBuffer()) += ((row, col))
    }

    val result = regionMapping.map { case (regionValue, positionsBuffer) =>
      regionValue -> positionsBuffer.toList
    }

    result
  }

  private def canInsertValue(
      position: Position,
      value: Int,
      valueMatrix: Matrix[Int],
      regionMatrix: Matrix[String],
      regionMapping: mutable.Map[String, List[(Int, Int)]]
  ): Boolean = {
    val (row, col) = position
    val regionValue = regionMatrix.getMatrixValue(position)
    val regionPositions = regionMapping(regionValue)

    val adjacentValues = List(
      (row - 1, col),
      (row + 1, col),
      (row, col - 1),
      (row, col + 1)
    ).filter(valueMatrix.isValidPosition).map(valueMatrix.getMatrixValue)

    val isTopValid =
      if (
        valueMatrix
          .isValidPosition((row - 1, col)) && regionMatrix.getMatrixValue(
          (row - 1, col)
        ) == regionValue
      ) {
        value < valueMatrix.getMatrixValue((row - 1, col))
      } else true

    val isBottomValid =
      if (
        valueMatrix
          .isValidPosition((row + 1, col)) && regionMatrix.getMatrixValue(
          (row + 1, col)
        ) == regionValue
      ) {
        value > valueMatrix.getMatrixValue((row + 1, col))
      } else true

    !(
      valueMatrix.getMatrixValue(position) != 0 ||
        value < 1 || value > regionPositions.length ||
        adjacentValues.contains(value) ||
        regionPositions.map(valueMatrix.getMatrixValue).contains(value) ||
        !isTopValid ||
        !isBottomValid
    )
  }

  @tailrec
  private def tryValues(
      row: Int,
      col: Int,
      valuesToTry: List[Int],
      valueMatrix: Matrix[Int],
      regionMatrix: Matrix[String],
      regionMapping: mutable.Map[String, List[(Int, Int)]]
  ): Option[Matrix[Int]] = {
    if (valuesToTry.isEmpty) {
      None
    } else {
      val value = valuesToTry.head
      if (
        canInsertValue(
          (row, col),
          value,
          valueMatrix,
          regionMatrix,
          regionMapping
        )
      ) {
        val updatedValueMatrix = valueMatrix.setMatrixValue((row, col), value)
        val nextPosition =
          if (col == valueMatrix.numCols - 1) (row + 1, 0) else (row, col + 1)
        solveKojun(
          updatedValueMatrix,
          regionMatrix,
          nextPosition,
          regionMapping
        ) match {
          case Some(solution) => Some(solution)
          case None =>
            tryValues(
              row,
              col,
              valuesToTry.tail,
              valueMatrix,
              regionMatrix,
              regionMapping
            )
        }
      } else {
        tryValues(
          row,
          col,
          valuesToTry.tail,
          valueMatrix,
          regionMatrix,
          regionMapping
        )
      }
    }
  }

  @tailrec
  private def solveKojun(
      valueMatrix: Matrix[Int],
      regionMatrix: Matrix[String],
      position: Position,
      regionMapping: mutable.Map[String, List[(Int, Int)]]
  ): Option[Matrix[Int]] = {
    val (row, col) = position
    if (row == valueMatrix.numRows) {
      Some(valueMatrix)
    } else if (col == valueMatrix.numCols) {
      solveKojun(valueMatrix, regionMatrix, (row + 1, 0), regionMapping)
    } else if (
      valueMatrix
        .isValidPosition(position) && valueMatrix.getMatrixValue(position) != 0
    ) {
      solveKojun(valueMatrix, regionMatrix, (row, col + 1), regionMapping)
    } else {
      val maxRegionSize = regionMapping(
        regionMatrix.getMatrixValue(position)
      ).length
      tryValues(
        row,
        col,
        (1 to maxRegionSize).toList,
        valueMatrix,
        regionMatrix,
        regionMapping
      )
    }
  }

  def solve(): Option[Matrix[Int]] = {
    solveKojun(valueMatrix, regionMatrix, (0, 0), regionMapping)
  }
}
