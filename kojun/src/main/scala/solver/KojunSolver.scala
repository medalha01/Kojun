// Definição do pacote ao qual a classe pertence
package solver

// Importações de classes e métodos necessários
import utils.Matrix
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable
import scala.language.postfixOps

// Definição da classe MyKojunSolver com duas matrizes como parâmetros: uma de valores e outra de regiões
class MyKojunSolver(valueMatrix: Matrix[Int], regionMatrix: Matrix[String]) {
  // Tipo Position é definido como um par de Inteiros para representar posições na matriz
  private type Position = (Int, Int)

  // regionMapping é um mapa mutável que liga strings (identificadores de região) a listas de posições
  private val regionMapping: mutable.Map[String, List[(Int, Int)]] =
    createRegionMapping

  // Método para criar o mapeamento das regiões da matriz de regiões para suas respectivas posições
  private def createRegionMapping: mutable.Map[String, List[(Int, Int)]] = {
    val regionMapping = mutable.Map[String, ListBuffer[(Int, Int)]]()

    // Loop para percorrer as células da matriz de regiões e preencher o mapeamento
    for {
      row <- 0 until regionMatrix.numRows
      col <- 0 until regionMatrix.numCols
    } {
      val regionValue = regionMatrix.getValueAt((row, col))
      regionMapping.getOrElseUpdate(regionValue, ListBuffer()) += ((row, col))
    }

    // Conversão de ListBuffer para List após o término do loop
    val result = regionMapping.map { case (regionValue, positionsBuffer) =>
      regionValue -> positionsBuffer.toList
    }

    result
  }

  // Método para verificar se é possível inserir um valor em uma determinada posição da matriz
  private def canInsertValue(
      position: Position,
      value: Int,
      valueMatrix: Matrix[Int],
      regionMatrix: Matrix[String],
      regionMapping: mutable.Map[String, List[(Int, Int)]]
  ): Boolean = {
    // Desconstrução da posição em linha e coluna
    val (row, col) = position
    // Obter o valor da região na posição atual
    val regionValue = regionMatrix.getValueAt(position)
    // Obter todas as posições na mesma região
    val regionPositions = regionMapping(regionValue)

    // Obter os valores adjacentes à posição atual, excluindo posições inválidas
    val adjacentValues = List(
      (row - 1, col),
      (row + 1, col),
      (row, col - 1),
      (row, col + 1)
    ).filter(valueMatrix.isValidPosition).map(valueMatrix.getValueAt)

    // Validar se o valor é menor que o da posição acima dentro da mesma região, se aplicável
    val isTopValid =
      if (
        valueMatrix.isValidPosition((row - 1, col)) && regionMatrix
          .getValueAt((row - 1, col)) == regionValue
      ) {
        value < valueMatrix.getValueAt((row - 1, col))
      } else true

    // Validar se o valor é maior que o da posição abaixo dentro da mesma região, se aplicável
    val isBottomValid =
      if (
        valueMatrix.isValidPosition((row + 1, col)) && regionMatrix
          .getValueAt((row + 1, col)) == regionValue
      ) {
        value > valueMatrix.getValueAt((row + 1, col))
      } else true

    // Retornar se todas as condições de validação são atendidas
    !(
      valueMatrix.getValueAt(
        position
      ) != 0 || // A posição atual deve estar vazia
        value < 1 || value > regionPositions.length || // O valor deve estar dentro do tamanho da região
        adjacentValues.contains(
          value
        ) || // O valor não deve ser igual a nenhum valor adjacente
        regionPositions
          .map(valueMatrix.getValueAt)
          .contains(value) || // O valor deve ser único na região
        !isTopValid || // Validação do topo
        !isBottomValid // Validação da base
    )
  }

  // Método recursivo para tentar inserir valores possíveis em uma posição da matriz
  @tailrec
  private def tryValues(
      row: Int,
      col: Int,
      valuesToTry: List[Int],
      valueMatrix: Matrix[Int],
      regionMatrix: Matrix[String],
      regionMapping: mutable.Map[String, List[(Int, Int)]]
  ): Option[Matrix[Int]] = {
    // Se não houver mais valores para tentar, retorna None
    if (valuesToTry.isEmpty) {
      None
    } else {
      // Pega o primeiro valor da lista para tentar
      val value = valuesToTry.head
      // Se for possível inserir o valor na posição atual
      if (
        canInsertValue(
          (row, col),
          value,
          valueMatrix,
          regionMatrix,
          regionMapping
        )
      ) {
        // Insere o valor na matriz
        val updatedValueMatrix = valueMatrix.setMatrixValue((row, col), value)
        // Determina a próxima posição para tentar
        val nextPosition =
          if (col == valueMatrix.numCols - 1) (row + 1, 0) else (row, col + 1)
        // Chama solveKojun recursivamente para resolver o próximo passo
        solveKojun(
          updatedValueMatrix,
          regionMatrix,
          nextPosition,
          regionMapping
        ) match {
          case Some(solution) =>
            Some(solution) // Se encontrou uma solução, retorna-a
          case None =>
            // Se não encontrou solução, tenta com o próximo valor
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
        // Se o valor atual não for válido, tenta o próximo
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

  // Método recursivo principal para resolver o quebra-cabeça
  @tailrec
  private def solveKojun(
      valueMatrix: Matrix[Int],
      regionMatrix: Matrix[String],
      position: Position,
      regionMapping: mutable.Map[String, List[(Int, Int)]]
  ): Option[Matrix[Int]] = {
    val (row, col) = position
    // Se chegou ao fim da matriz, retorna a matriz como solução
    if (row == valueMatrix.numRows) {
      Some(valueMatrix)
    } else if (col == valueMatrix.numCols) {
      // Se chegou ao fim da coluna, passa para a próxima linha
      solveKojun(valueMatrix, regionMatrix, (row + 1, 0), regionMapping)
    } else if (
      valueMatrix
        .isValidPosition(position) && valueMatrix.getValueAt(position) != 0
    ) {
      // Se a posição atual já tiver um valor, passa para a próxima coluna
      solveKojun(valueMatrix, regionMatrix, (row, col + 1), regionMapping)
    } else {
      // Determina o tamanho máximo da região para a posição atual
      val maxRegionSize = regionMapping(
        regionMatrix.getValueAt(position)
      ).length
      // Tenta valores de 1 até o tamanho máximo da região para a posição atual
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

  // Método público para resolver o quebra-cabeça; começa a resolução pela primeira posição da matriz
  def solve(): Option[Matrix[Int]] = {
    solveKojun(valueMatrix, regionMatrix, (0, 0), regionMapping)
  }
}
