package kojun.solve

import cats.effect.IO
import data_structures.data.Tabuleiros._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

def kojun(
    valoresTabuleiro: Tabuleiro,
    regioesTabuleiro: Tabuleiro,
    tamanho: Int
): String = {
  val regioesMapeadas = mapearTabuleiro(regioesTabuleiro, tamanho)
  val tabuleiroResolvido = resolverTabuleiro(
    0,
    0,
    tamanho,
    valoresTabuleiro,
    regioesTabuleiro,
    regioesMapeadas
  )

  formatarResultado(tabuleiroResolvido)
}

def formatarResultado(tabuleiroResolvido: Tabuleiro): String = {
  if (tabuleiroResolvido.isEmpty || tabuleiroResolvido.forall(_.isEmpty)) {
    "Não há solução para esse Tabuleiro"
  } else {
    tabuleiroResolvido
      .map(linha => linha.map(_.toString).mkString(" "))
      .mkString("\n")
  }
}

def mapearTabuleiro(
    regioesTabuleiro: Tabuleiro,
    tamanho: Int
): TabuleiroMapeado = {
  val regioesMapeadas = List.fill(qtdeRegioes(regioesTabuleiro))(List())
  mapearRegioes(regioesTabuleiro, regioesMapeadas, tamanho)
}

def qtdeRegioes(regioesTabuleiro: Tabuleiro): Int = {
  val maxRegionId = regioesTabuleiro.flatten.max
  maxRegionId + 1
}

def mapearRegioes(
    regioesTabuleiro: Tabuleiro,
    regioesMapeadas: TabuleiroMapeado,
    tamanho: Int
): TabuleiroMapeado = {
  val coordenadas = for {
    i <- 0 until tamanho
    j <- 0 until tamanho
  } yield (i, j)

  coordenadas.foldRight(regioesMapeadas) { (coordenada, mapeadoAtualizado) =>
    mapearRegiao(regioesTabuleiro, coordenada, mapeadoAtualizado)
  }
}

def mapearRegiao(
    regioesTabuleiro: Tabuleiro,
    vetor: Vetor,
    regioesMapeadas: TabuleiroMapeado
): TabuleiroMapeado = {
  val (i, j) = vetor
  val idRegiao = regioesTabuleiro(i)(j)
  val regiaoMapeada = vetor +: regioesMapeadas(idRegiao)
  val regiaoMapeadaAtualizada =
    mapearElemento(idRegiao, regiaoMapeada, regioesMapeadas)
  regiaoMapeadaAtualizada
}

def mapearElemento[A](n: Int, valorRegiao: A, linha: Linha[A]): Linha[A] = {
  if (n <= 0) {
    valorRegiao +: linha.tail
  } else if (linha.isEmpty) {
    linha
  } else {
    linha.head +: mapearElemento(n - 1, valorRegiao, linha.tail)
  }
}

def resolverTabuleiro(
    i: Int,
    j: Int,
    tamanho: Int,
    valoresTabuleiro: Tabuleiro,
    regioesTabuleiro: Tabuleiro,
    regioesMapeadas: TabuleiroMapeado
): Tabuleiro = {
  if (i == tamanho - 1 && j == tamanho) {
    valoresTabuleiro // If the entire matrix has been traversed and the problem is solved, return the solved board
  } else if (j == tamanho) {
    resolverTabuleiro(
      i + 1,
      0,
      tamanho,
      valoresTabuleiro,
      regioesTabuleiro,
      regioesMapeadas
    ) // If the entire row has been traversed, move to the next row and restart the process
  } else if (valoresTabuleiro(i)(j) > 0) {
    resolverTabuleiro(
      i,
      j + 1,
      tamanho,
      valoresTabuleiro,
      regioesTabuleiro,
      regioesMapeadas
    ) // If the position is already occupied, move to the next in the row
  } else {
    // Position is unoccupied
    // Search for a number to occupy it, starting from the largest number in the region (Region Size)
    val valMax = tamanhoRegiao(regioesMapeadas, regioesTabuleiro(i)(j))
    ocuparPosicao(
      valMax,
      i,
      j,
      tamanho,
      valoresTabuleiro,
      regioesTabuleiro,
      regioesMapeadas
    )
  }
}

def tamanhoRegiao(regioesMapeadas: TabuleiroMapeado, idRegiao: Int): Int = {
  regioesMapeadas(idRegiao).length
}

def ocuparPosicao(
    valorPosicao: Valor,
    i: Int,
    j: Int,
    tamanho: Int,
    valoresTabuleiro: Tabuleiro,
    regioesTabuleiro: Tabuleiro,
    regioesMapeadas: TabuleiroMapeado
): Tabuleiro = {
  if (valorPosicao <= 0) {
    List(
      List()
    ) // If it's not possible to occupy the position, the board is not solvable, return an empty board
  } else {
    // Check if it's possible to insert the value at that position
    if (
      valorPossivel(
        valorPosicao,
        i,
        j,
        tamanho,
        valoresTabuleiro,
        regioesTabuleiro,
        regioesMapeadas
      )
    ) {
      val tabuleiroAtualizado =
        atualizarTabuleiro(valorPosicao, i, j, valoresTabuleiro)
      val tabuleiro = resolverTabuleiro(
        i,
        j + 1,
        tamanho,
        tabuleiroAtualizado,
        regioesTabuleiro,
        regioesMapeadas
      )

      if (!tabuleiro.isEmpty && !tabuleiro.forall(_.isEmpty)) {
        tabuleiro // If the next iterations are successful, return the solved board up to this point
      } else {
        ocuparPosicao(
          valorPosicao - 1,
          i,
          j,
          tamanho,
          valoresTabuleiro,
          regioesTabuleiro,
          regioesMapeadas
        ) // If something goes wrong, try again with a smaller value at this position
      }
    } else {
      ocuparPosicao(
        valorPosicao - 1,
        i,
        j,
        tamanho,
        valoresTabuleiro,
        regioesTabuleiro,
        regioesMapeadas
      ) // If it's not possible, try again with a smaller value
    }
  }
}

def atualizarTabuleiro(
    valor: Valor,
    i: Int,
    j: Int,
    valoresTabuleiro: Tabuleiro
): Tabuleiro = {
  val linha = valoresTabuleiro(i)
  val linhaAtualizada = linha.updated(j, valor)
  valoresTabuleiro.updated(i, linhaAtualizada)
}

def valorPossivel(
    valorPosicao: Valor,
    i: Int,
    j: Int,
    tamanho: Int,
    valoresTabuleiro: Tabuleiro,
    regioesTabuleiro: Tabuleiro,
    regioesMapeadas: TabuleiroMapeado
): Boolean = {
  val regiaoMapeada = regioesMapeadas(regioesTabuleiro(i)(j))

  verificarValorRegiao(valorPosicao, valoresTabuleiro, regiaoMapeada) &&
  verificarValorAdjacentes(valorPosicao, i, j, tamanho, valoresTabuleiro) &&
  verificarColunaRegiao(
    valorPosicao,
    i,
    j,
    tamanho,
    valoresTabuleiro,
    regioesTabuleiro
  )
}

def verificarValorRegiao(
    valorPosicao: Valor,
    valoresTabuleiro: Tabuleiro,
    regiaoMapeada: Linha[Vetor]
): Boolean = {
  !regiaoMapeada.exists { case (i, j) =>
    valoresTabuleiro(i)(j) == valorPosicao
  }
}

def verificarValorAdjacentes(
    valorPosicao: Valor,
    i: Int,
    j: Int,
    tamanho: Int,
    valoresTabuleiro: Tabuleiro
): Boolean = {
  val rightValid =
    !(j + 1 < tamanho) || (valoresTabuleiro(i)(j + 1) != valorPosicao)
  val leftValid = !(j - 1 >= 0) || (valoresTabuleiro(i)(j - 1) != valorPosicao)
  val bottomValid =
    !(i + 1 < tamanho) || (valoresTabuleiro(i + 1)(j) != valorPosicao)
  val topValid = !(i - 1 >= 0) || (valoresTabuleiro(i - 1)(j) != valorPosicao)

  rightValid && leftValid && bottomValid && topValid
}

def verificarColunaRegiao(
    valorPosicao: Valor,
    i: Int,
    j: Int,
    tamanho: Int,
    valoresTabuleiro: Tabuleiro,
    regioesTabuleiro: Tabuleiro
): Boolean = {
  // Verifica se a posição inferior está no tabuleiro e na mesma região
  // Se sim, verifica se o valor dela é menor que o da posição
  val lowerValid = !(i - 1 >= 0) ||
    (regioesTabuleiro(i - 1)(j) != regioesTabuleiro(i)(j)) ||
    (valoresTabuleiro(i - 1)(j) < valorPosicao)

  // Verifica se a posição superior está no tabuleiro e na mesma região
  // Se sim, verifica se o valor dela é maior que o da posição
  val upperValid = !(i + 1 < tamanho) ||
    (regioesTabuleiro(i + 1)(j) != regioesTabuleiro(i)(j)) ||
    (valoresTabuleiro(i + 1)(j) > valorPosicao)

  lowerValid && upperValid
}
