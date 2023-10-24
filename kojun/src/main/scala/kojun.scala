package kojun.solve

import cats.effect.IO
import data_structures.data.Tabuleiros._

import scala.concurrent.ExecutionContext.Implicits.global

object KojunSolver {
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
      tabuleiroResolvido.map(_.mkString(" ")).mkString("\n")
    }
  }

  def mapearTabuleiro(
      regioesTabuleiro: Tabuleiro,
      tamanho: Int
  ): TabuleiroMapeado = {
    val qtdeRegioe = qtdeRegioes(regioesTabuleiro)
    val regioesMapeadas = List.fill(qtdeRegioe)(List())
    mapearRegioes(regioesTabuleiro, regioesMapeadas, tamanho)
  }

  def qtdeRegioes(regioesTabuleiro: Tabuleiro): Int = {
    regioesTabuleiro.flatten.max + 1
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

    coordenadas.foldLeft(regioesMapeadas) {
      case (mapeadoAtualizado, coordenada) =>
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
    mapearElemento(idRegiao, regiaoMapeada, regioesMapeadas)
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
      valoresTabuleiro
    } else if (j == tamanho) {
      resolverTabuleiro(
        i + 1,
        0,
        tamanho,
        valoresTabuleiro,
        regioesTabuleiro,
        regioesMapeadas
      )
    } else if (valoresTabuleiro(i)(j) > 0) {
      resolverTabuleiro(
        i,
        j + 1,
        tamanho,
        valoresTabuleiro,
        regioesTabuleiro,
        regioesMapeadas
      )
    } else {
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
      List(List())
    } else {
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
          tabuleiro
        } else {
          ocuparPosicao(
            valorPosicao - 1,
            i,
            j,
            tamanho,
            valoresTabuleiro,
            regioesTabuleiro,
            regioesMapeadas
          )
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
        )
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
    val leftValid =
      !(j - 1 >= 0) || (valoresTabuleiro(i)(j - 1) != valorPosicao)
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
    val lowerValid = !(i - 1 >= 0) ||
      (regioesTabuleiro(i - 1)(j) != regioesTabuleiro(i)(j)) ||
      (valoresTabuleiro(i - 1)(j) < valorPosicao)

    val upperValid = !(i + 1 < tamanho) ||
      (regioesTabuleiro(i + 1)(j) != regioesTabuleiro(i)(j)) ||
      (valoresTabuleiro(i + 1)(j) > valorPosicao)

    lowerValid && upperValid
  }
}
