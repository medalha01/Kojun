package kojun.solve

import cats.effect.IO
import data_structures.data.Tabuleiros._

type Tabuleiro = List[List[Int]]
type TabuleiroMapeado = List[List[Vetor]]
type Vetor = (Int, Int)
type Valor = Int

def mapearElemento(
    n: Int,
    valorRegiao: regiaoMapeada,
    linha: TabuleiroMapeado
): List[Int] = {
  linha.zipWithIndex.map { case (element, index) =>
    if (index == n) valorRegiao
    else element
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

def mapearRegioes(
    regioesTabuleiro: Tabuleiro,
    regioesMapeadas: TabuleiroMapeado,
    tamanho: Int
): TabuleiroMapeado = {
  val coordenadas = for {
    i <- 0 until tamanho
    j <- 0 until tamanho
  } yield (i, j)

  coordenadas.foldLeft(regioesMapeadas) { (acc, coord) =>
    mapearRegiao(regioesTabuleiro, coord, acc)
  }
}

def qtdeRegioes(regioesTabuleiro: Tabuleiro): Int = {
  val maxRegionId = regioesTabuleiro.flatten.max
  maxRegionId + 1
}

def mapearTabuleiro(
    regioesTabuleiro: Tabuleiro,
    tamanho: Int
): TabuleiroMapeado = {
  val regioesMapeadas =
    List.fill[List[Vetor]](qtdeRegioes(regioesTabuleiro))(List.empty)
  mapearRegioes(regioesTabuleiro, regioesMapeadas, tamanho)
}

def tamanhoRegiao(regioesMapeadas: TabuleiroMapeado, idRegiao: Int): Int = {
  regioesMapeadas(idRegiao).length
}

def formatarResultado(tabuleiroResolvido: Tabuleiro): String = {
  if (tabuleiroResolvido == null || tabuleiroResolvido.flatten.forall(_ == 0)) {
    "Não há solução para esse Tabuleiro"
  } else {
    tabuleiroResolvido.map(_.mkString(" ")).mkString("\n")
  }
}

def verificarColunaRegiao(
    valorPosicao: Valor,
    i: Int,
    j: Int,
    tamanho: Int,
    valoresTabuleiro: Tabuleiro,
    regioesTabuleiro: Tabuleiro
): Boolean = {
  val sameRegion = (i1: Int, j1: Int, i2: Int, j2: Int) =>
    regioesTabuleiro(i1)(j1) == regioesTabuleiro(i2)(j2)
  val isLower = (i1: Int, j1: Int, i2: Int, j2: Int) =>
    valoresTabuleiro(i1)(j1) < valoresTabuleiro(i2)(j2)
  val isUpper = (i1: Int, j1: Int, i2: Int, j2: Int) =>
    valoresTabuleiro(i1)(j1) > valoresTabuleiro(i2)(j2)

  val isNotLowerNeighbor =
    !((i - 1 >= 0) && sameRegion(i - 1, j, i, j) && isLower(i - 1, j, i, j))
  val isNotUpperNeighbor =
    !((i + 1 < tamanho) && sameRegion(i + 1, j, i, j) && isUpper(
      i + 1,
      j,
      i,
      j
    ))

  isNotLowerNeighbor && isNotUpperNeighbor
}

def verificarValorAdjacentes(
    valorPosicao: Valor,
    i: Int,
    j: Int,
    tamanho: Int,
    valoresTabuleiro: Tabuleiro
): Boolean = {
  val isNotRightNeighbor =
    (j + 1 >= tamanho) || valoresTabuleiro(i)(j + 1) != valorPosicao
  val isNotLeftNeighbor =
    (j - 1 < 0) || valoresTabuleiro(i)(j - 1) != valorPosicao
  val isNotBottomNeighbor =
    (i + 1 >= tamanho) || valoresTabuleiro(i + 1)(j) != valorPosicao
  val isNotTopNeighbor =
    (i - 1 < 0) || valoresTabuleiro(i - 1)(j) != valorPosicao

  isNotRightNeighbor && isNotLeftNeighbor && isNotBottomNeighbor && isNotTopNeighbor
}

def verificarValorRegiao(
    valorPosicao: Valor,
    valoresTabuleiro: Tabuleiro,
    regiaoMapeada: List[Vetor]
): Boolean = {
  !regiaoMapeada.exists { case (i, j) =>
    valoresTabuleiro(i)(j) == valorPosicao
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
    val tabuleiroAtualizado = ocuparPosicao(
      valMax,
      i,
      j,
      tamanho,
      valoresTabuleiro,
      regioesTabuleiro,
      regioesMapeadas
    )

    if (
      !tabuleiroAtualizado.isEmpty && !tabuleiroAtualizado.exists(
        _.exists(_ != 0)
      )
    ) {
      tabuleiroAtualizado
    } else {
      resolverTabuleiro(
        i,
        j + 1,
        tamanho,
        valoresTabuleiro,
        regioesTabuleiro,
        regioesMapeadas
      )
    }
  }
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

  val checkRegion =
    verificarValorRegiao(valorPosicao, valoresTabuleiro, regiaoMapeada)
  val checkAdjacent =
    verificarValorAdjacentes(valorPosicao, i, j, tamanho, valoresTabuleiro)
  val checkColumn = verificarColunaRegiao(
    valorPosicao,
    i,
    j,
    tamanho,
    valoresTabuleiro,
    regioesTabuleiro
  )

  checkRegion && checkAdjacent && checkColumn
}

def atualizarTabuleiro(
    valor: Valor,
    i: Int,
    j: Int,
    valoresTabuleiro: Tabuleiro
): Tabuleiro = {
  val linha = valoresTabuleiro(i)
  val linhaAtualizada = valoresTabuleiro(i).updated(j, valor)

  valoresTabuleiro.updated(i, linhaAtualizada)
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
    List.fill(tamanho, tamanho)(0) // Return an empty board
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

      if (!tabuleiro.isEmpty && !tabuleiro.forall(_.forall(_ == 0))) {
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

def kojun(
    valoresTabuleiro: Tabuleiro,
    regioesTabuleiro: Tabuleiro,
    tamanho: Int
): IO[String] = IO {
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
