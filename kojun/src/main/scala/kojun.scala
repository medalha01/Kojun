package kojun.solve

def mapearElemento(n: Int, valorRegiao: Int, linha: List[Int]): List[Int] = {
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
    Array.fill[List[Vetor]](qtdeRegioes(regioesTabuleiro))(List.empty)

  mapearRegioes(regioesTabuleiro, regioesMapeadas, tamanho)
}

def tamanhoRegiao(regioesMapeadas: List[List[Vetor]], idRegiao: Int): Int = {
  regioesMapeadas(idRegiao).length
}

def formatarResultado(tabuleiroResolvido: Tabuleiro): String = {
  if (tabuleiroResolvido == null || tabuleiroResolvido.flatten.forall(_ == 0)) {
    "Não há solução para esse Tabuleiro"
  } else {
    tabuleiroResolvido.map(_.mkString(" ")).mkString("\n")
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
