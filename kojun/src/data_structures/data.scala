object Tabuleiros {

  // Type Definitions
  type Tabuleiro = Matriz[Valor]
  type TabuleiroMapeado = Matriz[Vetor]
  type Matriz[Elementos] = List[Linha[Elementos]]
  type Linha[Elementos] = List[Elementos]
  type Valor = Int
  type Vetor = (Int, Int)

  // Tabuleiro Definitions
  // Tabuleiros taken from the website: https://www.janko.at/Raetsel/Kojun/index.htm

  // Tabuleiro 6x6
  val getValoresTabuleiro6x6: Tabuleiro = List(
    List(0, 0, 0, 0, 0, 2),
    List(2, 0, 0, 5, 0, 0),
    List(0, 0, 3, 0, 0, 4),
    List(0, 0, 0, 3, 0, 1),
    List(0, 0, 0, 0, 0, 0),
    List(0, 0, 3, 0, 2, 5)
  )

  val getRegioesTabuleiro6x6: Tabuleiro = List(
    List(0, 1, 2, 2, 3, 3),
    List(0, 1, 4, 3, 3, 3),
    List(0, 0, 4, 4, 4, 5),
    List(6, 6, 7, 5, 5, 5),
    List(6, 6, 7, 8, 9, 9),
    List(10, 10, 8, 8, 8, 8)
  )

  // Tabuleiro 8x8
  val getValoresTabuleiro8x8: Tabuleiro = List(
    List(2, 0, 7, 0, 3, 0, 0, 0),
    List(0, 0, 0, 0, 0, 0, 1, 0),
    List(0, 4, 0, 4, 7, 6, 0, 0),
    List(0, 0, 0, 0, 1, 4, 1, 3),
    List(0, 0, 0, 6, 4, 3, 0, 0),
    List(0, 3, 0, 0, 0, 0, 2, 0),
    List(0, 0, 3, 2, 6, 0, 0, 3),
    List(1, 4, 0, 0, 0, 0, 3, 0)
  )

  val getRegioesTabuleiro8x8: Tabuleiro = List(
    List(0, 0, 0, 1, 1, 1, 2, 2),
    List(0, 0, 3, 3, 4, 4, 2, 5),
    List(6, 0, 7, 3, 8, 8, 8, 5),
    List(6, 0, 7, 3, 3, 8, 8, 8),
    List(9, 9, 10, 11, 11, 11, 11, 8),
    List(9, 9, 10, 13, 11, 11, 12, 12),
    List(14, 15, 13, 13, 13, 13, 16, 12),
    List(14, 14, 14, 13, 16, 16, 16, 12)
  )

  // Tabuleiro 10x10
  val getValoresTabuleiro10x10: Tabuleiro = List(
    List(0, 4, 3, 0, 2, 5, 0, 0, 0, 0),
    List(0, 2, 0, 0, 0, 4, 2, 0, 3, 0),
    List(0, 0, 0, 1, 4, 0, 0, 1, 0, 0),
    List(5, 6, 0, 2, 3, 0, 5, 0, 0, 0),
    List(0, 3, 5, 0, 0, 0, 3, 0, 0, 0),
    List(0, 0, 0, 7, 0, 7, 0, 5, 0, 4),
    List(0, 0, 5, 3, 0, 2, 0, 4, 0, 0),
    List(0, 0, 1, 5, 0, 0, 0, 5, 3, 0),
    List(1, 3, 7, 0, 0, 0, 6, 0, 0, 5),
    List(2, 1, 0, 0, 3, 0, 1, 0, 3, 4)
  )

  val getRegioesTabuleiro10x10: Tabuleiro = List(
    List(0, 1, 1, 1, 1, 1, 2, 3, 4, 4),
    List(0, 0, 1, 1, 2, 2, 2, 3, 3, 5),
    List(0, 0, 6, 6, 6, 7, 8, 3, 3, 5),
    List(6, 6, 6, 9, 9, 7, 8, 10, 11, 12),
    List(6, 13, 13, 9, 14, 7, 8, 10, 11, 12),
    List(13, 13, 14, 14, 14, 15, 8, 11, 11, 11),
    List(13, 13, 14, 14, 15, 15, 8, 8, 8, 16),
    List(17, 17, 14, 18, 15, 15, 15, 15, 16, 16),
    List(17, 17, 18, 18, 18, 18, 18, 16, 16, 16),
    List(19, 19, 19, 19, 18, 20, 20, 20, 20, 16)
  )
}
