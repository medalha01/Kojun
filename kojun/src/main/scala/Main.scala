object Main extends App {
  import scala.io.StdIn.readLine
  import data_structures.data.Tabuleiros._

  val tabuleiroMap = Map(
    6 -> (getValoresTabuleiro6x6, getRegioesTabuleiro6x6),
    8 -> (getValoresTabuleiro8x8, getRegioesTabuleiro8x8),
    10 -> (getValoresTabuleiro10x10, getRegioesTabuleiro10x10)
  )

  println(
    "Digite o tamanho do tabuleiro que quer que seja resolvido 6, 8 ou 10:"
  )
  val input = readLine()
  val tamanho = input.toInt

  println("\n")

  tabuleiroMap.get(tamanho) match {
    case Some((valores, regioes)) =>
      val resultadoIO = kojun(valores, regioes, tamanho)
      println(resultadoIO)
    case None =>
      println(
        "Não existe um tabuleiro desse tamanho no banco. Tente novamente."
      )
      Main.main(Array())
  }
}
