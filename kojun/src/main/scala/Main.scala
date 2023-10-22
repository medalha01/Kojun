object Main extends App {
  import scala.io.StdIn.readLine
  import data_structures.data.Tabuleiros._

  println(
    "Digite o tamanho do tabuleiro que quer que seja resolvido 6, 8 ou 10:"
  )
  val input = readLine()
  val tamanho = input.toInt

  println("\n")
  tamanho match {
    case 6 =>
      val resultadoIO =
        kojun(getValoresTabuleiro6x6, getRegioesTabuleiro6x6, tamanho)
      println(resultadoIO)
    case 8 =>
      val resultadoIO =
        kojun(getValoresTabuleiro8x8, getRegioesTabuleiro8x8, tamanho)
      println(resultadoIO)
    case 10 =>
      val resultadoIO =
        kojun(getValoresTabuleiro10x10, getRegioesTabuleiro10x10, tamanho)
      println(resultadoIO)
    case _ =>
      println(
        "Não existe um tabuleiro desse tamanho no banco. Tente novamente."
      )
      Main.main(Array())
  }
}
