package utils

object TabuleirosPre {
  // Tabuleiro 15
  val matrixSize10 = 10
  val valueMatrixData10 = List(
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

  val regionMatrixData10 = List(
    List("a", "b", "b", "b", "b", "b", "c", "d", "e", "e"),
    List("a", "a", "b", "b", "c", "c", "c", "d", "d", "f"),
    List("a", "a", "g", "g", "g", "h", "i", "d", "d", "f"),
    List("g", "g", "g", "j", "j", "h", "i", "k", "l", "m"),
    List("g", "n", "n", "j", "o", "h", "i", "k", "l", "m"),
    List("n", "n", "o", "o", "o", "p", "i", "l", "l", "l"),
    List("n", "n", "o", "o", "p", "p", "i", "i", "i", "q"),
    List("r", "r", "o", "s", "p", "p", "p", "p", "q", "q"),
    List("r", "r", "s", "s", "s", "s", "s", "q", "q", "q"),
    List("t", "t", "t", "t", "s", "u", "u", "u", "u", "q")
  )
  val matrixSize8 = 8
  val valueMatrixData8 = List(
    List(2, 5, 0, 0, 3, 0, 0, 0),
    List(0, 0, 6, 0, 0, 0, 0, 0),
    List(0, 0, 5, 0, 5, 2, 0, 0),
    List(0, 0, 0, 2, 0, 0, 0, 0),
    List(0, 0, 1, 0, 4, 0, 0, 0),
    List(3, 0, 2, 0, 0, 4, 0, 0),
    List(0, 0, 0, 6, 0, 0, 0, 0),
    List(0, 0, 0, 0, 4, 0, 3, 2)
  )

  val regionMatrixData8 = List(
    List("a", "b", "b", "b", "b", "c", "d", "d"),
    List("a", "a", "e", "b", "c", "c", "f", "f"),
    List("g", "h", "e", "i", "c", "c", "j", "j"),
    List("g", "k", "e", "e", "e", "c", "j", "j"),
    List("g", "k", "e", "l", "l", "l", "j", "m"),
    List("n", "k", "o", "l", "o", "o", "m", "m"),
    List("n", "n", "o", "o", "o", "p", "p", "m"),
    List("n", "n", "q", "q", "p", "p", "p", "m")
  )

  val matrixSize = 6
  val valueMatrixData6 = List(
    List(0, 0, 4, 0, 2, 0),
    List(0, 0, 3, 0, 0, 0),
    List(1, 4, 0, 4, 0, 0),
    List(0, 5, 0, 0, 0, 2),
    List(0, 0, 0, 0, 3, 0),
    List(6, 2, 0, 2, 0, 5)
  )

  val regionMatrixData6 = List(
    List("a", "b", "b", "b", "c", "d"),
    List("a", "e", "b", "c", "c", "c"),
    List("a", "a", "f", "c", "g", "g"),
    List("h", "i", "f", "j", "j", "g"),
    List("h", "i", "i", "k", "k", "g"),
    List("i", "i", "i", "k", "k", "k")
  )

  def getMatrices(size: Int): Option[(List[List[Int]], List[List[String]])] = {
    size match {
      case 6  => Some((valueMatrixData6, regionMatrixData6))
      case 8  => Some((valueMatrixData8, regionMatrixData8))
      case 10 => Some((valueMatrixData10, regionMatrixData10))
      case _  => None // Invalid size
    }
  }

}
