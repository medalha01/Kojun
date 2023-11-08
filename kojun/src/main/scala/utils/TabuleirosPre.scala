package utils

object TabuleirosPre {
  val matrixSize10 = 10
  val valueMatrixData10 = List(
    List(5, 0, 2, 0, 2, 0, 3, 1, 3, 1),
    List(0, 4, 0, 1, 0, 5, 0, 5, 0, 4),
    List(7, 5, 1, 7, 0, 0, 3, 1, 3, 0),
    List(0, 4, 0, 0, 0, 0, 0, 0, 0, 3),
    List(2, 0, 3, 4, 0, 2, 0, 0, 4, 0),
    List(5, 0, 2, 0, 6, 0, 0, 0, 0, 0),
    List(0, 1, 3, 0, 1, 0, 0, 4, 0, 3),
    List(6, 7, 0, 3, 0, 1, 4, 0, 0, 1),
    List(4, 0, 3, 0, 4, 0, 0, 0, 0, 3),
    List(0, 1, 0, 2, 0, 6, 2, 0, 2, 1)
  )

  val regionMatrixData10 = List(
    List("a", "b", "b", "b", "c", "c", "c", "c", "d", "d"),
    List("a", "a", "a", "b", "e", "e", "f", "f", "d", "f"),
    List("g", "g", "a", "e", "e", "h", "i", "f", "f", "f"),
    List("g", "g", "e", "e", "j", "h", "i", "i", "i", "k"),
    List("g", "g", "g", "e", "j", "j", "l", "k", "k", "k"),
    List("m", "m", "n", "n", "n", "j", "o", "o", "p", "p"),
    List("m", "m", "m", "n", "n", "q", "r", "s", "p", "p"),
    List("t", "t", "m", "n", "q", "q", "r", "s", "u", "u"),
    List("t", "t", "v", "v", "v", "v", "r", "s", "s", "w"),
    List("t", "t", "t", "v", "v", "v", "r", "r", "w", "w")
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
