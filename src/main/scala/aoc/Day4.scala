package aoc

import better.files.Resource
import scala.collection.immutable
import scala.collection.mutable.ListBuffer

object Day4 {
  val data = Resource
    .getAsString("day4/real.txt")
  def parse(str: String): Field = {
    val letters = data.split("\n").toVector.map(_.toVector)
    val nRows = letters.size
    val nCols = letters.head.size
    Field(letters, nRows, nCols)
  }

  val xmas = "XMAS"
  val samx = xmas.reverse

  def isMatch(str: String): Boolean = str == xmas | str == samx

  // check Horizontal for both
  def matchHorizontal(letters: List[String]): Int =
    letters.map(_.sliding(4).toList.count(isMatch(_))).sum

  // check vertical for both
  def matchVertical(letters: List[String]): Int = {
    // rotate by 90 degrees
    matchHorizontal(letters.map(_.toList).transpose.map(_.mkString))
  }

  def boxIsMatch(box: Box): Boolean = {
    val center = box(1)(1)
    val upLeft = box(0)(0)
    val upRight = box(0)(2)
    val downLeft = box(2)(0)
    val downRight = box(2)(2)
    val check1 = upLeft == 'M' && downRight == 'S'
    val check2 = upLeft == 'S' && downRight == 'M'
    val check3 = upRight == 'M' && downLeft == 'S'
    val check4 = upRight == 'S' && downLeft == 'M'

    (center == 'A' && ((check1 && check3) || (check2 && check4) || (check2 && check3) || (check1 && check4)))
  }

  type Box = Vector[Vector[Char]]

  final case class Field(
      rows: Vector[Vector[Char]],
      nRows: Int,
      nCols: Int
  ) { self =>
    def get(row: Int, col: Int): Char =
      rows(row)(col)

    def contains(row: Int, col: Int): Boolean =
      row >= 0 && row < nRows && col >= 0 && col < nCols

    def getLine(row: Int, col: Int, incRow: Int, incCol: Int): String = {
      val buffer = new StringBuilder()

      var currentRow = row
      var currentCol = col
      while (contains(currentRow, currentCol)) {
        buffer.addOne(get(currentRow, currentCol))
        currentRow += incRow
        currentCol += incCol
      }
      buffer.mkString
    }

    private def getRow(row: Int, col: Int): String = getLine(row, col, 0, 1)
    private def getCol(row: Int, col: Int): String = getLine(row, col, 1, 0)
    private def getDiagonalLeft(row: Int, col: Int): String =
      getLine(row, col, 1, 1)
    private def getDiagonalRight(row: Int, col: Int): String =
      getLine(row, col, -1, 1)

    // part 1
    def getAllLines: List[String] = {
      val rows = (0 to nRows).map(getRow(_, 0)).toList
      val cols = (0 to nCols).map(getCol(0, _)).toList
      val diaLeft = (0 to nRows)
        .map(getDiagonalLeft(_, 0))
        .toList ++ (1 to nCols).map(getDiagonalLeft(0, _)).toList
      val diaRight = (0 to nRows)
        .map(getDiagonalRight(_, 0))
        .toList ++ (1 to nCols).map(getDiagonalRight(nCols - 1, _)).toList
      rows ++ cols ++ diaRight.filter(_.size >= 4) ++ diaLeft.filter(
        _.size >= 4
      )
    }

    // part 2
    def getBox(row: Int, col: Int): Option[Box] = {
      if (row > 0 && col > 0 && row < nRows - 1 && col < nCols - 1) {
        val upLeft = get(row - 1, col - 1)
        val up = get(row - 1, col)
        val upRight = get(row - 1, col + 1)
        val left = get(row, col - 1)
        val center = get(row, col)
        val right = get(row, col + 1)
        val downLeft = get(row + 1, col - 1)
        val down = get(row + 1, col)
        val downRight = get(row + 1, col + 1)
        val box = Vector(
          Vector(upLeft, up, upRight),
          Vector(left, center, right),
          Vector(downLeft, down, downRight)
        )
        Some(box)
      } else None
    }

    def getAllBoxes: List[Box] = for {
      row <- (0 to nRows).toList
      col <- 0 to nCols
      box <- getBox(row, col)
    } yield box
  }

  def main(arg: Array[String]): Unit = {
    val part1Result = parse(data).getAllLines
      .map(_.sliding(4))
      .flatMap(_.toList)
      .count(isMatch(_))
    val part2 = parse(data).getAllBoxes.count(boxIsMatch)
    println(part2)
  }

}
