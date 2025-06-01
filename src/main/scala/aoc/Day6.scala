package aoc

import better.files.Resource
import scala.annotation.tailrec

object Day6 {
  val data = Resource.getAsString("day6/real.txt")

  def makeItem(char: Char): Item = char match {
    case '.'     => Unvisited
    case '#'     => Blocker
    case '^'     => Guard(Up)
    case '<'     => Guard(LeftG)
    case '>'     => Guard(RightG)
    case 'v'     => Guard(Down)
    case 'X'     => Visited
    case _: Char => throw new Exception
  }

  sealed trait Item {
    override def toString() = {
      this match {
        case Blocker       => "B"
        case Guard(Up)     => "^"
        case Guard(Down)   => "v"
        case Guard(RightG) => ">"
        case Guard(LeftG)  => "<"
        case Unvisited     => "."
        case Visited       => "X"
      }
    }

    def guardDirection: Option[Direction] = this match {
      case Guard(d) => Some(d)
      case _        => None
    }
  }

  case object Unvisited extends Item
  case object Visited extends Item
  case object Blocker extends Item
  final case class Guard(facing: Direction) extends Item

  sealed trait Direction {
    def turn: Direction = {
      this match {
        case Up     => RightG
        case RightG => Down
        case Down   => LeftG
        case LeftG  => Up
      }
    }
  }
  case object Up extends Direction
  case object RightG extends Direction
  case object Down extends Direction
  case object LeftG extends Direction

  def parse(str: String): LabMap = {
    val lab = data.split("\n").toVector.map(_.toVector.map(makeItem))
    val allRows = lab.size
    val allCols = lab.head.size
    val (guardX, guardY, guardFacing) = (for {
      row <- 0 until allRows
      col <- 0 until allCols
      content = lab(row)(col)
      direction <- content.guardDirection
    } yield (row, col, direction)).head // could have more than 1 true cases

    LabMap(lab, guardX, guardY, guardFacing, allRows, allCols)
  }

  // part 1

  final case class LabMap(
      value: Vector[Vector[Item]],
      guardX: Int,
      guardY: Int,
      guardFacing: Direction,
      allRows: Int,
      allCols: Int
  ) {

    override def toString(): String = {
      s"""guard: ($guardX, $guardY)
           |facing: $guardFacing
           |
           |${value.map(_.mkString).mkString("\n")}""".stripMargin
    }

    def stillIn(x: Int, y: Int): Boolean = {
      x < allRows && x >= 0 && y < allCols && y >= 0
    }

    def get(x: Int, y: Int): Item = {
      value(x)(y)
    }

    def isBlocker(x: Int, y: Int): Boolean = get(x, y) == Blocker

    def giveVisited(): List[(Int, Int)] = {
      for {
        i <- 0 until this.allRows
        j <- 0 until this.allCols
        if get(i, j) == Visited
      } yield (i, j)
    }.toList

    def sameRoute(x: Int, y: Int): Boolean =
      get(x, y).guardDirection.fold(false)(_ == this.guardFacing)

    def countVisited(): Int =
      value.map(_.count(i => i == Visited)).sum

    def set(x: Int, y: Int, item: Item): LabMap = {
      val newValue = value.updated(x, value(x).updated(y, item))
      copy(value = newValue)
    }
    @tailrec
    def turnGuard(): (Int, LabMap) = {
      val (nextX, nextY) = guardFacing match {
        case Up     => (guardX - 1, guardY)
        case RightG => (guardX, guardY + 1)
        case Down   => (guardX + 1, guardY)
        case LeftG  => (guardX, guardY - 1)
      }
      if (!stillIn(nextX, nextY)) {
        val updateMap = this.set(guardX, guardY, Visited)
        (updateMap.countVisited(), updateMap)
      } else if (isBlocker(nextX, nextY)) {
        this.copy(guardFacing = guardFacing.turn).turnGuard
      } else {
        this
          .set(guardX, guardY, Visited)
          .set(nextX, nextY, Guard(guardFacing))
          .copy(guardX = nextX, guardY = nextY)
          .turnGuard
      }
    }

    def solvePart1(): Int = this.turnGuard()._1

    // part 2

    def solvePart2(): Int = {
      val (_, finishedMap) = turnGuard()
      val candidates = finishedMap.giveVisited()
      var n = 0
      candidates.count { case (r, c) =>
        println(s"Evaluating blocker ${n += 1; n}/${candidates.length}")
        set(r, c, Blocker).isCircle
      }
    }

    // cycle is detected (revisiting same position + direction)
    @tailrec
    def isCircle(): Boolean = {
      println("\n\n")
      println(this)

      val (nextX, nextY) = guardFacing match {
        case Up     => (guardX - 1, guardY)
        case RightG => (guardX, guardY + 1)
        case Down   => (guardX + 1, guardY)
        case LeftG  => (guardX, guardY - 1)
      }
      if (!stillIn(nextX, nextY)) false
      else if (isBlocker(nextX, nextY)) {
        copy(guardFacing = guardFacing.turn)
          .set(guardX, guardY, Guard(guardFacing))
          .isCircle
      } else if (sameRoute(nextX, nextY)) {
        true
      } else {
        set(guardX, guardY, Guard(guardFacing))
          .set(nextX, nextY, Guard(guardFacing))
          .copy(guardX = nextX, guardY = nextY)
          .isCircle
      }
    }
  }

}

object Day6Part2 {
  def main(args: Array[String]): Unit = {
    println(Day6.parse(Day6.data).solvePart2())
  }
}
