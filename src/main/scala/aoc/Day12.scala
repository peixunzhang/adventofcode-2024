package aoc
import better.files.Resource
import scala.collection.mutable

object Day12 {
  def example = Resource.getAsString("day12/example.txt")
  def real = Resource.getAsString("day12/real.txt")

  def parse(input: String): Garden = {
    val garden = input.split("\n").toArray.map(l => l.toArray.map(Plant(_, false)))
    val nRows = garden.size
    val nCols = garden(0).size
    Garden(garden, nRows, nCols)
  }

  final case class Plant(plant: Char, visited: Boolean) {
    def visied: Plant = Plant(plant, true)
  }
  
  type Row = Int
  type Col = Int
  type Coordinate = (Row, Col)

  final case class Garden(
    all: Array[Array[Plant]],
    nRows: Int,
    ncols: Int
  ) { self =>

    def getPlant(co: Coordinate): Char = all(co._1)(co._2).plant

    def visited(co: Coordinate): Boolean = all(co._1)(co._2).visited

    def markVisited(co: Coordinate): Unit = all(co._1)(co._2) = all(co._1)(co._2).visied

    def getNeighbors(co: Coordinate): Set[Coordinate] = {
      val (row, col) = co
      Set(
        // (row-1, col-1),  // top-left
        (row-1, col),    // top
        // (row-1, col+1),  // top-right
        (row,   col-1),  // left
        (row,   col+1),  // right
        // (row+1, col-1),  // bottom-left
        (row+1, col),    // bottom
        // (row+1, col+1)   // bottom-right
      ).filter { case (row, col) => row >= 0 && col >= 0 && row < nRows && col < ncols}
    }

    def getAreaNMarkVisited(current: Coordinate): Set[Coordinate] = {
      val queue = mutable.Queue(current)
      val acc = mutable.Set.empty[Coordinate]
      val plantType = getPlant(current)
      while (queue.nonEmpty) {
        val next = queue.dequeue()
        if (getPlant(next) != plantType || visited(next))
        {}
        else
        {
          acc.add(next)
          markVisited(next)
          queue.enqueueAll(getNeighbors(next))
        }
      }
      acc.toSet
    }

    def getAllArea(): List[Set[Coordinate]] = (for {
        i <- 0 until nRows
        j <- 0 until ncols
        area = getAreaNMarkVisited((i, j))
        if (area.nonEmpty)
      } yield area
    ).toList
    override def toString() = all.map(_.map(p => if (p.visited) p.plant.toLower else p.plant).mkString).mkString("\n")
  }

  def getPrice(subG: Set[Coordinate]): Long = {
    def getFour(co: Coordinate): Set[Coordinate] = {
      val (row, col) = co
      Set(
        (row-1, col),    // top
        (row,   col-1),  // left
        (row,   col+1),  // right
        (row+1, col),    // bottom
      )
    }

    val perimeter = subG.toList.map(p => getFour(p).count(!subG.contains(_))).sum

    println(s"perimeter: $perimeter; size: ${subG.size}")
    perimeter * subG.size
  }

  def solvePart1(data: String): Long = parse(data).getAllArea().map(getPrice(_)).sum
}
