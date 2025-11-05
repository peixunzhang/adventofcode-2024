package aoc
import better.files.Resource
import scala.collection.mutable
import scala.collection.immutable

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
    def makeVisited: Plant = Plant(plant, true)
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

    def markVisited(co: Coordinate): Unit = all(co._1)(co._2) = all(co._1)(co._2).makeVisited

    def getNeighbors(co: Coordinate): Set[Coordinate] = {
      val (row, col) = co
      Set(
        (row-1, col),    // top
        (row,   col-1),  // left
        (row,   col+1),  // right
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

  // part1

  def getPrice(subG: Set[Coordinate]): Int = {
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

  // part2
  sealed trait Edge {
    def forDeduplication: (Coordinate, Boolean) = this match {
      case HorizontalTop(from) => (from, true)
      case HorizontalBottom(from) => (from, true)
      case VerticalLeft(from) => (from, false)
      case VerticalRight(from) => (from, false)
    }
  }
  final case class HorizontalTop(from: Coordinate) extends Edge
  final case class HorizontalBottom(from: Coordinate) extends Edge
  final case class VerticalLeft(from: Coordinate) extends Edge
  final case class VerticalRight(from: Coordinate) extends Edge

  def getPlantEdge(co: Coordinate): List[Edge] = {
    val (row, col) = co
    List(
      HorizontalTop((row, col)),
      HorizontalBottom((row+1, col)),
      VerticalLeft((row, col)),
      VerticalRight((row, col+1))
    )
  }

  def getEdges(area: Set[Coordinate]): (List[HorizontalTop], List[HorizontalBottom], List[VerticalLeft], List[VerticalRight]) = {
    val allEdges = area.toList.flatMap(getPlantEdge(_)).groupBy(_.forDeduplication).collect{case (_, edge :: Nil) => edge}.toList
    val ht = allEdges.collect{case e:HorizontalTop => e}.sortBy(_.from._1)
    val hb = allEdges.collect{case e: HorizontalBottom => e}.sortBy(_.from._1)
    val vl = allEdges.collect{case e: VerticalLeft => e}.sortBy(_.from._2)
    val vr = allEdges.collect{case e: VerticalRight => e}.sortBy(_.from._2)

    (ht, hb, vl, vr)
  }

  def mergeEdges(edges: (List[HorizontalTop], List[HorizontalBottom], List[VerticalLeft], List[VerticalRight])) = {
    val (ht, hb, vl, vr) = edges

    def countContinuous(values: List[Int]): Int = {
      val sorted = values.sorted
      val merged = sorted.foldLeft(List.empty[List[Int]]){case (acc, next) => 
        acc match {
          case Nil => List(List(next))
          case (currentGroup @ (last :: _)) :: previousGroups => 
            if (next == last + 1) {
              (next :: currentGroup) :: previousGroups
            } else {
              List(next) :: acc
            }
          case Nil :: _ => throw new IllegalStateException("impossible")
        }
      }
      merged.size
    }

    val mergeHT = ht.groupBy(_.from._1).values.map(row => countContinuous(row.map(_.from._2))).sum
    
    val mergeHB = hb.groupBy(_.from._1).values.map{row => countContinuous(row.map(_.from._2))}.sum

    val mergeVL= vl.groupBy(_.from._2).values.map(col => countContinuous(col.map(_.from._1))).sum

    val mergeVR= vr.groupBy(_.from._2).values.map(col => countContinuous(col.map(_.from._1))).sum

    mergeVR + mergeVL + mergeHB + mergeHT

  }

  def countPrice(area: Set[Coordinate]): Int = mergeEdges(getEdges(area)) * area.size

  def priceWithMergedEdges(areas: List[Set[Coordinate]])= areas.map(countPrice(_)).sum

  def solvePart1(data: String): Int = parse(data).getAllArea().map(getPrice(_)).sum
  def solvePart2(data: String): Int = priceWithMergedEdges(parse(data).getAllArea())
}
