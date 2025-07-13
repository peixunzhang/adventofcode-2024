package aoc

import better.files.Resource
import scala.collection.immutable

object Day8 {
  def example = Resource.getAsString("day8/example.txt")
  def real = Resource.getAsString("day8/real.txt")
  def fake = Resource.getAsString("day8/fake.txt")

  type Coordinate = (Int, Int)
  type Vec = (Int, Int)

  def parse(str: String): Grid = {
    val raw = str.split("\n").toList.map(_.toList)
    val antennas = raw.zipWithIndex
      .flatMap { case (row, rowIndex) =>
        row.zipWithIndex.collect {
          case (antenna, colIndex) if antenna != '.' =>
            antenna -> (rowIndex, colIndex)
        }
      }
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).toSet)
      .toList

    Grid(antennas, raw.size, raw.head.size)
  }

  final case class Grid(
      antennas: List[(Char, Set[Coordinate])],
      rowSize: Int,
      columnSize: Int
  )

  def isAntinode(possibleAntinode: Coordinate, antanne: Set[Coordinate]): Boolean = {
    antanne.exists { case (antennaRow, antennaCol) =>
      val distanceR = (antennaRow - possibleAntinode._1)
      val distanceC = (antennaCol - possibleAntinode._2)
      val possblieAnntent = (possibleAntinode._1+ distanceR*2, possibleAntinode._2 + distanceC*2)
      antanne.contains(possblieAnntent)
    }
  }

  def findAntinodes(grid: Grid): List[(Char, Coordinate)] = {
    val maybeAnti =
      for {
        row <- 0 until grid.rowSize
        col <- 0 until grid.columnSize
        result <- grid.antennas.collect { 
          case (char, indices) if isAntinode((row, col), indices) => (char, (row, col))
        }
      } yield result

    maybeAnti.toList
  }

  def findNonOverlappingAntinodes(grid: Grid): Set[Coordinate] = {    
    val allAntinode = findAntinodes(grid)

    allAntinode.filterNot { case (antenna, cords) =>
      val overlapWithSameAntenna = grid.antennas.filter(_._1 == antenna).exists(_._2.contains(cords))
      overlapWithSameAntenna
    }.map(_._2).toSet
  }

  // part2



  def findVector(current: Coordinate, target: Coordinate): Vec = (target._1 - current._1, target._2 - current._2)

  def allVectors(target: Coordinate, antannes: Set[Coordinate]): Set[Vec] = {
    antannes.collect{ case next if (next!=target) => findVector(target, next)}
  }

  def addVector(vector: Vec, start: Coordinate): Coordinate = (vector._1+start._1, vector._2+ start._2)

  def antinodes(vec: Vec, start: Coordinate, rowSize: Int, columnSize: Int): Set[Coordinate] = {
    val result = scala.collection.mutable.Set.empty[Coordinate]
    var current: Coordinate = start
    var done = false
    while (!done) {
      val maybeAntinode = addVector(vec, current)
      if (stillIn(maybeAntinode, rowSize, columnSize)) {
        current = maybeAntinode
        result += maybeAntinode
      } else {
        done = true
      }
    }
    result.toSet
  }

  def stillIn(possibleAntinode: Coordinate, rowSize: Int, columnSize: Int): Boolean = 
    possibleAntinode._1 >= 0 && possibleAntinode._1 < rowSize && possibleAntinode._2 >= 0 && possibleAntinode._2 < columnSize 

  def collectAntinodes(antennas: Set[Coordinate], rowSize: Int, columnSize: Int): Set[Coordinate] = {
    antennas.flatMap { antenna => 
      val vectors = allVectors(antenna, antennas)
      vectors.flatMap(antinodes(_, antenna, rowSize, columnSize))
    }
  }


  def solvePart2(data: String): Int = {
    val grid = parse(data)

    grid.antennas.flatMap{ case (c, antennaGroup) => collectAntinodes(antennaGroup, grid.rowSize, grid.columnSize)}.toSet.size

  }


  def solvePart1(data: String): Int = {
    findNonOverlappingAntinodes(parse(data)).size
  }
}
