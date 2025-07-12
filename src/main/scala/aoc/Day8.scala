package aoc

import better.files.Resource
import scala.collection.immutable

object Day8 {
  val example = Resource.getAsString("day8/example.txt")
  val real = Resource.getAsString("day8/real.txt")
  val fake = Resource.getAsString("day8/fake.txt")

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
      antennas: List[(Char, Set[(Int, Int)])],
      rowSize: Int,
      columnSize: Int
  )

  def isAntinode(possibleAntinode: (Int, Int), antanne: Set[(Int, Int)]): Boolean = {
    antanne.exists { case (antennaRow, antennaCol) =>
      val distanceR = (antennaRow - possibleAntinode._1)
      val distanceC = (antennaCol - possibleAntinode._2)
      val possblieAnntent = (possibleAntinode._1+ distanceR*2, possibleAntinode._2 + distanceC*2)
      antanne.contains(possblieAnntent)
    }
  }

  def findAntinodes(grid: Grid): List[(Char, (Int, Int))] = {
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

  def findNonOverlappingAntinodes(grid: Grid): Set[(Int, Int)] = {    
    val allAntinode = findAntinodes(grid)

    allAntinode.filterNot { case (antenna, cords) =>
      val overlapWithSameAntenna = grid.antennas.filter(_._1 == antenna).exists(_._2.contains(cords))
      overlapWithSameAntenna
    }.map(_._2).toSet
  }

  def solvePart1(data: String): Int = {
    findNonOverlappingAntinodes(parse(data)).size
  }
}
