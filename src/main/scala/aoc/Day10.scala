package aoc

import better.files.Resource
import scala.collection.immutable
import scala.annotation.tailrec
import aoc.Day2.allowOneBadLevel

object Day10 {
  def example = Resource.getAsString("day10/example.txt")
  def real = Resource.getAsString("day10/real.txt")

  def parse(input: String): HikingMap = {
    val fullMap = input.split("\n").toVector.map(str => str.map(_.asDigit).toVector)
    val nRows = fullMap.size
    val nCols = fullMap(0).size
    HikingMap(fullMap, nRows, nCols)
  }

  type Height = Int
  type Row = Int
  type Col = Int
  type Coordinate = (Row, Col)

    final case class HikingMap(
    full: Vector[Vector[Height]],
    nRows: Int,
    nCols: Int
  ) { self => 
    
    def get(coor: Coordinate): Height = full(coor._1)(coor._2)

    def isStart(coor: Coordinate): Boolean = get(coor._1, coor._2) == 0
    def isEnd(coor: Coordinate): Boolean = get(coor._1, coor._2) == 9
    
    def getNext(current: Coordinate, visited: Set[Coordinate]): Set[Coordinate] = {

      def isNext(current: Coordinate, neighbor: Coordinate): Boolean = 
        (neighbor._1 >= 0 && neighbor._1 < nRows) && 
        (neighbor._2 >= 0 && neighbor._2 < nCols) && 
        (! isStart(neighbor._1, neighbor._2)) &&
        (get(neighbor._1, neighbor._2) - get(current._1, current._2) == 1)
        
      val up: Coordinate = ((current._1-1), (current._2))
      val down: Coordinate = ((current._1+1), (current._2))
      val right: Coordinate = ((current._1), (current._2+1))
      val left: Coordinate = ((current._1), (current._2-1))
      val neighbors = List(up, down, right, left)

      neighbors.filter(coo => isNext(current, coo) && !visited.contains(coo)).toSet
      
    }

    val allTrailHeads: List[Coordinate] = ( for {
        row <- 0 until(nRows)
        col <- 0 until(nCols)
        if isStart(row, col)
      } yield (row, col)).toList
    
    // part 1: number of distinct 9 cells reachable from a trailhead.
    def getHikingPath(start: Coordinate): Int = {
      def go(start: Coordinate, acc: Set[Coordinate], visited: Set[Coordinate]): Set[Coordinate] = {
        if (isEnd(start)) {
          Set(start)
        } else {
          val nexts = getNext(start, visited + start)
          nexts.flatMap(go(_, acc + start, visited ++ acc.toSet)).toSet
        }
      }
      go(start, Set.empty, Set.empty).size
    }
    def countScores: Int = allTrailHeads.map(getHikingPath(_)).sum


    //part 2: number of distinct hiking trails starting at a trailhead.
    def rating(start: Coordinate): Int = {
      def go(start: Coordinate, acc: Int, visited: Set[Coordinate]): Int = {
        if (isEnd(start)) {
          acc + 1
        } else {
          val nexts = getNext(start, visited + start)
          nexts.foldLeft(acc)((count, next) => go(next, count, visited + next))
        }
      }
      go(start, 0, Set.empty)
    }

    def countAllRating: Int = allTrailHeads.map(rating(_)).sum

    
  } 

  def solvePart1(data: String): Int = parse(data).countScores
  def solvePart2(data: String): Int = parse(data).countAllRating
}
