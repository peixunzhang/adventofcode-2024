package aoc

import better.files.Resource
import math._

object Day1 {
  val data = Resource.getAsString("day1/example.txt").split("\n").toList.map(str => str.split("   ").map(_.toInt))
  def parse(data: List[Array[Int]]): List[(Int, Int)] = data match {
    case x :: xs => ((x(0), x(1))) :: parse(xs)
    case Nil => Nil 
  }

  def sortPair(nums: List[(Int, Int)], allLeft: List[Int] = Nil, allRight: List[Int] = Nil): List[(Int, Int)] = nums match {
    case x :: xs => sortPair(xs, x._1 :: allLeft, x._2 :: allRight)
    case Nil => allLeft.sorted.zip(allRight.sorted)
  }

  def getTotalDistance(newPairs: List[(Int, Int)]): Int = 
    newPairs match {
      case x :: xs => findDistance(x) + getTotalDistance(xs)
      case Nil => 0
    }

  def findDistance(pair: (Int, Int)): Int = 
      abs(pair._1 - pair._2)
  
// part 2
  def toLists(parsed: List[(Int, Int)], allLeft: List[Int] = Nil, allRight: List[Int] = Nil): (List[Int], Map[Int, Int]) = parsed match {
    case x :: xs => toLists(xs, x._1 :: allLeft, x._2 :: allRight)
    case Nil => (allLeft, allRight.foldLeft(Map.empty[Int, Int]){
      (acc, next) => acc + (next -> (acc.getOrElse(next, 0) + 1))
    })
  }

  def getScore(leftNum: Int, allRight: Map[Int, Int]): Int = allRight.getOrElse(leftNum, 0) * leftNum

  def findSimilarityScore(allLeft: List[Int], allRight: Map[Int, Int]): Long = 
    allLeft match {
      case x :: xs => getScore(x, allRight) + findSimilarityScore(xs, allRight)
      case Nil => 0L
    }

  def main(args: Array[String]): Unit = {
    val parsedData = parse(data)
    println("Part1:" + getTotalDistance(sortPair(parsedData)))
    println("Part2:" + findSimilarityScore(toLists(parsedData)._1, toLists(parsedData)._2))
  }
}
