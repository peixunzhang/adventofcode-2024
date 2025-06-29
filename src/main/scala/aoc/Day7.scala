package aoc

import better.files.Resource
import scala.collection.immutable

object Day7 {
  val example = Resource.getAsString("day7/example.txt")
  val real = Resource.getAsString("day7/real.txt")


  def parse(str: String): List[(Long, List[Long])] = str.split("\n").toList.map(parseEquation(_))

  def parseEquation(str: String): (Long, List[Long]) = {
    val equation = str.split(": ")
    val result = equation.head.toLong
    val numbers = equation.tail.flatMap(_.split(" ").map(_.toLong)).toList
    (result, numbers)
  }

  def isEqual(result: Long, numbers: List[Long]): Boolean = {
    numbers match {
      case x :: xs => 
        val allPossibels = xs.foldLeft(List(x)){case (acc, next) => acc.flatMap(calculate(_, next))}
        allPossibels.contains(result)
      case immutable.Nil => false
    }
  }
 def calculate(x: Long, y: Long): List[Long] = {
  List(x+y, x*y)
 }

// part 2
 def isEqualPT(result: Long, numbers: List[Long]): Boolean = {
    numbers match {
      case x :: xs => 
        val allPossibels = xs.foldLeft(List(x)){case (acc, next) => acc.flatMap(calculatePT(_, next))}
        allPossibels.contains(result)
      case immutable.Nil => false
    }
  }
 def calculatePT(x: Long, y: Long): List[Long] = {
  List(x+y, x*y, s"$x$y".toLong)
 }

 def solvePart1(data: String): Long = parse(data).collect{ case (target, nums) if (isEqual(target, nums)) => target}.sum
 def solvePart2(data:String): Long = parse(data).collect{ case (target, nums) if (isEqualPT(target, nums)) => target}.sum
}
