package aoc
import better.files.Resource
import scala.annotation.tailrec
import scala.collection.mutable
import aoc.Day11.real

object Day11{
  def example = Resource.getAsString("day11/example.txt")
  def real = Resource.getAsString("day11/real.txt")

  def parse(input: String): List[Stone] = input.split("\\s+").toList.map(i => Stone(i.toLong))


 final case class Stone(n: Long) {

  def splitInMid: List[Stone] = {
    val s = n.toString
    val half = s.length/2
    val (left, right) = s.splitAt(half)
    List(Stone(left.toLong), Stone(right.toLong))
  }

  def otherCase: Stone = Stone(n * 2024)

  def rules: List[Stone] = {
      n match {
        case isZero if isZero == 0 => Stone(1) :: Nil
        case isEven if (isEven.toString.length)%2 == 0 => splitInMid
        case noneOfAbove => Stone(noneOfAbove * 2024) :: Nil
      }
  }  
 }

  def blink(stones: List[Stone]): List[Stone] = stones.flatMap(_.rules)

  def blinkMap(stones: Map[Stone, Long]): Map[Stone, Long] = {
    stones.foldLeft(Map.empty[Stone, Long]) { case (acc, (stone, stoneCount)) =>
      val nextStones = stone.rules
      nextStones.foldLeft(acc) { (acc, stone) => acc + (stone -> (acc.getOrElse(stone, 0L) + stoneCount)) }
    }
  }

  @tailrec
  def manyBlink(stones: List[Stone], times: Int): Long = {
    if (times <= 0) stones.length
      else manyBlink(blink(stones), times - 1)
  }
  
  def manyBlinkMap(stones: List[Stone], times: Int): Long = {
    val m = stones.groupBy(identity).view.mapValues(_.length.toLong).toMap

    @tailrec
    def go(stones: Map[Stone, Long], times: Int): Long = {
      if (times <= 0) stones.values.sum
      else go(blinkMap(stones), times -1)
    }
    go(m, times)
  }

 def solvePart1(data: String, times: Int): Long = manyBlink(parse(data), times)
 def solvePart2(data: String, times: Int): Long = manyBlinkMap(parse(data), times)
}

object Day11Part2 {
  def main(args: Array[String]): Unit = {
    println(Day11.solvePart2(Day11.real, 75))
  }
}
