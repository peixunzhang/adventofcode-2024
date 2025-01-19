package aoc

import better.files.Resource
import math.abs
import aoc.Day2.Safety.Safe
import aoc.Day2.Safety.Unsafe
import aoc.Day2.Safety.SingleBad
import aoc.Day2.Safety.SingleBaded

object Day2 {
  val data = Resource
    .getAsString("day2/real.txt")
    .split("\n")
    .toList
    .map(str => str.split(" ").map(_.toInt).toList)
  type Level = Int
  type Report = List[Level]

  // part 1
  def increaseCheck(report: Report): Boolean = report match {
    case x :: y :: rest =>
      x < y && differCheck(x, y) && increaseCheck(y :: rest)
    case _ => true
  }

  def decreaseCheck(report: Report): Boolean = report match {
    case x :: y :: rest =>
      x > y && differCheck(x, y) && decreaseCheck(y :: rest)
    case _ => true
  }

  def differCheck(current: Level, next: Level): Boolean =
    abs(current - next) >= 1 && (abs(current - next) <= 3)

  def countSafe(reports: List[Report]): Int =
    reports.count(x => increaseCheck(x) || decreaseCheck(x))

    // go through list once
  def safeReport(report: Report): Boolean = {
    val (de, in , _) = 
    report.foldLeft((true, true, Option.empty[Int])) {
      case ((de, in, None), next) => (de, in, Some(next))
      case ((de, in, Some(current)), next) =>
        (
          de && (current > next && differCheck(current, next)),
          in && (current < next && differCheck(current, next)),
          Some(next)
        )
    }
    de || in
  }
  def countSafeUpgrade(reports: List[Report]): Int = reports.count(safeReport)

  // part 2

  sealed trait Safety {
    def toBoolean: Boolean = this match {
      case Safe => true
      case Unsafe => false
      case SingleBaded => true
      case SingleBad(_) => true
    }
  }
  object Safety {
    case object Safe extends Safety
    case object Unsafe extends Safety
    case object SingleBaded extends Safety
    final case class SingleBad(previous: Int) extends Safety
  }

  def allowOneBadLevel(report: Report): Boolean = {
    val (de, in , _) = 
    report.foldLeft[(Safety, Safety, Option[Int])]((Safe, Safe, None)) {
      case ((de, in, None), next) => (de, in, Some(next))
      case ((de, in, Some(current)), next) =>
        (
          decreaseOneBad(current, next, de),
          increaseOneBad(current, next, in),
          Some(next)
        )
    }
    de.toBoolean || in.toBoolean
  }

  def decreaseOneBad(current: Int, next: Int, safety: Safety): Safety = safety match {
    case Safe => if ((current > next) && differCheck(current, next)) Safe else SingleBad(current)
    case Unsafe => Unsafe
    case SingleBaded => if ((current > next) && differCheck(current, next)) SingleBaded else Unsafe
    case SingleBad(previous) => if ((previous > next) && differCheck(previous, next)) SingleBaded else Unsafe
  }

  def increaseOneBad(current: Int, next: Int, safety: Safety): Safety = safety match {
    case Safe => if ((current < next) && differCheck(current, next)) Safe else SingleBad(current)
    case Unsafe => Unsafe
    case SingleBaded => if ((current < next) && differCheck(current, next)) SingleBaded else Unsafe
    case SingleBad(previous) => if ((previous < next) && differCheck(previous, next)) SingleBaded else Unsafe
  }

  def dampener(reports: List[Report]): Int = reports.count(allowOneBadLevel)

  object part2 {
    def check(report: Report, check: (Int, Int) => Boolean): Boolean = {
      def go(report: Report, singleBad: Boolean): Boolean = {
        report match {
          case x1 :: x2 :: xs =>
            (!singleBad && go(x1 :: xs, true)) || (check(x1, x2) && go(x2 :: xs, singleBad))
          case _ => true
        }
      }
      go(report, false) || go(report.tail, true)
    }
    def checkDecreasing(report: Report): Boolean = check(report, (x1, x2) => x1 > x2 && differCheck(x1, x2))
    def checkIncreasing(report: Report): Boolean = check(report, (x1, x2) => x1 < x2 && differCheck(x1, x2))

    def checkReports(reports: List[Report]): Int =
      reports.count(r => checkDecreasing(r) || checkIncreasing(r))
  }

  def solution: Unit = {
    println("Part1:" + countSafeUpgrade(data))
    println("Part2:" + part2.checkReports(data))
    println(data.map(allowOneBadLevel))
  }
}
