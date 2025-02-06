package aoc

import better.files.Resource
import scala.util.matching.Regex
import scala.collection.immutable

object Day3 {
  val goal: Regex = """(?:mul)\((\d+)\,(\d+)\)""".r

  val data = Resource
    .getAsString("day3/real.txt")

  // regex, find all, regex capture group
  //part 1
  def getMul(str: String): Int = goal.findAllMatchIn(str).toList.foldLeft(0){ (acc, next) => (next.group(1).toInt * next.group(2).toInt) + acc}

  // part 2
  val instruction: Regex = """(?:mul\((\d+)\,(\d+)\))|do\(\)|don\'t\(\)""".r

  sealed trait Instruction
  final case class Mul(first: Int, second: Int) extends Instruction
  case object Enable extends Instruction
  case object Disable extends Instruction


  def toInstruction(matches: List[Regex.Match], acc: List[Instruction] = Nil): List[Instruction] = matches match {
    case Regex.Match("do()") :: xs => toInstruction(xs, Enable :: acc)
    case Regex.Match("don't()") :: xs => toInstruction(xs, Disable :: acc)
    case mul :: xs => toInstruction(xs, Mul(mul.group(1).toInt, mul.group(2).toInt) :: acc)
    case Nil => acc.reverse
  }

  def applyInstruction(i: List[Instruction], switch: Boolean = true, acc: Int = 0): Int = i match {
    case Disable :: xs => applyInstruction(xs, false, acc)
    case Enable :: xs => applyInstruction(xs, true, acc)
    case Mul(first, second) :: xs => 
      if (switch) 
        applyInstruction(xs, true, (first * second) + acc) 
      else 
        applyInstruction(xs, switch, acc)
    case Nil => acc
  }

  def main(arg: Array[String]): Unit = {
    val result = applyInstruction(toInstruction(instruction.findAllMatchIn(data).toList))
    println(result)
  }

}
