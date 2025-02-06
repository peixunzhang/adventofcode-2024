import aoc.Day3
import aoc.Day3._


toInstruction(instruction.findAllMatchIn(data).toList)

val a: List[Instruction] = Nil
val b = Enable
val c = Disable

val ab = a.::(b)
ab :+ c
