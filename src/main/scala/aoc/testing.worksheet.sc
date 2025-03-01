import aoc.Day4
import aoc.Day3
import aoc.Day3._

val letters = Day4.data.split("\n").toList

val a = Day4.parse(data)
a.rows.map(_.sliding(3).toList)
