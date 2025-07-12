import aoc.Day8._

parse(fake)
solvePart1(real)






val data =  parse(example)
val annetant = data.antennas.map(_._2).flatten

val ant = findAntinodes(data).map(_._2)
 for (i <- 0 until data.rowSize) {
  for (j <- 0 until data.columnSize) {
    val isAnntent = annetant.contains((i, j))
    val isAntinodes = ant.contains((i, j))
    if (isAnntent) {print('a')}
    else if (isAntinodes) {
      print('#')
      // suffix.append(s"($i, $j): dist_1: ${distance((i, j), (3, 4))}, dist_2: ${distance((i, j), (5, 5))}\n")
    }
    else {print('.')}
  }
  println()
}

println(findNonOverlappingAntinodes(data).mkString("\n"))
