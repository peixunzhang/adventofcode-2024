import aoc.Day12._
val garden = parse(example)
val area = garden.getAllArea()
val edges = getEdges(area.head)
// val edges = area.map(getEdges)
val mer = mergeEdges(edges)
val so = solvePart2(real)
