import aoc.Day12._
val garden = parse(example)
val current: Coordinate = (0, 0)
val edge = getEdge(current)
val area = garden.getAllArea().head
val square = getAllEdges(area) 
