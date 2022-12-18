package aoc22.day18

import scala.collection.mutable.{Set as MutableSet, Map as MutableMap}
import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


case class Cube(x: Int, y: Int, z: Int):
  def this(t: (Int, Int, Int)) = this(t(0), t(1), t(2))

  def adjacentPositions: List[Cube] = List(
    Cube(x - 1, y, z),
    Cube(x + 1, y, z),
    Cube(x, y - 1, z),
    Cube(x, y + 1, z),
    Cube(x, y, z - 1),
    Cube(x, y, z + 1)
  )


case class Droplet(cubes: List[Cube]):
  val cubeSet: Set[Cube] = cubes.toSet

  def cubeExists(cube: Cube): Boolean = cubeSet.contains(cube)

  def minBound: Cube =
    Cube(cubeSet.map(_.x).min, cubeSet.map(_.y).min, cubeSet.map(_.z).min)

  def maxBound: Cube =
    Cube(cubeSet.map(_.x).max, cubeSet.map(_.y).max, cubeSet.map(_.z).max)

  def isOnBoundary(cube: Cube): Boolean =
    cube.adjacentPositions.exists { n =>
      n.x < minBound.x || n.y < minBound.y || n.z < minBound.z
        || n.x > maxBound.x || n.y > maxBound.y || n.z > maxBound.z
    }

  def surfaceAreaOf(cs: Set[Cube], existing: Boolean): Int =
    cs
      // toList because other map() will secretly dedupe
      .toList
      .map { c => c.adjacentPositions.count(n => (existing && !cubeExists(n)) || (!existing && cubeExists(n))) }
      .sum

  def totalSurfaceArea: Int = surfaceAreaOf(cubeSet, true)

  // Depth-first search of a potential hole
  // Returns the cube positions making up the whole if it is truly internal
  // (i.e. if a boundary is not encountered)
  def dfsHole(pos: Cube, visited: Set[Cube], cache: MutableMap[Cube, Option[Set[Cube]]]): Option[Set[Cube]] =
    if cache.contains(pos) then
      return cache(pos)

    // Base case, our current position hits a boundary, then we aren't in a hole
    if isOnBoundary(pos) then
      cache(pos) = None
      return None

    // Find new empty spaces to go
    val toVisit = pos.adjacentPositions.filter(n => !visited.contains(n) && !cubeExists(n))

    var newVisited = visited + pos

    for n <- toVisit do
      dfsHole(n, newVisited, cache) match
        case Some(value) =>
          newVisited ++= value
        case None =>
          // Immediately propagate a boundary up, no point continuing
          cache(pos) = None
          return None
    cache(pos) = Some(newVisited)
    Some(newVisited)

  def holes: Set[Set[Cube]] =
    // Start searching from empty adjacent positions of filled ones
    // (Dedupes due to being a set)
    val emptyAdjacencies = cubeSet.flatMap(c => c.adjacentPositions.filter(!cubeExists(_))).toList

    var holes: Set[Set[Cube]] = Set.empty

    for (c, idx) <- emptyAdjacencies.zipWithIndex do

      if !holes.exists(_.contains(c)) then
        val res = dfsHole(c, Set.empty, MutableMap.empty)
        if res.isDefined then
          holes = holes + res.get

    holes


  def externalSurfaceArea: Int =
    totalSurfaceArea - holes.toList.map(h => surfaceAreaOf(h, false)).sum


object Day18 extends SolutionWithParser[List[Cube], Int, Int]:
  override def dayNumber: Int = 18

  override def parser: Parser[List[Cube]] =
    CommonParsers.lineSeparated(
      CommonParsers.triple(CommonParsers.int, CommonParsers.int, CommonParsers.int, Parser.char(','))
        .map(Cube.apply)
    )

  override def solvePart1(input: List[Cube]): Int =
    Droplet(input).totalSurfaceArea

  override def solvePart2(input: List[Cube]): Int =
    Droplet(input).externalSurfaceArea


@main def run(): Unit = Day18.run()
@main def test(): Unit = Day18.test()
@main def testParser(): Unit =
  Day18.testParser(Day18.parser)
@main def runParser(): Unit =
  Day18.runParser(Day18.parser)

