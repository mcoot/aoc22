package aoc22.day18

import scala.collection.mutable.Set as MutableSet
import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


case class Cube(x: Int, y: Int, z: Int):
  def this(t: (Int, Int, Int)) = this(t(0), t(1), t(2))

  def maximalAdjacency: CubeInDroplet = CubeInDroplet(this, List(
    Some(Cube(x - 1, y, z)),
    Some(Cube(x + 1, y, z)),
    Some(Cube(x, y - 1, z)),
    Some(Cube(x, y + 1, z)),
    Some(Cube(x, y, z - 1)),
    Some(Cube(x, y, z + 1)),
  ))

  def adjacentPositions: List[Cube] =
    maximalAdjacency.neighbours.map(_.get)

  def isAdjacentTo(other: Cube): Boolean = adjacentPositions.contains(other)


// List always of size 6
// easier than using a tuple
case class CubeInDroplet(cube: Cube, neighbours: List[Option[Cube]])


case class Droplet(cubes: List[Cube]):
  val cubeSet: Set[Cube] = cubes.toSet

  def cubeGraph: Map[Cube, CubeInDroplet] =
    cubes.map { cube =>
      (cube, CubeInDroplet(cube, cube.maximalAdjacency.neighbours.map { n =>
        Some(n.get).filter(cubeSet.contains)
      }))
    }.toMap

  def totalSurfaceArea: Int =
    cubeGraph.values.map(c => c.neighbours.count(_.isEmpty)).sum

  def externalSurfaceArea: Int =
    cubeGraph.values.map(c => c.neighbours.count(_.isEmpty)).sum


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

