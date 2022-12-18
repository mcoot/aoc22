package aoc22.day18

import scala.collection.mutable.Set as MutableSet
import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


case class Cube(x: Int, y: Int, z: Int):
  def this(t: (Int, Int, Int)) = this(t(0), t(1), t(2))

  def adjacencies: List[Cube] = List(
    Cube(x - 1, y, z),
    Cube(x + 1, y, z),
    Cube(x, y - 1, z),
    Cube(x, y + 1, z),
    Cube(x, y, z - 1),
    Cube(x, y, z + 1),
  )

  def isAdjacentTo(other: Cube): Boolean = adjacencies.contains(other)


def findSurfaceAreaPart1(cubes: List[Cube]): Int =
  val cubeCartesian: MutableSet[(Cube, Cube)] = MutableSet.empty
  for
    a <- cubes
    b <- cubes
    if a != b && !cubeCartesian.contains((a, b)) && !cubeCartesian.contains((b, a))
  do
    cubeCartesian.addOne((a, b))
  cubes.size * 6 - 2 * cubeCartesian.count { case (a, b) => a.isAdjacentTo(b) }


case class CubeInDroplet(cube: Cube, neighbours: (Option[Cube], Option[Cube], Option[Cube], Option[Cube], Option[Cube], Option[Cube]))


case class Droplet(cubes: List[Cube]):
  def cubeGraph: Map[Cube, CubeInDroplet] =
    ???

  def externalSurfaceArea: Int =
    ???


object Day18 extends SolutionWithParser[List[Cube], Int, Int]:
  override def dayNumber: Int = 18

  override def parser: Parser[List[Cube]] =
    CommonParsers.lineSeparated(
      CommonParsers.triple(CommonParsers.int, CommonParsers.int, CommonParsers.int, Parser.char(','))
        .map(Cube.apply)
    )

  override def solvePart1(input: List[Cube]): Int =
    findSurfaceAreaPart1(input)

  override def solvePart2(input: List[Cube]): Int =
    Droplet(input).externalSurfaceArea


@main def run(): Unit = Day18.run()
@main def test(): Unit = Day18.test()
@main def testParser(): Unit =
  Day18.testParser(Day18.parser)
@main def runParser(): Unit =
  Day18.runParser(Day18.parser)

