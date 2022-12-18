package aoc22.day18

import scala.collection.mutable.ArrayBuffer
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


def findSurfaceArea(cubes: List[Cube]): Int =
  val cubeCartesian =
    for
      a <- cubes
      b <- cubes
    yield (a, b)
  cubes.size * 6 - cubeCartesian.count { case (a, b) => a.isAdjacentTo(b) }



object Day18 extends SolutionWithParser[List[Cube], Int, Int]:
  override def dayNumber: Int = 18

  override def parser: Parser[List[Cube]] =
    CommonParsers.lineSeparated(
      CommonParsers.triple(CommonParsers.int, CommonParsers.int, CommonParsers.int, Parser.char(','))
        .map(Cube.apply)
    )

  override def solvePart1(input: List[Cube]): Int = findSurfaceArea(input)

  override def solvePart2(input: List[Cube]): Int = ???


@main def run(): Unit = Day18.run()
@main def test(): Unit = Day18.test()
@main def testParser(): Unit =
  Day18.testParser(Day18.parser)
@main def runParser(): Unit =
  Day18.runParser(Day18.parser)

