package aoc22.day18

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

case class Cube(x: Int, y: Int, z: Int):
  def this(t: (Int, Int, Int)) = this(t(0), t(1), t(2))

object Day18 extends SolutionWithParser[List[Cube], Int, Int]:
  override def dayNumber: Int = 18


  override def parser: Parser[List[Cube]] =
    CommonParsers.lineSeparated(
      CommonParsers.triple(CommonParsers.int, CommonParsers.int, CommonParsers.int, Parser.char(','))
        .map(Cube.apply)
    )

  override def solvePart1(input: List[Cube]): Int = ???

  override def solvePart2(input: List[Cube]): Int = ???


@main def run(): Unit = Day18.run()
@main def test(): Unit = Day18.test()
@main def testParser(): Unit =
  Day18.testParser(Day18.parser)
@main def runParser(): Unit =
  Day18.runParser(Day18.parser)

