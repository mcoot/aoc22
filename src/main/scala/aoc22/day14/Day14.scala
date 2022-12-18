package aoc22.day14

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


case class Position(x: Int, y: Int):
  def this(p: (Int, Int)) = this(p(0), p(1))
  def pair: (Int, Int) = (x, y)


case class RockPath(pathDefinition: List[Position])


object Day14 extends SolutionWithParser[List[RockPath], Int, Int]:
  override def dayNumber: Int = 14

  private def positionParser: Parser[Position] =
    CommonParsers.pair(CommonParsers.int, CommonParsers.int, Parser.char(','))
      .map(Position.apply)

  private def rockPathParser: Parser[RockPath] =
    CommonParsers.separated(positionParser, Parser.string(" -> "))
      .map(RockPath.apply)

  override def parser: Parser[List[RockPath]] =
    CommonParsers.lineSeparated(rockPathParser)

  override def solvePart1(input: List[RockPath]): Int = ???

  override def solvePart2(input: List[RockPath]): Int = ???


@main def run(): Unit = Day14.run()
@main def test(): Unit = Day14.test()
@main def testParser(): Unit =
  Day14.testParser(Day14.parser)
@main def runParser(): Unit =
  Day14.runParser(Day14.parser)
