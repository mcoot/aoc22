package aoc22.day12

import aoc22.common.SolutionWithParser
import cats.parse.Parser





object Parsing:
  def inputParser = ???


object Day12 extends SolutionWithParser[String, Int, Int]:
  override def dayNumber: Int = 12

  override def parser: Parser[String] = Parsing.inputParser

  override def solvePart1(input: String): Int = ???

  override def solvePart2(input: String): Int = ???


@main def run(): Unit = Day12.run()
@main def test(): Unit = Day12.test()
@main def testParser(): Unit =
  Day12.testParser(Day12.parser)
@main def runParser(): Unit =
  Day12.runParser(Day12.parser)