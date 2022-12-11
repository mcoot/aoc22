package aoc22.day11

import aoc22.common.SolutionWithParser
import cats.parse.Parser


object Parsing:
  def inputParser: Parser[String] = ???


object Day11 extends SolutionWithParser[String, Int, Int]:
  override def dayNumber: Int = 11

  override def parser: Parser[String] = Parsing.inputParser

  override def solvePart1(input: String): Int = ???

  override def solvePart2(input: String): Int = ???


@main def run(): Unit = Day11.run()
@main def test(): Unit = Day11.test()
@main def testParser(): Unit =
  Day11.testParser(Day11.parser)
@main def runParser(): Unit =
  Day11.runParser(Day11.parser)