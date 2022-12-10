package aoc22.day10

import aoc22.common.SolutionWithParser
import cats.parse.Parser





object Parsing:
  def inputParser = ???


object Day10 extends SolutionWithParser[String, Int, Int]:
  override def dayNumber: Int = 10

  override def parser: Parser[String] = Parsing.inputParser

  override def solvePart1(input: String): Int = ???

  override def solvePart2(input: String): Int = ???


@main def run(): Unit = Day10.run()
@main def test(): Unit = Day10.test()
@main def testParser(): Unit =
  Day10.testParser(Day10.parser)
@main def runParser(): Unit =
  Day10.runParser(Day10.parser)