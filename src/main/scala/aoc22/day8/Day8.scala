package aoc22.day8

import aoc22.common.SolutionWithParser
import cats.parse.Parser


object Parsing:
  def finalParser = ???


object Day8 extends SolutionWithParser[String, Int, Int]:
  override def dayNumber: Int = 8

  override def parser = Parsing.finalParser

  override def solvePart1(input: String): Int = ???

  override def solvePart2(input: String): Int = ???


@main def run(): Unit = Day8.run()
@main def test(): Unit = Day8.test()
@main def testParser(): Unit =
  Day8.testParser(Day8.parser)
@main def runParser(): Unit =
  Day8.runParser(Day8.parser)