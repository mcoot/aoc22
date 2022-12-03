package aoc22.day3

import aoc22.common.SolutionWithParser
import cats.parse.Parser


// Domain models




// Parsing

object Parsing:
  def finalParser: Parser[String] = ???


// Solution

object Day3 extends SolutionWithParser[String, Int, Int]:
  override def dayNumber: Int = 3

  override def parser: Parser[String] = Parsing.finalParser

  override def solvePart1(input: String): Int = ???

  override def solvePart2(input: String): Int = ???


@main def run(): Unit = Day3.run()
@main def test(): Unit = Day3.test()
@main def testParser(): Unit = Day3.testParser(Day3.parser)



