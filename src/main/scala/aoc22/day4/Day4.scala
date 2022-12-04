package aoc22.day4

import aoc22.common.SolutionWithParser
import cats.parse.Parser


// Domain models




// Parsing

object Parsing:
  def day4Parser = ???


// Solution

object Day4 extends SolutionWithParser[String, Int, Int]:
  override def dayNumber: Int = 4

  override def parser = Parsing.day4Parser

  override def solvePart1(input: String) = ???

  override def solvePart2(input: String) = ???


@main def run(): Unit = Day4.run()
@main def test(): Unit = Day4.test()
@main def testParser(): Unit = Day4.testParser(Day4.parser)
