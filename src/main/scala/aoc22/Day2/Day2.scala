package aoc22.Day2

import aoc22.common.SolutionWithParser
import cats.parse.Parser

object Parsing:
  val parser: Parser[String] = ???


object Day2 extends SolutionWithParser[String, Int, Int] {
  override def dayNumber: Int = 2

  override def parser: Parser[String] = Parsing.parser

  override def solvePart1(input: String) = ???

  override def solvePart2(input: String) = ???
}

@main def run(): Unit = Day2.run()

@main def test(): Unit = Day2.test()

@main def testParser(): Unit = Day2.testParser(Parsing.parser)