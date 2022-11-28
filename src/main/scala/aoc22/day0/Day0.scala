package aoc22.day0

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

/**
 * A test day to check that the skeleton works
 *
 * The parts are counting the number of elements in the list, and summing them
 */
object Day0 extends SolutionWithParser[List[Int], Int, Int]:
  override def dayNumber: Int = 0

  override def parser: Parser[List[Int]] = CommonParsers.commaSeparated(CommonParsers.int)

  override def solvePart1(input: List[Int]): Int = input.length

  override def solvePart2(input: List[Int]): Int = input.sum


@main def run(): Unit = Day0.run()


@main def test(): Unit = Day0.test()


@main def testSingle(): Unit = Day0.test("single")