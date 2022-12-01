package aoc22.day1

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

import scala.io.Source

object Day1 extends SolutionWithParser[List[List[Int]], Int, Int] {
  override def dayNumber: Int = 1

  override def parser: Parser[List[List[Int]]] =
    CommonParsers.separated(CommonParsers.lineSeparated(CommonParsers.int), CommonParsers.blankLine)


  override def solvePart1(input: List[List[Int]]): Int =
    input.map(_.sum).max

  override def solvePart2(input: List[List[Int]]): Int =
    input.map(_.sum).sorted.takeRight(3).sum
}

@main def run(): Unit = Day1.run()

@main def test(): Unit = Day1.test()