package aoc22.day6

import aoc22.common.SolutionWithParser
import cats.parse.Parser


private def findMarker(input: List[Char], messageSize: Int) =
  input
    .zipWithIndex
    .sliding(messageSize)
    .collectFirst {
      case cs if cs.map(_(0)).toSet.size == messageSize => cs.last(1) + 1
    }.get


object Day6 extends SolutionWithParser[List[Char], Int, Int]:
  override def dayNumber: Int = 6

  override def parser: Parser[List[Char]] =
    Parser.charIn('a' to 'z').rep(1).map(_.toList)

  override def solvePart1(input: List[Char]): Int = findMarker(input, 4)

  override def solvePart2(input: List[Char]): Int = findMarker(input, 14)


@main def run(): Unit = Day6.run()
@main def test(): Unit = Day6.test()