package aoc22.day9

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

enum Direction(val char: Char):
  case L extends Direction('L')
  case U extends Direction('U')
  case R extends Direction('R')
  case D extends Direction('D')


case class Instruction(dir: Direction, distance: Int)


object Parsing:
  def directionParser: Parser[Direction] =
    Parser.charIn(Direction.values.map(_.char))
      .map { c =>
        Direction
          .values
          .find(c == _.char)
          .getOrElse(throw Exception("Invalid direction"))
      }

  def instructionParser: Parser[Instruction] =
    CommonParsers.pair(directionParser, CommonParsers.int, Parser.char(' '))
      .map { case (dir, dist) => Instruction(dir, dist) }

  def inputParser: Parser[List[Instruction]] = CommonParsers.lineSeparated(instructionParser)


object Day9 extends SolutionWithParser[List[Instruction], Int, Int]:
  override def dayNumber: Int = 9

  override def parser: Parser[List[Instruction]] = Parsing.inputParser

  override def solvePart1(input: List[Instruction]): Int = ???

  override def solvePart2(input: List[Instruction]): Int = ???


@main def run(): Unit = Day9.run()
@main def test(): Unit = Day9.test()
@main def testParser(): Unit =
  Day9.testParser(Day9.parser)
@main def runParser(): Unit =
  Day9.runParser(Day9.parser)