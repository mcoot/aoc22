package aoc22.day10

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


enum Instruction:
  case Noop
  case AddX(value: Int)


object Parsing:
  private def noopParser = Parser.string("noop").map(_ => Instruction.Noop)

  private def addxParser = for
    _ <- Parser.string("addx ")
    value <- CommonParsers.int
  yield
    Instruction.AddX(value)

  private def instructionParser: Parser[Instruction] = noopParser | addxParser

  def inputParser = CommonParsers.lineSeparated(instructionParser)


object Day10 extends SolutionWithParser[List[Instruction], Int, Int]:
  override def dayNumber: Int = 10

  override def parser: Parser[List[Instruction]] = Parsing.inputParser

  override def solvePart1(input: List[Instruction]): Int = ???

  override def solvePart2(input: List[Instruction]): Int = ???


@main def run(): Unit = Day10.run()
@main def test(): Unit = Day10.test()
@main def testLarge(): Unit = Day10.test("large")
@main def testParser(): Unit =
  Day10.testParser(Day10.parser)
@main def runParser(): Unit =
  Day10.runParser(Day10.parser)