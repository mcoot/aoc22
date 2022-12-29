package aoc22.day22

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

enum TurnDirection:
  case L
  case R

enum Instruction:
  case Forward(steps: Int)
  case Turn(dir: TurnDirection)

// Raw grid data before we process it into a map, including the empty/non-existent sections
// -1 = non-existent, 0 = open tile, 1 = wall tile
case class RawGridData(arr: Array[Array[Int]])

object Day22 extends SolutionWithParser[(RawGridData, List[Instruction]), Long, Long]:
  override def dayNumber: Int = 22

  private object Parsing:
    private def turnDir: Parser[TurnDirection] =
      Parser.char('L').map(_ => TurnDirection.L) | Parser.char('R').map(_ => TurnDirection.R)

    private def instruction: Parser[Instruction] =
      turnDir.map(Instruction.Turn(_)) | CommonParsers.int.map(Instruction.Forward(_))

    def instructionList: Parser[List[Instruction]] =
      instruction.rep(1).map(_.toList)

    def rawTileParser: Parser[Int] =
      Parser.char(' ').map(_ => -1) | Parser.char('.').map(_ => 0) | Parser.char('#').map(_ => 1)

    def rawGridLineParser: Parser[Array[Int]] =
      rawTileParser.rep(1).map(l => Array.from(l.toList))


    def rawGridData: Parser[RawGridData] =
      CommonParsers.lineSeparated(rawGridLineParser).map { lines =>
        val maxLength = lines.map(_.length).max
        RawGridData(Array.from(
          lines.map { l =>
            // Copy the line we have, appending -1s to the max length
            // so we get a consistent 2D grid
            val res = Array.fill(maxLength)(-1)
            System.arraycopy(l, 0, res, 0, l.length)
            res
          }
        ))
      }

  override def parser: Parser[(RawGridData, List[Instruction])] =
    CommonParsers.pair(Parsing.rawGridData, Parsing.instructionList, CommonParsers.blankLine)

  override def solvePart1(input: (RawGridData, List[Instruction])): Long = ???

  override def solvePart2(input: (RawGridData, List[Instruction])): Long = ???


@main def run(): Unit = Day22.run()
@main def test(): Unit = Day22.test()
@main def testParser(): Unit =
  Day22.testParser(Day22.parser)
@main def runParser(): Unit =
  Day22.runParser(Day22.parser)