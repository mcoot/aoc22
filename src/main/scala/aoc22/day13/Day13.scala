package aoc22.day13

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

enum Packet:
  case Literal(value: Int)
  case Sublist(values: List[Packet])


object Parsing:
  private def literalParser: Parser[Packet.Literal] =
    CommonParsers.int.map(Packet.Literal.apply)

  // Explicitly handle empty sublist
  // because cats.parse doesn't love possibly-empty parsers
  private def emptySublistParser: Parser[Packet.Sublist] =
    Parser.string("[]").map(_ => Packet.Sublist(List()))

  private def sublistParser: Parser[Packet.Sublist] =
    for
      _ <- Parser.char('[')
      values <- CommonParsers.commaSeparated(packetParser)
      _ <- Parser.char(']')
    yield
      Packet.Sublist(values)

  private def packetParser: Parser[Packet] =
    literalParser | emptySublistParser | sublistParser

  private def packetPairParser: Parser[(Packet, Packet)] =
    CommonParsers.pair(packetParser, packetParser, CommonParsers.newLine)

  def inputParser: Parser[List[(Packet, Packet)]] =
    CommonParsers.separated(packetPairParser, CommonParsers.blankLine)


object Day13 extends SolutionWithParser[List[(Packet, Packet)], Int, Int]:
  override def dayNumber: Int = 13

  override def parser: Parser[List[(Packet, Packet)]] = Parsing.inputParser

  override def solvePart1(input: List[(Packet, Packet)]): Int = ???

  override def solvePart2(input: List[(Packet, Packet)]): Int = ???


@main def run(): Unit = Day13.run()
@main def test(): Unit = Day13.test()
@main def testParser(): Unit =
  Day13.testParser(Day13.parser)
@main def runParser(): Unit =
  Day13.runParser(Day13.parser)