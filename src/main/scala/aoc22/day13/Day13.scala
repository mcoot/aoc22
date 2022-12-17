package aoc22.day13

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


enum Packet extends Ordered[Packet]:
  case Literal(value: Int)
  case Sublist(values: List[Packet])

  override def compare(that: Packet): Int =
    (this, that) match
      // Both literals, compare integers
      case (Packet.Literal(l), Packet.Literal(r)) => l compare r
      // Left is literal, convert it to a singleton list and compare
      case (Packet.Literal(l), r) => Packet.Sublist(List(Packet.Literal(l))) compare r
      // Right is literal, convert it to a singleton list and compare
      case (l, Packet.Literal(r)) => l compare Packet.Sublist(List(Packet.Literal(r)))
      // Both are lists
      case (Packet.Sublist(lSublist), Packet.Sublist(rSublist)) =>
        // Find the first subentry pair up to the min of both lists' lengths that aren't equal
        lSublist.zip(rSublist).collectFirst {
          case (l, r) if (l compare r) != 0 => l compare r
        }.getOrElse {
          // All were equal up to the min of both lengths
          // Compare the length
          // (If still equal, assume this will be resolved at a higher level)
          lSublist.length compare rSublist.length
        }


object Packet:
  def dividerPackets: List[Packet] =
    List(Packet.Sublist(List(Packet.Literal(2))), Packet.Sublist(List(Packet.Literal(6))))


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

  override def solvePart1(input: List[(Packet, Packet)]): Int =
    input
      .zipWithIndex
      .filter { case ((l, r), _) => l < r }
      // 1-indexed indices
      .map(_(1) + 1)
      .sum

  override def solvePart2(input: List[(Packet, Packet)]): Int =
    input
      // Ignore pairings and treat them all as a list
      // (explicit cast required because productIterator gives Iterator[Any]
      .flatMap(_.productIterator.toList.asInstanceOf[List[Packet]])
      // Include the divider packets
      .prependedAll(Packet.dividerPackets)
      // Sort them (lol no change needed from part 1 :^)
      .sorted
      // Find the divider packet indices
      .zipWithIndex
      .collect { case (pkt, idx) if Packet.dividerPackets.contains(pkt) => idx }
      // 1-indexed indices again!
      .map(_ + 1)
      .product


@main def run(): Unit = Day13.run()
@main def test(): Unit = Day13.test()
@main def testParser(): Unit =
  Day13.testParser(Day13.parser)
@main def runParser(): Unit =
  Day13.runParser(Day13.parser)