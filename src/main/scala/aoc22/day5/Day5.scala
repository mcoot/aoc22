package aoc22.day5

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

case class BlankSpace()

case class Item(id: Char)

case class CraneStack(stackIdx: Int, items: List[Item])

case class Move(quantity: Int, originStack: Int, destStack: Int)

case class Program(startState: Map[Int, CraneStack], moves: List[Move])


object Parsing:
  def blankParser: Parser[BlankSpace] = Parser.string("   ").map(_ => BlankSpace())

  def itemParser: Parser[Item] =
    ((Parser.char('[') *> Parser.charIn('A' to 'Z')) <* Parser.char(']')).map(Item.apply)

  def itemRowParser: Parser[List[BlankSpace|Item]] =
    CommonParsers.spaceSeparated(itemParser | blankParser)

  def stackRangeParser: Parser[Range] =
    CommonParsers.withTrimmedStartingSpaces(CommonParsers.spaceSeparated(CommonParsers.int).map { l => l.min to l.max })

  def craneSectionParser: Parser[Map[Int, CraneStack]] =
    CommonParsers.pair(CommonParsers.lineSeparated(itemRowParser), stackRangeParser, CommonParsers.newLine)
      .map { case (itemRows, range) =>
        range.map { index =>
          // Zero-indexed idx
          val actualIndex = index - range.start
          val filteredItems: List[Item] = itemRows
            .filter(_.size > actualIndex)
            .flatMap(_(actualIndex) match {
              case BlankSpace() => List()
              case x => List(x.asInstanceOf[Item])
            })
          (index, CraneStack(index, filteredItems.reverse))
        }.toMap
      }

  def moveParser = for
    _ <- Parser.string("move ")
    quantity <- CommonParsers.int
    _ <- Parser.string(" from ")
    origin <- CommonParsers.int
    _ <- Parser.string(" to ")
    dest <- CommonParsers.int
  yield Move(quantity, origin, dest)

  def programParser: Parser[Program] = for
    startingState <- craneSectionParser
    _ <- CommonParsers.blankLine
    moves <- CommonParsers.lineSeparated(moveParser)
  yield Program(startingState, moves)


object Day5 extends SolutionWithParser[Program, Int, Int]:
  override def dayNumber: Int = 5

  override def parser: Parser[Program] = Parsing.programParser

  override def solvePart1(input: Program): Int = ???

  override def solvePart2(input: Program): Int = ???


@main def run(): Unit = Day5.run()
@main def test(): Unit = Day5.test()
@main def testParser(): Unit =
  Day5.testParser(Parsing.programParser)
