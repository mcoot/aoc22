package aoc22.day5

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

case class BlankSpace()

case class Item(id: Char)

case class CraneStack(stackIdx: Int, items: List[Item]):
  def topOfStack: Item = items.head

case class CraneState(stacks: Map[Int, CraneStack]):
  def applyMove(move: Move): CraneState =
    val toMove = stacks(move.originStack).items.take(move.quantity).reverse
    println(s"Moving ${toMove} from ${move.originStack} to ${move.destStack}")
    CraneState(stacks.map { case (idx, stack) =>
      if idx == move.originStack then
        (idx, CraneStack(idx, stack.items.drop(move.quantity)))
      else if idx == move.destStack then
        (idx, CraneStack(idx, toMove ++ stack.items))
      else
        (idx, stack)
    })

  def stacksAsList: List[CraneStack] = stacks.values.toList.sortBy(_.stackIdx)

  def getTopsOfStacks: List[Item] = stacksAsList.map(_.topOfStack)

case class Move(quantity: Int, originStack: Int, destStack: Int)

case class Program(startState: CraneState, moves: List[Move]):
  def run: CraneState =
    moves.foldLeft(startState) { case (s, m) =>
      println(s)
      s.applyMove(m)
    }


object Parsing:
  def blankParser: Parser[BlankSpace] = Parser.string("   ").map(_ => BlankSpace())

  def itemParser: Parser[Item] =
    ((Parser.char('[') *> Parser.charIn('A' to 'Z')) <* Parser.char(']')).map(Item.apply)

  def itemRowParser: Parser[List[BlankSpace|Item]] =
    CommonParsers.separated(itemParser | blankParser, Parser.char(' '))

  def stackRangeParser: Parser[Range] =
    CommonParsers.withTrimmedStartingSpaces(CommonParsers.spaceSeparated(CommonParsers.int).map { l => l.min to l.max })

  def craneSectionParser: Parser[CraneState] =
    CommonParsers.pair(CommonParsers.lineSeparated(itemRowParser), stackRangeParser, CommonParsers.newLine)
      .map { case (itemRows, range) =>
        val m = range.map { index =>
          // Zero-indexed idx
          val actualIndex = index - range.start
          val filteredItems: List[Item] = itemRows
            .filter(_.size > actualIndex)
            .flatMap(_(actualIndex) match {
              case BlankSpace() => List()
              case x => List(x.asInstanceOf[Item])
            })
          (index, CraneStack(index, filteredItems))
        }.toMap
        CraneState(m)
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


object Day5 extends SolutionWithParser[Program, String, String]:
  override def dayNumber: Int = 5

  override def parser: Parser[Program] = Parsing.programParser

  override def solvePart1(input: Program): String =
    input.run.getTopsOfStacks.map(_.id).mkString


  override def solvePart2(input: Program): String = ???


@main def run(): Unit = Day5.run()
@main def test(): Unit = Day5.test()
@main def testParser(): Unit =
  Day5.testParser(Parsing.programParser)
@main def runParser(): Unit =
  Day5.runParser(Parsing.programParser)
//  Day5.runParser((Parsing.craneSectionParser <* CommonParsers.blankLine) ~ CommonParsers.lineSeparated(Parsing.moveParser))
