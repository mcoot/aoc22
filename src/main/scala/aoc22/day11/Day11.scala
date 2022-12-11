package aoc22.day11

import scala.collection.mutable.Map as MutableMap
import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

case class MonkeyId(id: Int)


enum WorryOperand:
  case Old
  case Literal(literalValue: Int)

  def value(oldValue: Int): Int = this match {
    case Old => oldValue
    case Literal(v) => v
  }


enum WorryOperator:
  case Add
  case Multiply

case class WorryOperation(left: WorryOperand, right: WorryOperand, op: WorryOperator):
  def apply(old: Int): Int = op match {
    case WorryOperator.Add => left.value(old) + right.value(old)
    case WorryOperator.Multiply => left.value(old) * right.value(old)
  }

case class WorryTest(factor: Int, ifTrueId: MonkeyId, ifFalseId: MonkeyId):
  def apply(worryLevel: Int): MonkeyId =
    if worryLevel % factor == 0 then
      ifTrueId
    else
      ifFalseId

case class Monkey(id: MonkeyId, items: List[Int], op: WorryOperation, test: WorryTest):
  // Apply the operator and then int-divide by 3
  private def inspect(item: Int): Int = op(item) / 3

  // Inspect all items and determine where they should be thrown
  def performInspections(): List[(Int, MonkeyId)] =
    items.map { worryLevel =>
      val postInspectionLevel = inspect(worryLevel)
      val toThrowTo = test(postInspectionLevel)
      (postInspectionLevel, toThrowTo)
    }

  // Empty the list when we throw all our inspected items
  def throwAllItems: Monkey =
    Monkey(id, List(), op, test)

  // Take items thrown to this monkey and append them to our list
  def acceptItems(incomingItems: Iterable[Int]): Monkey =
    Monkey(id, items ++ incomingItems.toList, op, test)


class MonkeyGroup(initialState: Map[MonkeyId, Monkey]):
  // State of the monkeys / holding items
  private val state = MutableMap.from(initialState)
  // To start with no monkey has performed inspections
  private val inspectionsPerformed = MutableMap.from(initialState.view.mapValues(_ => 0))

  // The actual number of monkeys doesn't change
  private def allMonkeyIds = initialState.keys.toList

  private def giveTurn(id: MonkeyId): Unit =
    // Let this monkey inspect its items
    val inspectedAndThrown = state(id).performInspections()
    // Update the number of inspections performed for this monkey
    inspectionsPerformed(id) += inspectedAndThrown.size
    // This monkey should empty its items
    state(id) = state(id).throwAllItems

    // Other monkeys should then receive the thrown items
    inspectedAndThrown.groupBy { case (_, id) => id }
      .foreach { case (receivingMonkey, items) =>
        state(receivingMonkey) = state(receivingMonkey).acceptItems(items.map(_(0)))
      }

  def runRound(): Unit =
    // Give all monkeys a turn
    allMonkeyIds.foreach(giveTurn)

  // Monkey business defined by the product of the number of inspections performed by the top-inspecting two monkeys
  def monkeyBusiness: Int =
    inspectionsPerformed.values.toList.sorted.takeRight(2).product

  def printState: String = state.toList.sortBy(_(0).id).map { case (id, monkey) =>
    s"Monkey ${id.id}: ${monkey.items.mkString(", ")}"
  }.mkString("\n")

  def printInspectionsState: String = inspectionsPerformed.toList.sortBy(_(0).id).map { case (id, inspections) =>
    s"Monkey ${id.id} inspected items ${inspections} times."
  }.mkString("\n")


object Parsing:
  def monkeyIdParser: Parser[MonkeyId] = CommonParsers.int.map(MonkeyId.apply)

  def monkeyHeaderParser: Parser[MonkeyId] = for
    _ <- Parser.string("Monkey ")
    id <- monkeyIdParser
    _ <- Parser.string(":")
  yield
    id

  def startingItemsParser: Parser[List[Int]] = for
    _ <- Parser.string("Starting items: ")
    items <- CommonParsers.separated(CommonParsers.int, Parser.string(", "))
  yield
    items

  def operandParser: Parser[WorryOperand] =
    Parser.string("old").map(_ => WorryOperand.Old) | CommonParsers.int.map(WorryOperand.Literal(_))

  def operatorParser: Parser[WorryOperator] =
    Parser.char('+').map(_ => WorryOperator.Add) | Parser.char('*').map(_ => WorryOperator.Multiply)

  def operationParser: Parser[WorryOperation] = for
    _ <- Parser.string("Operation: new = ")
    left <- operandParser <* CommonParsers.spaces
    op <- operatorParser <* CommonParsers.spaces
    right <- operandParser
  yield
    WorryOperation(left, right, op)

  def testParser: Parser[WorryTest] = for
    _ <- Parser.string("Test: divisible by ")
    factor <- CommonParsers.int <* CommonParsers.newLine
    _ <- CommonParsers.indented(Parser.string("If true: throw to monkey "), 4)
    ifTrue <- monkeyIdParser <* CommonParsers.newLine
    _ <- CommonParsers.indented(Parser.string("If false: throw to monkey "), 4)
    ifFalse <- monkeyIdParser
  yield
    WorryTest(factor, ifTrue, ifFalse)

  def monkeyParser: Parser[Monkey] = for
    monkeyId <- monkeyHeaderParser <* CommonParsers.newLine
    items <- CommonParsers.indented(startingItemsParser, 2) <* CommonParsers.newLine
    operation <- CommonParsers.indented(operationParser, 2) <* CommonParsers.newLine
    test <- CommonParsers.indented(testParser, 2)
  yield
    Monkey(monkeyId, items, operation, test)


  def inputParser: Parser[Map[MonkeyId, Monkey]] =
    CommonParsers.separated(monkeyParser, CommonParsers.blankLine).map { monkeys =>
      monkeys.map(monkey => (monkey.id, monkey)).toMap
    }


object Day11 extends SolutionWithParser[Map[MonkeyId, Monkey], Int, Int]:
  override def dayNumber: Int = 11

  override def parser: Parser[Map[MonkeyId, Monkey]] = Parsing.inputParser

  override def solvePart1(input: Map[MonkeyId, Monkey]): Int =
    val group = MonkeyGroup(input)
    (1 to 20).foreach(_ => group.runRound())
    println(group.printState)
    println(group.printInspectionsState)
    group.monkeyBusiness

  override def solvePart2(input: Map[MonkeyId, Monkey]): Int = ???


@main def run(): Unit = Day11.run()
@main def test(): Unit = Day11.test()
@main def testParser(): Unit =
  Day11.testParser(Day11.parser)
@main def runParser(): Unit =
  Day11.runParser(Day11.parser)