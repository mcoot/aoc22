package aoc22.day11

import scala.collection.mutable.Map as MutableMap
import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

import scala.annotation.targetName

//
//def primeFactors(n: Int): Set[Int] =
//  def rec(n: Int, factor: Int): Set[Int] =
//    // Base case, only need to check up to sqrt(n)
//    if factor * factor > n then
//      // n and 1 always factors of themselves
//      Set(n, 1)
//    else if n % factor == 0 then
//      // Factor found
//      rec(n / factor, factor) + factor
//    else
//      // Continue checking factors
//      rec(n, factor + 1)
//
//  // Start at 2 and work up
//  rec(n, 2)
//
//
//case class WorryLevel(primeFactors: Set[Int]):
//  def this(initialValue: Int) =
//    this(primeFactors(initialValue))
//
//  def hasFactor(prime: Int): Boolean = primeFactors.contains(prime)
//
//  // No way to do addition without prime factoring the new value...
//  @targetName("add")
//  def +(other: Int): WorryLevel = ???
//
//  @targetName("multiply")
//  def *(other: Int): WorryLevel = ???
//
//  def square: WorryLevel = ???
//
//  def moduloFactor(factor: Int): WorryLevel =
//    if !hasFactor(factor) then
//      throw Exception("Cannot modulo by non-factor")
//    WorryLevel(primeFactors - factor)


case class MonkeyId(id: Int)


enum WorryOperand:
  case Old
  case Literal(literalValue: Long)

  def value(oldValue: Long): Long = this match {
    case Old => oldValue
    case Literal(v) => v
  }


enum WorryOperator:
  case Add
  case Multiply


case class WorryOperation(left: WorryOperand, right: WorryOperand, op: WorryOperator):
  def apply(old: Long): Long = op match {
    case WorryOperator.Add => left.value(old) + right.value(old)
    case WorryOperator.Multiply => left.value(old) * right.value(old)
  }


case class WorryTest(factor: Int, ifTrueId: MonkeyId, ifFalseId: MonkeyId):
  def moduloFactor(worryLevel: Long): Long = worryLevel % factor

  def apply(worryLevel: Long): MonkeyId =
    if moduloFactor(worryLevel) == 0 then
      ifTrueId
    else
      ifFalseId


case class Monkey(id: MonkeyId, items: List[Long], op: WorryOperation, test: WorryTest):
  // Apply the operator
  private def inspect(item: Long): Long = op(item)

  // Int-divide by 3 after completing inspection
  private def decreaseWorry(item: Long): Long = item / 3

  // Inspect all items and determine where they should be thrown
  def performInspections(decreaseWorryAfterInspection: Boolean): List[(Long, MonkeyId)] =
    items.map { worryLevel =>
      val inspected = inspect(worryLevel)
      val finalised = if decreaseWorryAfterInspection then
        decreaseWorry(inspected)
      else
        inspected
      val toThrowTo = test(finalised)
      (finalised, toThrowTo)
    }

  // Empty the list when we throw all our inspected items
  def throwAllItems: Monkey =
    Monkey(id, List(), op, test)

  // Take items thrown to this monkey and append them to our list
  def acceptItems(incomingItems: Iterable[Long]): Monkey =
    Monkey(id, items ++ incomingItems.toList, op, test)


class MonkeyGroup(initialState: Map[MonkeyId, Monkey], decreaseWorryAfterInspections: Boolean):
  // State of the monkeys / holding items
  private val state = MutableMap.from(initialState)
  // To start with no monkey has performed inspections
  private val inspectionsPerformed = MutableMap.from(initialState.view.mapValues(_ => 0L))

  // In part 1 we don't need this
  // In part 2 we modulo by the product of all the divisor tests
  // to keep from overflowing
  private val overallModuloFactor =
    if decreaseWorryAfterInspections then
      None
    else
      Some(initialState.values.map(_.test.factor).product)

  // The actual number of monkeys doesn't change
  private def sortedMonkeyIds = initialState.keys.toList.sortBy(_.id)

  def giveTurn(id: MonkeyId): Unit =
    // Let this monkey inspect its items
    val inspectedAndThrown = state(id).performInspections(decreaseWorryAfterInspections)
    // Update the number of inspections performed for this monkey
    inspectionsPerformed(id) += inspectedAndThrown.size
    // This monkey should empty its items
    state(id) = state(id).throwAllItems

    // Other monkeys should then receive the thrown items
    inspectedAndThrown.groupBy { case (_, id) => id }
      .foreach { case (receivingMonkey, items) =>
        state(receivingMonkey) = state(receivingMonkey).acceptItems(items.map { case (item, _) =>
          overallModuloFactor.map(item % _).getOrElse(item)
        })
      }

  def runRound(): Unit =
    // Give all monkeys a turn
    sortedMonkeyIds.foreach(id => giveTurn(id))

  // Monkey business defined by the product of the number of inspections performed by the top-inspecting two monkeys
  def monkeyBusiness: Long =
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

  def startingItemsParser: Parser[List[Long]] = for
    _ <- Parser.string("Starting items: ")
    items <- CommonParsers.separated(CommonParsers.int, Parser.string(", "))
  yield
    items.map(_.toLong)

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


object Day11 extends SolutionWithParser[Map[MonkeyId, Monkey], Long, Long]:
  override def dayNumber: Int = 11

  override def parser: Parser[Map[MonkeyId, Monkey]] = Parsing.inputParser

  override def solvePart1(input: Map[MonkeyId, Monkey]): Long =
    val group = MonkeyGroup(input, true)
    (1 to 20).foreach { r =>
      group.runRound()
    }
    println()
    println(group.printState)
    println()
    println(group.printInspectionsState)
    println()
    group.monkeyBusiness

  override def solvePart2(input: Map[MonkeyId, Monkey]): Long =
    val group = MonkeyGroup(input, false)
    (1 to 10000).foreach { r =>
      group.runRound()
    }
    println()
    println(group.printState)
    println()
    println(group.printInspectionsState)
    println()
    group.monkeyBusiness


@main def run(): Unit = Day11.run()
@main def test(): Unit = Day11.test()
@main def testParser(): Unit =
  Day11.testParser(Day11.parser)
@main def runParser(): Unit =
  Day11.runParser(Day11.parser)