package aoc22.day11

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


enum WorryOperand:
  case Old
  case Literal(value: Int)


enum WorryOperator:
  case Add
  case Multiply

case class WorryOperation(left: WorryOperand, right: WorryOperand, op: WorryOperator)

case class WorryTest(factor: Int, ifTrueId: Int, ifFalseId: Int)

case class Monkey(id: Int, items: List[Int], op: WorryOperation, test: WorryTest)


object Parsing:
  def monkeyIdParser: Parser[Int] = for
    _ <- Parser.string("Monkey ")
    monkeyId <- CommonParsers.int
    _ <- Parser.string(":")
  yield
    monkeyId

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
    ifTrue <- CommonParsers.int <* CommonParsers.newLine
    _ <- CommonParsers.indented(Parser.string("If false: throw to monkey "), 4)
    ifFalse <- CommonParsers.int
  yield
    WorryTest(factor, ifTrue, ifFalse)

  def monkeyParser: Parser[Monkey] = for
    monkeyId <- monkeyIdParser <* CommonParsers.newLine
    items <- CommonParsers.indented(startingItemsParser, 2) <* CommonParsers.newLine
    operation <- CommonParsers.indented(operationParser, 2) <* CommonParsers.newLine
    test <- CommonParsers.indented(testParser, 2)
  yield
    Monkey(monkeyId, items, operation, test)


  def inputParser: Parser[Map[Int, Monkey]] =
    CommonParsers.separated(monkeyParser, CommonParsers.blankLine).map { monkeys =>
      monkeys.map(monkey => (monkey.id, monkey)).toMap
    }


object Day11 extends SolutionWithParser[Map[Int, Monkey], Int, Int]:
  override def dayNumber: Int = 11

  override def parser: Parser[Map[Int, Monkey]] = Parsing.inputParser

  override def solvePart1(input: Map[Int, Monkey]): Int = ???

  override def solvePart2(input: Map[Int, Monkey]): Int = ???


@main def run(): Unit = Day11.run()
@main def test(): Unit = Day11.test()
@main def testParser(): Unit =
  Day11.testParser(Day11.parser)
@main def runParser(): Unit =
  Day11.runParser(Day11.parser)