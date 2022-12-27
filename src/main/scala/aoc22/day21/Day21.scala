package aoc22.day21

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


enum Value:
  case Literal(value: Int)
  case Reference(name: String)


enum Expression:
  case Literal(value: Value)
  case Add(left: Value, right: Value)
  case Mult(left: Value, right: Value)
  case Minus(left: Value, right: Value)
  case Div(left: Value, right: Value)


case class Monkey(name: String, op: Expression)


object Day21 extends SolutionWithParser[List[Monkey], Int, Int]:
  override def dayNumber: Int = 21

  object Parsing:
    def name: Parser[String] = Parser.charIn('a' to 'z').rep.map(l => l.toList.mkString)

    def literal: Parser[Value.Literal] = CommonParsers.int.map(Value.Literal(_))

    def reference: Parser[Value.Reference] = name.map(Value.Reference(_))

    def binaryOperation: Parser[(Value, Value, Char)] =
      for
        l <- reference
        op <- (CommonParsers.spaces *> Parser.charIn(List('+', '-', '*', '/'))) <* CommonParsers.spaces
        r <- reference
      yield
        (l, r, op)

    def expr: Parser[Expression] =
      binaryOperation.map { (l, r, op) => op match
        case '+' => Expression.Add(l, r)
        case '*' => Expression.Mult(l, r)
        case '-' => Expression.Minus(l, r)
        case '/' => Expression.Div(l, r)
      } | literal.map(Expression.Literal(_))

    def monkey: Parser[Monkey] =
      CommonParsers.pair(name, expr, Parser.string(": ")).map((n, e) => Monkey(n, e))

  override def parser: Parser[List[Monkey]] =
    CommonParsers.lineSeparated(Parsing.monkey)

  override def solvePart1(input: List[Monkey]): Int = ???

  override def solvePart2(input: List[Monkey]): Int = ???


@main def run(): Unit = Day21.run()
@main def test(): Unit = Day21.test()
@main def testParser(): Unit =
  Day21.testParser(Day21.parser)
@main def runParser(): Unit =
  Day21.runParser(Day21.parser)