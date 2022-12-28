package aoc22.day21

import scala.collection.mutable
import scala.collection.mutable.Set as MutableSet
import scala.collection.mutable.Map as MutableMap
import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


enum Value:
  case Literal(value: Long)
  case Reference(name: String)

  def resolve(refMap: Map[String, Long]): Long =
    this match
      case Value.Literal(v) => v
      case Value.Reference(ref) => refMap.getOrElse(ref, throw Exception("Reference not in map"))


enum Expression:
  case Literal(value: Value)
  case Add(left: Value, right: Value)
  case Mult(left: Value, right: Value)
  case Minus(left: Value, right: Value)
  case Div(left: Value, right: Value)

  def apply(refMap: Map[String, Long]): Long =
    this match
      case Expression.Literal(v) => v.resolve(refMap)
      case Expression.Add(l, r) => l.resolve(refMap) + r.resolve(refMap)
      case Expression.Mult(l, r) => l.resolve(refMap) * r.resolve(refMap)
      case Expression.Minus(l, r) => l.resolve(refMap) - r.resolve(refMap)
      case Expression.Div(l, r) => l.resolve(refMap) / r.resolve(refMap)
      case _ => throw Exception("Invalid application")


case class Monkey(name: String, op: Expression)


case class MonkeyGraph(monkeyList: List[Monkey]):
  val vertices: Map[String, Monkey] = Map.from(monkeyList.map(m => (m.name, m)))

  val edges: Set[(String, String)] = monkeyList.flatMap { m =>
    m.op match
      case Expression.Add(Value.Reference(l), Value.Reference(r)) => List((m.name, l), (m.name, r))
      case Expression.Mult(Value.Reference(l), Value.Reference(r)) => List((m.name, l), (m.name, r))
      case Expression.Minus(Value.Reference(l), Value.Reference(r)) => List((m.name, l), (m.name, r))
      case Expression.Div(Value.Reference(l), Value.Reference(r)) => List((m.name, l), (m.name, r))
      case _ => List()
  }.toSet

  // Evaluate the monkeys to their final values, following references
  def evaluate: Map[String, Long] =
    // Topological sort, evaluating the monkeys as we go

    // We need to check each monkey
    val monkeysToSort: MutableSet[String] = MutableSet.from(vertices.keySet)
    // All edges/references start being unresolved
    val unresolvedEdges: MutableSet[(String, String)] = MutableSet.from(edges)
    // Result
    val evaluatedMonkeys: MutableMap[String, Long] = MutableMap.empty

    while monkeysToSort.nonEmpty do
      // Find monkeys we haven't yet checked that have no dependencies
      val noDependencies = monkeysToSort -- unresolvedEdges.map(_._1)
      // If they have no dependencies they can be evaluated and removed from our to-sort set
      evaluatedMonkeys.addAll(noDependencies.map { name =>
        (name, vertices(name).op(evaluatedMonkeys.toMap))
      })
      monkeysToSort.subtractAll(noDependencies)
      // Resolve incoming edges for these monkeys
      unresolvedEdges.subtractAll(unresolvedEdges.filter(e => noDependencies.contains(e._2)))

    evaluatedMonkeys.toMap


object Day21 extends SolutionWithParser[List[Monkey], Long, Long]:
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

  override def solvePart1(input: List[Monkey]): Long =
    val evaled = MonkeyGraph(input).evaluate
    evaled("root")

  override def solvePart2(input: List[Monkey]): Long = ???


@main def run(): Unit = Day21.run()
@main def test(): Unit = Day21.test()
@main def testParser(): Unit =
  Day21.testParser(Day21.parser)
@main def runParser(): Unit =
  Day21.runParser(Day21.parser)