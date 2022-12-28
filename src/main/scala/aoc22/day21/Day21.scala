package aoc22.day21

import scala.collection.mutable
import scala.collection.mutable.Set as MutableSet
import scala.collection.mutable.Map as MutableMap
import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


enum Expression:
  case Literal(value: Long)
  // (We only have a single variable)
  case Variable
  case Reference(name: String)
  case Add(left: Expression, right: Expression)
  case Mult(left: Expression, right: Expression)
  case Minus(left: Expression, right: Expression)
  case Div(left: Expression, right: Expression)
  case Equals(left: Expression, right: Expression)

  // Throws if not a binary op,
  // returns the operands
  def binaryOperands: (Expression, Expression) =
    this match
      case Expression.Add(l, r) => (l, r)
      case Expression.Mult(l, r) => (l, r)
      case Expression.Minus(l, r) => (l, r)
      case Expression.Div(l, r) => (l, r)
      case Expression.Equals(l, r) => (l, r)
      case _ => throw Exception("Cannot get operands for non-binary op")

  def expand(refMap: Map[String, Expression]): Expression =
    this match
      // Nothing to expand for literals
      case Expression.Literal(_) => this
      // Nothing to expand for the variable either
      case Expression.Variable => this
      // Expand references directly
      case Expression.Reference(r) => refMap.getOrElse(r, throw Exception("Missing reference"))
      // Expand binary ops
      case Expression.Add(l, r) => Expression.Add(l.expand(refMap), r.expand(refMap))
      case Expression.Mult(l, r) => Expression.Mult(l.expand(refMap), r.expand(refMap))
      case Expression.Minus(l, r) => Expression.Minus(l.expand(refMap), r.expand(refMap))
      case Expression.Div(l, r) => Expression.Div(l.expand(refMap), r.expand(refMap))
      case Expression.Equals(l, r) => Expression.Equals(l.expand(refMap), r.expand(refMap))

  // Evaluate only implemented after expansion since we have to expand for part 2 anyway
  // For part 2 we should have re-arranged our expression before eval'ing
  // So there shouldn't be variables or equals operators
  def eval: Long =
    this match
      case Expression.Literal(v) => v
      case Expression.Variable => throw Exception("Encountered variable while evaluating")
      case Expression.Reference(r) => throw Exception("Unexpanded reference found")
      case Expression.Add(l, r) => l.eval + r.eval
      case Expression.Mult(l, r) => l.eval * r.eval
      case Expression.Minus(l, r) => l.eval - r.eval
      case Expression.Div(l, r) => l.eval / r.eval
      case Expression.Equals(_, _) => throw Exception("Equals found in eval")

  def rearrangeForVar: Expression.Equals =
    this match
      case Expression.Equals(l, r) => ???
      case _ => throw Exception("Attempted to re-arrange non-equals")

case class Monkey(name: String, op: Expression)


case class MonkeyGraph(monkeyList: List[Monkey]):
  val vertices: Map[String, Monkey] = Map.from(monkeyList.map(m => (m.name, m)))

  val edges: Set[(String, String)] = monkeyList.flatMap { m =>
    // Relying on our input set not having nesting etc.
    m.op match
      case Expression.Add(Expression.Reference(l), Expression.Reference(r)) => List((m.name, l), (m.name, r))
      case Expression.Mult(Expression.Reference(l), Expression.Reference(r)) => List((m.name, l), (m.name, r))
      case Expression.Minus(Expression.Reference(l), Expression.Reference(r)) => List((m.name, l), (m.name, r))
      case Expression.Div(Expression.Reference(l), Expression.Reference(r)) => List((m.name, l), (m.name, r))
      case Expression.Equals(Expression.Reference(l), Expression.Reference(r)) => List((m.name, l), (m.name, r))
      case Expression.Reference(r) => List((m.name, r))
      case _ => List()
  }.toSet

  // Expand out all expressions
  def expand: Map[String, Expression] =
    // Topological sort, expanding the monkeys as we go

    // We need to check each monkey
    val monkeysToSort: MutableSet[String] = MutableSet.from(vertices.keySet)
    // All edges/references start being unresolved
    val unresolvedEdges: MutableSet[(String, String)] = MutableSet.from(edges)
    // Result
    val evaluatedMonkeys: MutableMap[String, Expression] = MutableMap.empty

    while monkeysToSort.nonEmpty do
      // Find monkeys we haven't yet checked that have no dependencies
      val noDependencies = monkeysToSort -- unresolvedEdges.map(_._1)
      // If they have no dependencies they can be expanded and removed from our to-sort set
      evaluatedMonkeys.addAll(noDependencies.map { name =>
        (name, vertices(name).op.expand(evaluatedMonkeys.toMap))
      })
      monkeysToSort.subtractAll(noDependencies)
      // Resolve incoming edges for these monkeys
      unresolvedEdges.subtractAll(unresolvedEdges.filter(e => noDependencies.contains(e._2)))

    evaluatedMonkeys.toMap


object Day21 extends SolutionWithParser[List[Monkey], Long, Long]:
  override def dayNumber: Int = 21

  object Parsing:
    def name: Parser[String] = Parser.charIn('a' to 'z').rep.map(l => l.toList.mkString)

    def literal: Parser[Expression.Literal] = CommonParsers.int.map(Expression.Literal(_))

    def reference: Parser[Expression.Reference] = name.map(Expression.Reference(_))

    def binaryOperation: Parser[(Expression, Expression, Char)] =
      for
        l <- reference
        op <- (CommonParsers.spaces *> Parser.charIn(List('+', '-', '*', '/'))) <* CommonParsers.spaces
        r <- reference
      yield
        (l, r, op)

    // We don't have pure reference expressions which helps here
    def expr: Parser[Expression] =
      binaryOperation.map { (l, r, op) => op match
        case '+' => Expression.Add(l, r)
        case '*' => Expression.Mult(l, r)
        case '-' => Expression.Minus(l, r)
        case '/' => Expression.Div(l, r)
      } | literal

    def monkey: Parser[Monkey] =
      CommonParsers.pair(name, expr, Parser.string(": ")).map((n, e) => Monkey(n, e))

  override def parser: Parser[List[Monkey]] =
    CommonParsers.lineSeparated(Parsing.monkey)

  override def solvePart1(input: List[Monkey]): Long =
    val expanded = MonkeyGraph(input).expand
    expanded("root").eval

  override def solvePart2(input: List[Monkey]): Long =
    val adaptedInput = input.map {
      _ match
        case m if m.name == "humn" =>
          Monkey(m.name, Expression.Variable)
        case m if m.name == "root" =>
          val (l, r) = m.op.binaryOperands
          Monkey(m.name, Expression.Equals(l, r))
        case m => m
    }
    val expanded = MonkeyGraph(adaptedInput).expand
    val rearranged = expanded("root").rearrangeForVar
    rearranged.right.eval


@main def run(): Unit = Day21.run()
@main def test(): Unit = Day21.test()
@main def testParser(): Unit =
  Day21.testParser(Day21.parser)
@main def runParser(): Unit =
  Day21.runParser(Day21.parser)