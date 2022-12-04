package aoc22.day4

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


// Domain models

case class Assignment(start: Int, end: Int):
  def area = (start to end).toSet

case class AssignmentPair(a: Assignment, b: Assignment):
  def oneContainsOther = a.area.subsetOf(b.area) ||  b.area.subsetOf(a.area)

  def hasAnyOverlap = a.area.intersect(b.area).nonEmpty

// Parsing

object Parsing:
  def assignmentParser =
    CommonParsers.pair(CommonParsers.int, CommonParsers.int, Parser.char('-'))
      .map { case (s, e) => Assignment(s, e) }

  def assignmentPairParser =
    CommonParsers.pair(assignmentParser, assignmentParser, Parser.char(','))
      .map { case (a, b) => AssignmentPair(a, b) }

  def day4Parser = CommonParsers.lineSeparated(assignmentPairParser)


// Solution

object Day4 extends SolutionWithParser[List[AssignmentPair], Int, Int]:
  override def dayNumber: Int = 4

  override def parser = Parsing.day4Parser

  override def solvePart1(input: List[AssignmentPair]) =
    input.count(_.oneContainsOther)

  override def solvePart2(input: List[AssignmentPair]) =
    input.count(_.hasAnyOverlap)


@main def run(): Unit = Day4.run()
@main def test(): Unit = Day4.test()
@main def testParser(): Unit = Day4.testParser(Day4.parser)
