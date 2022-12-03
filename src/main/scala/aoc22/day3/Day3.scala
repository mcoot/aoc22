package aoc22.day3

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


// Domain models

case class Rucksack(items: List[Int])

// Parsing

object Parsing:
  def charRangeValueParser(start: Char, end: Char): Parser[Int] =
    Parser.charIn(start to end).map(_.toInt - start.toInt)

  def itemParser: Parser[Int] =
    charRangeValueParser('a', 'z').map(_ + 1) | charRangeValueParser('A', 'Z').map(_ + 27)

  def rucksackParser: Parser[Rucksack] = itemParser.rep.map(items => Rucksack(items.toList))

  def finalParser: Parser[List[Rucksack]] = CommonParsers.lineSeparated(rucksackParser)


// Solution

object Day3 extends SolutionWithParser[List[Rucksack], Int, Int]:
  override def dayNumber: Int = 3

  override def parser: Parser[List[Rucksack]] = Parsing.finalParser

  override def solvePart1(input: List[Rucksack]): Int = ???

  override def solvePart2(input: List[Rucksack]): Int = ???


@main def run(): Unit = Day3.run()
@main def test(): Unit = Day3.test()
@main def testParser(): Unit = Day3.testParser(Day3.parser)



