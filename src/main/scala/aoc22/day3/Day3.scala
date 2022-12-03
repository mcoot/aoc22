package aoc22.day3

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


case class Rucksack(items: List[Int]):
  def firstCompartment: List[Int] = items.take(items.size / 2)
  def secondCompartment: List[Int] = items.takeRight(items.size / 2)

  def itemInBoth: Int = firstCompartment.intersect(secondCompartment).head

  def findBadgeWith(a: Rucksack, b: Rucksack): Int =
    items.intersect(a.items).intersect(b.items).head


object Parsing:
  def charRangeValueParser(start: Char, end: Char): Parser[Int] =
    Parser.charIn(start to end).map(_.toInt - start.toInt)

  def itemParser: Parser[Int] =
    charRangeValueParser('a', 'z').map(_ + 1) | charRangeValueParser('A', 'Z').map(_ + 27)

  def rucksackParser: Parser[Rucksack] = itemParser.rep.map(items => Rucksack(items.toList))

  def finalParser: Parser[List[Rucksack]] = CommonParsers.lineSeparated(rucksackParser)


object Day3 extends SolutionWithParser[List[Rucksack], Int, Int]:
  override def dayNumber: Int = 3

  override def parser: Parser[List[Rucksack]] = Parsing.finalParser

  override def solvePart1(input: List[Rucksack]): Int =
    input.map(_.itemInBoth).sum

  override def solvePart2(input: List[Rucksack]): Int =
    input.grouped(3).map { case List(a, b, c) => a.findBadgeWith(b, c) }.sum


@main def run(): Unit = Day3.run()
@main def test(): Unit = Day3.test()
@main def testParser(): Unit = Day3.testParser(Day3.parser)



