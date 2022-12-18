package aoc22.day15

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


case class Position(x: Int, y: Int):
  def this(p: (Int, Int)) = this(p(0), p(1))
  def pair: (Int, Int) = (x, y)


extension (p: (Int, Int))
  def pos: Position = Position(p(0), p(1))


case class Sensor(pos: Position, nearestBeacon: Position)


object Day15 extends SolutionWithParser[List[Sensor], Int, Int]:
  override def dayNumber: Int = 15

  private def positionParser: Parser[Position] =
    for
      _ <- Parser.string("x=")
      x <- CommonParsers.int
      _ <- Parser.string(", y=")
      y <- CommonParsers.int
    yield
      Position(x, y)

  private def sensorParser: Parser[Sensor] =
    for
      _ <- Parser.string("Sensor at ")
      pos <- positionParser
      _ <- Parser.string(": closest beacon is at ")
      nearestBeacon <- positionParser
    yield
      Sensor(pos, nearestBeacon)

  override def parser: Parser[List[Sensor]] =
    CommonParsers.lineSeparated(sensorParser)

  override def solvePart1(input: List[Sensor]): Int = ???

  override def solvePart2(input: List[Sensor]): Int = ???


@main def run(): Unit = Day15.run()
@main def test(): Unit = Day15.test()
@main def testParser(): Unit =
  Day15.testParser(Day15.parser)
@main def runParser(): Unit =
  Day15.runParser(Day15.parser)