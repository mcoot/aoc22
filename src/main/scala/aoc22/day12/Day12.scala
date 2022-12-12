package aoc22.day12

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


def heightFromChar(char: Char): Int = char - 'a'
def heightToChar(height: Int): Char = ('a' + height).toChar


case class Terrain(terrain: Array[Array[Int]], start: (Int, Int), end: (Int, Int))


object Parsing:
  enum LocationParseValue(val value: Int):
    case StartLocation extends LocationParseValue(heightFromChar('a'))
    case EndLocation extends LocationParseValue(heightFromChar('z'))
    case Location(c: Char) extends LocationParseValue(heightFromChar(c))

  def startLoc: Parser[LocationParseValue] =
    Parser.char('S').map(_ => LocationParseValue.StartLocation)

  def endLoc: Parser[LocationParseValue] =
    Parser.char('E').map(_ => LocationParseValue.EndLocation)

  def regularLoc: Parser[LocationParseValue] =
    Parser.charIn('a' to 'z').map(c => LocationParseValue.Location(c))

  def loc: Parser[LocationParseValue] = startLoc | endLoc | regularLoc

  def locArray: Parser[Array[Array[LocationParseValue]]] =
    CommonParsers.lineSeparated(loc.rep(1).map(l => Array.from(l.toList))).map(r => Array.from(r))

  def inputParser: Parser[Terrain] = locArray.map { arr =>
    var start = (0, 0)
    var end = (0, 0)
    for (row, rowIdx) <- arr.zipWithIndex do
      for (l, colIdx) <- row.zipWithIndex do
        l match
          case LocationParseValue.StartLocation => start = (rowIdx, colIdx)
          case LocationParseValue.EndLocation => end = (rowIdx, colIdx)
          case _ => ()
    val finalTerrain = arr.map(row => row.map(loc => loc.value))
    Terrain(finalTerrain, start, end)
  }


object Day12 extends SolutionWithParser[Terrain, Int, Int]:
  override def dayNumber: Int = 12

  override def parser: Parser[Terrain] = Parsing.inputParser

  override def solvePart1(input: Terrain): Int = ???

  override def solvePart2(input: Terrain): Int = ???


@main def run(): Unit = Day12.run()
@main def test(): Unit = Day12.test()
@main def testParser(): Unit =
  Day12.testParser(Day12.parser)
@main def runParser(): Unit =
  Day12.runParser(Day12.parser)