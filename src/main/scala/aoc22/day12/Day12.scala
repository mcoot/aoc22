package aoc22.day12

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

import scala.collection.mutable
import scala.collection.mutable.{Map as MutableMap, Set as MutableSet}
import java.util.{Comparator, PriorityQueue}
import scala.collection
import scala.util.control.Breaks.break


def heightFromChar(char: Char): Int = char - 'a'
def heightToChar(height: Int): Char = ('a' + height).toChar


case class Terrain(terrain: Array[Array[Int]], start: (Int, Int), end: (Int, Int)):
  def apply(pos: (Int, Int)): Int =
    if !inBounds(pos) then
      throw Exception(s"Accessing invalid position ${pos}")
    val (r, c) = pos
    terrain(r)(c)

  def allPositions: Seq[(Int, Int)] =
    for
      row <- terrain.indices
      col <- terrain(row).indices
    yield
      (row, col)

  def inBounds(pos: (Int, Int)): Boolean =
    val (r, c) = pos
    r >= 0 && c >= 0 && r < terrain.length && c < terrain(r).length

  def elevationDiff(posFrom: (Int, Int), posTo: (Int, Int)): Int =
    if !inBounds(posFrom) || !inBounds(posTo) then
      throw Exception(s"Finding elevation for invalid positions ${posFrom} ${posTo}")
    this(posTo) - this(posFrom)


  def accessibleNeighbours(pos: (Int, Int)): Set[(Int, Int)] =
    val (r, c) = pos
    // Up, down, left, right
    val neighbours = List(
      (r - 1, c),
      (r + 1, c),
      (r, c - 1),
      (r, c + 1),
    )
    neighbours
      // This elevation check is actually reversed from the rules
      // because we reverse start/end in our inputs
      .filter(p => inBounds(p) && elevationDiff(p, pos) <= 1)
      .toSet

case class TerrainDist(pos: (Int, Int), dist: Int, prior: Option[(Int, Int)])

// Dijkstra's
def findBestPathsToEnd(terrain: Terrain, startPos: (Int, Int), endPositions: List[(Int, Int)]): List[List[(Int, Int)]] =
  // Map of distances and the previous cell to backtrack from
  val distances: MutableMap[(Int, Int), TerrainDist] =
    MutableMap.from(terrain.allPositions.map(p => (p, TerrainDist(p, Int.MaxValue, None))))

  // Set the start's prior cell to be itself
  distances(startPos) = TerrainDist(startPos, 0, Some(startPos))

  // Set up a PQ - it's a max-pq so invert distance metric
  val pq = PriorityQueue[TerrainDist](1, Comparator.comparing(-_.dist))
  pq.offer(distances(startPos))

  // Dijkstra this
  while !pq.isEmpty do
    val td = pq.poll()

    terrain.accessibleNeighbours(td.pos).foreach { n =>
      val distViaHere = td.dist + 1
      if distViaHere < distances(n).dist then
        distances(n) = TerrainDist(n, distViaHere, Some(td.pos))
        pq.offer(distances(n))
    }

  // Trace back the shortest paths from each end pos and discard any that don't actually connect
  endPositions.flatMap { endPos =>
    // Trace back to the start, if possible
    val attemptedTrace = (endPos :: Iterable.unfold(endPos) { pos =>
      val td = distances(pos)
      td match
        // If prior is none there is no path that can be traced back
        case TerrainDist(_, _, None) => None
        // If prior is itself we're at the start
        case TerrainDist(_, _, Some(prior)) if prior == td.pos => None
        case TerrainDist(_, _, Some(prior)) => Some((prior, prior))
    }.toList).reverse

    if attemptedTrace.head != startPos then
      None
    else
      Some(attemptedTrace)
  }


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

  override def solvePart1(input: Terrain): Int =
    // Treat the end as the start for consistency with pt2
    findBestPathsToEnd(input, input.end, List(input.start)).head.size - 1

  override def solvePart2(input: Terrain): Int =
    // Treat the end as the start
    // Because dijkstra's is single-source all-ends
    val minElevationPositions = input.allPositions.filter(input(_) == 0)
    val paths = findBestPathsToEnd(input, input.end, minElevationPositions.toList)
    val pathSizes = paths.map(_.size - 1)
    pathSizes.min


@main def run(): Unit = Day12.run()
@main def test(): Unit = Day12.test()
@main def testParser(): Unit =
  Day12.testParser(Day12.parser)
@main def runParser(): Unit =
  Day12.runParser(Day12.parser)