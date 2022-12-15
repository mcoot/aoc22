package aoc22.day12

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

import scala.collection.mutable
import scala.collection.mutable.{Map as MutableMap, Set as MutableSet}
import java.util.{Comparator, PriorityQueue}
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
      .filter(p => inBounds(p) && elevationDiff(pos, p) <= 1)
      .toSet

case class TerrainDist(pos: (Int, Int), dist: Int, prior: Option[(Int, Int)])

// Dijkstra's
def findBestPathToEnd(terrain: Terrain): List[(Int, Int)] =
  // Map of distances and the previous cell to backtrack from
  val distances: MutableMap[(Int, Int), TerrainDist] =
    MutableMap.from(terrain.allPositions.map(p => (p, TerrainDist(p, Int.MaxValue, None))))

  // Set the start's prior cell to be itself
  distances(terrain.start) = TerrainDist(terrain.start, 0, Some(terrain.start))

  // Set up a PQ - it's a max-pq so invert distance metric
  val pq = PriorityQueue[TerrainDist](1, Comparator.comparing(-_.dist))
  pq.offer(distances(terrain.start))

  // Dijkstra this
  while !pq.isEmpty do
    val td = pq.poll()

    terrain.accessibleNeighbours(td.pos).foreach { n =>
      val distViaHere = td.dist + 1
      if distViaHere < distances(n).dist then
        distances(n) = TerrainDist(n, distViaHere, Some(td.pos))
        pq.offer(distances(n))
    }

  // Trace back the shortest path
  var l: List[(Int, Int)] = List()
  var c = terrain.end
  while c != terrain.start do
    l = c :: l
    c = distances(c).prior.get

  c :: l.reverse


//  val memo: MutableMap[(Int, Int), Option[List[(Int, Int)]]] = MutableMap.empty
//
//  def dfsRec(visited: Set[(Int, Int)], pos: (Int, Int)): Option[List[(Int, Int)]] =
//    // Base case: are we already at the end?
//    if terrain.end == pos then
//      val bestFromHere = Some(List(terrain.end))
//      memo.put(pos, bestFromHere)
//
//    // Memoized case: have we already found the best path from here?
//    if memo.contains(pos) then
//      return memo(pos)
//
//    // Search out the possibilities from here
//    val fromHere = terrain
//      // Find the neighbouring tiles that are accessible
//      .accessibleNeighbours(pos)
//      // Filter out ones we've already tracked down
//      .filter(!visited.contains(_))
//      // Find the best paths from each neighbour to the goal
//      .map { neighbourPos =>
//        dfsRec(visited + pos, neighbourPos)
//          .map(pos :: _)
//      }
//    // Take the best one from here
//    val bestFromHere = if !fromHere.exists(_.isDefined) then
//      None
//    else
//      fromHere.filter(_.isDefined).minBy(_.get.size)
//    memo.put(pos, bestFromHere)
//    print(pos)
//    bestFromHere
//
//  dfsRec(Set.empty, terrain.start).get


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
    findBestPathToEnd(input).size - 1

  override def solvePart2(input: Terrain): Int = ???


@main def run(): Unit = Day12.run()
@main def test(): Unit = Day12.test()
@main def testParser(): Unit =
  Day12.testParser(Day12.parser)
@main def runParser(): Unit =
  Day12.runParser(Day12.parser)