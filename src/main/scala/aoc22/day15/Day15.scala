package aoc22.day15

import scala.collection.mutable.Set as MutableSet
import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

import scala.annotation.targetName


case class Position(x: Int, y: Int):
  def this(p: (Int, Int)) = this(p(0), p(1))
  def pair: (Int, Int) = (x, y)

  def hdist(otherX: Int): Int = Math.abs(otherX - x)
  def hdist(other: Position): Int = hdist(other.x)

  def vdist(otherY: Int): Int = Math.abs(otherY - y)
  def vdist(other: Position): Int = vdist(other.y)

  @targetName("add")
  def +(other: Position): Position = Position(x + other.x, y + other.y)

  def inBounds(minBound: Position, maxBound: Position): Boolean =
    val Position(minX, minY) = minBound
    val Position(maxX, maxY) = maxBound
    x >= minX && x <= maxX && y >= minY && y <= maxY

  def manhattan(other: Position): Int = hdist(other) + vdist(other)

  def tuningFrequency: Long = x.toLong * 4000000L + y.toLong


extension (p: (Int, Int))
  def pos: Position = Position(p(0), p(1))


case class Sensor(pos: Position, nearestBeacon: Position):
  def distanceToNearest: Int = pos.manhattan(nearestBeacon)

  def inRangeOfRow(rowY: Int): Boolean = pos.vdist(rowY) <= distanceToNearest

  def positionsExcludedInRow(rowY: Int): Set[Position] =
    if !inRangeOfRow(rowY) then
      return Set.empty

    val possibleHorDist = distanceToNearest - pos.vdist(rowY)
    val (minX, maxX) = (pos.x - possibleHorDist, pos.x + possibleHorDist)
    (minX to maxX)
      .map(x => (x, rowY).pos)
      .filter(_ != nearestBeacon)
      .toSet

  def positionsExcluded: MutableSet[Position] =
    val d = distanceToNearest
    val excluded: MutableSet[Position] = MutableSet.empty
    for
      x <- pos.x - d to pos.x + d
      y <- pos.y - d to pos.y + d
      if pos.manhattan((x, y).pos) <= d && (x, y).pos != nearestBeacon
    do
      excluded.addOne((x, y).pos)
    excluded


def findPositionsInRowWhichCannotHaveBeacons(sensors: List[Sensor], rowY: Int): Set[Position] =
  // Ignore sensors far enough from this row that their sensor zones do not overlap it
  val relevantSensors = sensors.filter(s => s.inRangeOfRow(rowY))
  println(s"There were ${relevantSensors.size} relevant sensors")

  val excluded: MutableSet[Position] = MutableSet.empty
  for (sensor, i) <- relevantSensors.zipWithIndex do
    println(s"Handling sensor ${i}")
    excluded.addAll(sensor.positionsExcludedInRow(rowY))
  excluded.toSet


def findPositionOfBeacon(sensors: List[Sensor], maxBound: Int): Position =
  def isBlocked(p: Position): Boolean =
    sensors.exists(s => p.manhattan(s.pos) <= s.distanceToNearest)

  // For there to be precisely one beacon, it must be _just_ beyond the boundary
  // of sensor ranges. So we can restrict the search to there
  sensors.zipWithIndex.flatMap { case (s, idx) =>
    println(s"Working on sensor ${idx+1}/${sensors.size}, radius ${s.distanceToNearest}")
    for
      x <- (-s.distanceToNearest - 1) to (s.distanceToNearest + 1)
      yDir <- List(-1, 1)
      y = yDir * (s.distanceToNearest + 1 - Math.abs(x))
      p = s.pos + (x, y).pos
      if !isBlocked(p) && p.inBounds((0, 0).pos, (maxBound, maxBound).pos)
    yield
      p
  }.head


object Day15 extends SolutionWithParser[List[Sensor], Long, Long]:
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

  override def solvePart1(input: List[Sensor]): Long = 0
//    findPositionsInRowWhichCannotHaveBeacons(input, 2000000).size

  override def solvePart2(input: List[Sensor]): Long =
    findPositionOfBeacon(input, 4000000).tuningFrequency


@main def run(): Unit = Day15.run()
@main def test(): Unit = Day15.test()
@main def testParser(): Unit =
  Day15.testParser(Day15.parser)
@main def runParser(): Unit =
  Day15.runParser(Day15.parser)