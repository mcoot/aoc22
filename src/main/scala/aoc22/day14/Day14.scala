package aoc22.day14

import scala.util.control.Breaks.*
import scala.collection.mutable.Set as MutableSet
import aoc22.common.{CommonParsers, SolutionWithParser}
import aoc22.day14.CaveState.INITIAL_SAND_POSITION
import cats.parse.Parser

import scala.collection.mutable


case class Position(x: Int, y: Int):
  def this(p: (Int, Int)) = this(p(0), p(1))
  def pair: (Int, Int) = (x, y)


extension (p: (Int, Int))
  def pos: Position = Position(p(0), p(1))

case class RockPathSegment(start: Position, end: Position):
  def positions: Seq[Position] =
    val (diffX, diffY) = (end.x - start.x, end.y - start.y)
    val magnitude = List(Math.abs(diffX), Math.abs(diffY)).max
    (0 to magnitude)
      .map(i => Position(start.x + i * Math.signum(diffX).toInt, start.y + i * Math.signum(diffY).toInt))



case class RockPath(path: List[Position]):
  def segments: List[RockPathSegment] =
    path.zip(path.drop(1)).map { case (s, e) => RockPathSegment(s, e) }



enum SandMove:
  case Down
  case DownLeft
  case DownRight
  case FallingForever
  case Blocked


class CaveState(val paths: List[RockPath], val hasFloor: Boolean):
  val rocks: Set[Position] =
    paths.flatMap { path =>
      path.segments.flatMap(_.positions)
    }.toSet

  // The bottom of the lowest rock - any sand which reaches this will fall forever
  val boundY: Int = rocks.map(_.y).max

  // Y value of the floor for part 2
  val floorY: Int = boundY + 2

  val sandAtRest: MutableSet[Position] = MutableSet.empty

  var sandInMotion: Position = INITIAL_SAND_POSITION

  var isTerminated = false

  def xBounds: (Int, Int) =
    val occupied = Set(rocks.map(_.x), sandAtRest.map(_.x)).flatten
    (occupied.min, occupied.max)

  def isEmpty(pos: Position): Boolean =
    !rocks.contains(pos) && !sandAtRest.contains(pos) && (!hasFloor || pos.y < floorY)

  def printState: String =
    val (minX, maxX) = xBounds
    val sb = mutable.StringBuilder()
    for y <- 0 to floorY do
      for x <- minX to maxX do
        if rocks.contains((x, y).pos) || y >= floorY then
          sb.append('#')
        else if sandAtRest.contains((x, y).pos) then
          sb.append('o')
        else if sandInMotion == (x, y).pos then
          sb.append('+')
        else
          sb.append('.')
      sb.append('\n')
    sb.mkString

  def genMove: SandMove = sandInMotion match
    case Position(_, y) if y >= boundY && !hasFloor => SandMove.FallingForever
    case Position(x, y) if isEmpty((x, y + 1).pos) => SandMove.Down
    case Position(x, y) if isEmpty((x - 1, y + 1).pos) => SandMove.DownLeft
    case Position(x, y) if isEmpty((x + 1, y + 1).pos) => SandMove.DownRight
    case _ => SandMove.Blocked

  def nextPosForSandInMotion(move: SandMove): Position =
    val Position(x, y) = sandInMotion
    move match
      case SandMove.Down => (x, y + 1).pos
      case SandMove.DownLeft => (x - 1, y + 1).pos
      case SandMove.DownRight => (x + 1, y + 1).pos
      case SandMove.FallingForever => (x, y + 1).pos
      case SandMove.Blocked => (x, y).pos

  def step(): Unit =
    if isTerminated then
      return

    val move = genMove
    move match
      // Falling forever into the void
      case SandMove.FallingForever =>
        // Sand falling into the void
        isTerminated = true
      case SandMove.Blocked =>
        if isEmpty(INITIAL_SAND_POSITION) then
          sandAtRest.addOne(sandInMotion)
          sandInMotion = INITIAL_SAND_POSITION
        else
          // Cave filled up to the entry point
          isTerminated = true
      case _ =>
        sandInMotion = nextPosForSandInMotion(move)

  def stepUntilNextGrain(): Unit =
    breakable {
      while !isTerminated do
        val curAtRestSize = sandAtRest.size
        step()
        if sandAtRest.size > curAtRestSize then
          break()
    }

  def simulateToFinish(): Unit =
    var idx = 0
    while !isTerminated do
      stepUntilNextGrain()
      idx += 1
    println(printState)


object CaveState:
  val INITIAL_SAND_POSITION: Position = Position(500, 0)


object Day14 extends SolutionWithParser[List[RockPath], Int, Int]:
  override def dayNumber: Int = 14

  private def positionParser: Parser[Position] =
    CommonParsers.pair(CommonParsers.int, CommonParsers.int, Parser.char(','))
      .map(Position.apply)

  private def rockPathParser: Parser[RockPath] =
    CommonParsers.separated(positionParser, Parser.string(" -> "))
      .map(RockPath.apply)

  override def parser: Parser[List[RockPath]] =
    CommonParsers.lineSeparated(rockPathParser)

  override def solvePart1(input: List[RockPath]): Int =
    val cs = CaveState(input, false)
    cs.simulateToFinish()
    cs.sandAtRest.size

  override def solvePart2(input: List[RockPath]): Int =
    val cs = CaveState(input, true)
    cs.simulateToFinish()
    cs.sandAtRest.size


@main def run(): Unit = Day14.run()
@main def test(): Unit = Day14.test()
@main def testParser(): Unit =
  Day14.testParser(Day14.parser)
@main def runParser(): Unit =
  Day14.runParser(Day14.parser)
