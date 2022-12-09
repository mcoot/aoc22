package aoc22.day9

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

import scala.collection.mutable

enum Direction(val char: Char):
  case L extends Direction('L')
  case U extends Direction('U')
  case R extends Direction('R')
  case D extends Direction('D')

  def move(pos: (Int, Int)): (Int, Int) =
    val (x, y) = pos
    // Coords origin at (0, 0) with (right, down) being positive coords
    this match {
      case Direction.L => (x - 1, y)
      case Direction.U => (x, y - 1)
      case Direction.R => (x + 1, y)
      case Direction.D => (x, y + 1)
    }


def moveMulti(dirs: List[Direction], initial: (Int, Int)): (Int, Int) =
  dirs.foldLeft(initial) { case (pos, dir) => dir.move(pos) }


case class Instruction(dir: Direction, distance: Int):
  def resolve: List[Direction] = (0 until distance).map(_ => dir).toList

case class State(head: (Int, Int), tail: List[(Int, Int)]):
  def lastKnot: (Int, Int) = tail.last

  private def tailKnotStep(leader: (Int, Int), knot: (Int, Int)): (Int, Int) =
    val (leaderX, leaderY) = leader
    val (knotX, knotY) = knot
    val diff = (leaderX - knotX, leaderY - knotY)
    diff match {
      // Tail two to the left
      case (x, y) if x == 2 && y == 0 => Direction.R.move(knot)
      // Tail two to the right
      case (x, y) if x == -2 && y == 0 => Direction.L.move(knot)
      // Tail two above
      case (x, y) if x == 0 && y == 2 => Direction.D.move(knot)
      // Tail two below
      case (x, y) if x == 0 && y == -2 => Direction.U.move(knot)
      // Diagonals

      // Left two, above
      case (x, y) if x == 2 && y == 1 => moveMulti(List(Direction.R, Direction.D), knot)
      // Left, above two
      case (x, y) if x == 1 && y == 2 => moveMulti(List(Direction.R, Direction.D), knot)

      // Left two, below
      case (x, y) if x == 2 && y == -1 => moveMulti(List(Direction.R, Direction.U), knot)
      // Left, below two
      case (x, y) if x == 1 && y == -2 => moveMulti(List(Direction.R, Direction.U), knot)

      // Right two, above
      case (x, y) if x == -2 && y == 1 => moveMulti(List(Direction.L, Direction.D), knot)
      // Right, above two
      case (x, y) if x == -1 && y == 2 => moveMulti(List(Direction.L, Direction.D), knot)

      // Right two, below
      case (x, y) if x == -2 && y == -1 => moveMulti(List(Direction.L, Direction.U), knot)
      // Right, below two
      case (x, y) if x == -1 && y == -2 => moveMulti(List(Direction.L, Direction.U), knot)

      // Left two, above two
      case (x, y) if x == 2 && y == 2 => moveMulti(List(Direction.R, Direction.D), knot)
      // Left two, below two
      case (x, y) if x == 2 && y == -2 => moveMulti(List(Direction.R, Direction.U), knot)
      // Right two, above two
      case (x, y) if x == -2 && y == 2 => moveMulti(List(Direction.L, Direction.D), knot)
      // Right two, below two
      case (x, y) if x == -2 && y == -2 => moveMulti(List(Direction.L, Direction.U), knot)

      // Already in position
      case (_, _) => knot
    }

  def step(dir: Direction): State =
    val newHead = dir.move(head)
    val newTail = tail.foldLeft(List(newHead)) { case (acc, knot) =>
      tailKnotStep(acc.head, knot) :: acc
    }.reverse.drop(1)
    State(newHead, newTail)

  def printGrid(minBound: (Int, Int), maxBound: (Int, Int)): String =
    val (minX, minY) = minBound
    val (maxX, maxY) = maxBound
    val sb = mutable.StringBuilder()
    for y <- minY to maxY do
      for x <- minX to maxX do
        if head == (x, y) then
          sb.append('H')
        else if tail.contains((x, y)) then
          sb.append(((tail.indexOf((x, y))) + 1).toString)
        else
          sb.append('.')
      sb.append('\n')
    sb.mkString


object State:
  def initial(trailingKnots: Int): State = State((0, 0), (0 until trailingKnots).map(_ => (0, 0)).toList)


case class ExecutionResult(prevStates: List[State], finalState: State)


def executeInstructions(instructions: List[Instruction], startingState: State): ExecutionResult =
  val (prevStates, finalState) = instructions
    .flatMap(_.resolve)
    .foldLeft((List.empty[State], startingState)) { case ((prevStates, curState), dir) =>
      val newState = curState.step(dir)
      (newState :: prevStates, newState)
    }
  ExecutionResult(prevStates, finalState)


object Parsing:
  def directionParser: Parser[Direction] =
    Parser.charIn(Direction.values.map(_.char))
      .map { c =>
        Direction
          .values
          .find(c == _.char)
          .getOrElse(throw Exception("Invalid direction"))
      }

  def instructionParser: Parser[Instruction] =
    CommonParsers.pair(directionParser, CommonParsers.int, Parser.char(' '))
      .map { case (dir, dist) => Instruction(dir, dist) }

  def inputParser: Parser[List[Instruction]] = CommonParsers.lineSeparated(instructionParser)


object Day9 extends SolutionWithParser[List[Instruction], Int, Int]:
  override def dayNumber: Int = 9

  override def parser: Parser[List[Instruction]] = Parsing.inputParser

  override def solvePart1(input: List[Instruction]): Int =
    executeInstructions(input, State.initial(1))
      .prevStates
      .map(_.lastKnot)
      .toSet
      .size

  override def solvePart2(input: List[Instruction]): Int =
    executeInstructions(input, State.initial(9))
      .prevStates
      .map(_.lastKnot)
      .toSet
      .size


@main def run(): Unit = Day9.run()
@main def test(): Unit = Day9.test("2")
@main def testParser(): Unit =
  Day9.testParser(Day9.parser)
@main def runParser(): Unit =
  Day9.runParser(Day9.parser)