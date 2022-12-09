package aoc22.day9

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

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

case class State(head: (Int, Int), tail: (Int, Int)):
  def step(dir: Direction): State =
    val (tailX, tailY) = tail
    val (newHeadX, newHeadY) = dir.move(head)
    val diff = (newHeadX - tailX, newHeadY - tailY)
    val newTail = diff match {
      // Tail two to the left
      case (x, y) if x == 2 && y == 0 => Direction.R.move(tail)
      // Tail two to the right
      case (x, y) if x == -2 && y == 0 => Direction.L.move(tail)
      // Tail two above
      case (x, y) if x == 0 && y == 2 => Direction.D.move(tail)
      // Tail two below
      case (x, y) if x == 0 && y == -2 => Direction.U.move(tail)
      // Diagonals

      // Left two, above
      case (x, y) if x == 2 && y == 1 => moveMulti(List(Direction.R, Direction.D), tail)
      // Left, above two
      case (x, y) if x == 1 && y == 2 => moveMulti(List(Direction.R, Direction.D), tail)

      // Left two, below
      case (x, y) if x == 2 && y == -1 => moveMulti(List(Direction.R, Direction.U), tail)
      // Left, below two
      case (x, y) if x == 1 && y == -2 => moveMulti(List(Direction.R, Direction.U), tail)

      // Right two, above
      case (x, y) if x == -2 && y == 1 => moveMulti(List(Direction.L, Direction.D), tail)
      // Right, above two
      case (x, y) if x == -1 && y == 2 => moveMulti(List(Direction.L, Direction.D), tail)

      // Right two, below
      case (x, y) if x == -2 && y == -1 => moveMulti(List(Direction.L, Direction.U), tail)
      // Right, below two
      case (x, y) if x == -1 && y == -2 => moveMulti(List(Direction.L, Direction.U), tail)

      // Already in position
      case (_, _) => tail
    }
    State((newHeadX, newHeadY), newTail)



object State:
  def initial: State = State((0, 0), (0, 0))


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
    input
      .flatMap(_.resolve)
      .foldLeft((Set.empty[State], State.initial)) { case ((prevStates, curState), dir) =>
        val newState = curState.step(dir)
        (prevStates + newState, newState)
      }(0).map(_.tail).toSet.size



  override def solvePart2(input: List[Instruction]): Int = ???


@main def run(): Unit = Day9.run()
@main def test(): Unit = Day9.test()
@main def testParser(): Unit =
  Day9.testParser(Day9.parser)
@main def runParser(): Unit =
  Day9.runParser(Day9.parser)