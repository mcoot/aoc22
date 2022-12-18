package aoc22.day14

import aoc22.common.{CommonParsers, SolutionWithParser}
import aoc22.day14.CaveState.INITIAL_SAND_POSITION
import cats.parse.Parser


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


case class CaveState(paths: List[RockPath], sandAtRest: Set[Position], sandInMotion: Position):
  val rocks: Set[Position] =
    paths.flatMap { path =>
      path.segments.flatMap(_.positions)
    }.toSet

  // The bottom of the lowest rock - any sand which reaches this will fall forever
  val boundY: Int = rocks.map(_.y).max

  def isEmpty(pos: Position): Boolean = !rocks.contains(pos) && !sandAtRest.contains(pos)

  def genMove: SandMove = sandInMotion match
    case Position(_, y) if y >= boundY => SandMove.FallingForever
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

  def step: Option[CaveState] =
    val move = genMove
    move match
      case SandMove.FallingForever => None
      case SandMove.Blocked => Some(copy(sandAtRest = sandAtRest + sandInMotion, sandInMotion = INITIAL_SAND_POSITION))
      case _ => Some(copy(sandInMotion = nextPosForSandInMotion(move)))


  def simulateToFinish: CaveState =
    Iterable.unfold((this, 0)) { case (state, idx) =>
      if (idx + 1) % 1000 == 0 then
        println(s"Step ${idx+1}")
      state.step.map(s => (s, (s, idx + 1)))
    }
      .last


object CaveState:
  val INITIAL_SAND_POSITION: Position = Position(500, 0)

  def initial(paths: List[RockPath]): CaveState = CaveState(paths, Set.empty, INITIAL_SAND_POSITION)


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
    CaveState.initial(input)
      .simulateToFinish
      .sandAtRest
      .size

  override def solvePart2(input: List[RockPath]): Int =
    ???


@main def run(): Unit = Day14.run()
@main def test(): Unit = Day14.test()
@main def testParser(): Unit =
  Day14.testParser(Day14.parser)
@main def runParser(): Unit =
  Day14.runParser(Day14.parser)
