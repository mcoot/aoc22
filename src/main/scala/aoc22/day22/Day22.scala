package aoc22.day22

import aoc22.common.{CommonParsers, Point2D, SolutionWithParser, pos}
import cats.parse.Parser

import scala.annotation.tailrec


enum TurnDirection:
  case L
  case R


enum Direction:
  case Left
  case Right
  case Up
  case Down

  def moveVec: Point2D = this match
    case Direction.Left => (-1, 0).pos
    case Direction.Right => (1, 0).pos
    case Direction.Up => (0, -1).pos
    case Direction.Down => (0, 1).pos

  def isHor: Boolean = Direction.horDirections.contains(this)
  def isVert: Boolean = Direction.vertDirections.contains(this)

  def flip: Direction = this match
    case Direction.Left => Direction.Right
    case Direction.Right => Direction.Left
    case Direction.Up => Direction.Down
    case Direction.Down => Direction.Up

  def turn(td: TurnDirection): Direction = (this, td) match
    // counter-clockwise
    case (Direction.Left, TurnDirection.L) => Direction.Down
    case (Direction.Down, TurnDirection.L) => Direction.Right
    case (Direction.Right, TurnDirection.L) => Direction.Up
    case (Direction.Up, TurnDirection.L) => Direction.Left
    // clockwise
    case (Direction.Left, TurnDirection.R) => Direction.Up
    case (Direction.Up, TurnDirection.R) => Direction.Right
    case (Direction.Right, TurnDirection.R) => Direction.Down
    case (Direction.Down, TurnDirection.R) => Direction.Left

  def facingVal: Int = this match
    case Direction.Right => 0
    case Direction.Down => 1
    case Direction.Left => 2
    case Direction.Up => 3


object Direction:
  def horDirections: List[Direction] = List(Direction.Left, Direction.Right)
  def vertDirections: List[Direction] = List(Direction.Up, Direction.Down)
  def directions: List[Direction] = horDirections ++ vertDirections


enum Instruction:
  case Forward(steps: Int)
  case Turn(dir: TurnDirection)


// An individual rectangular plane
case class Plane(id: Int, posInNet: Point2D, arr: Array[Array[Boolean]]):
  def sideLength: Int = arr.length

  def netPositionFor(posInPlane: Point2D): Point2D =
    // Scale the position of this plane in the net by the side length, and add the pos within the plane
    posInNet * sideLength + posInPlane

  def maxBound: Point2D = (arr.head.length - 1, arr.length - 1).pos

  def isWall(pos: Point2D): Boolean = arr(pos.y)(pos.x)


// Raw grid data as a a 2d grid of square segments
case class PlaneGrid(arr: Array[Array[Option[Plane]]]):
  val planes: Map[Int, Plane] = arr
    .flatMap(row => row.collect { case Some(p) => p } )
    .map(p => (p.id, p))
    .toMap

  val startingPlane: Plane = planes.values.filter(_.posInNet.y == 0).minBy(_.posInNet.x)

  val gridMaxBound: Point2D = (arr.head.length - 1, arr.length - 1).pos

  def apply(gridCoord: Point2D): Option[Plane] = arr(gridCoord.y)(gridCoord.x)


// Describes where the player is by the plane they're on, position in the plane and cur direction
case class PlayerPos(plane: Plane, pos: Point2D, dir: Direction):
  def turn(td: TurnDirection): PlayerPos = copy(dir = dir.turn(td))

  def step: PlayerPos = copy(pos = pos + dir.moveVec)

  def score: Long =
    val netPos = plane.netPositionFor(pos)
    (netPos.y + 1) * 1000 + (netPos.x + 1) * 4 + dir.facingVal


// When moving across planes, a coordinate may change in one of several ways\
enum CoordTransition:
  // The coordinate could wrap to 0
  case ToZero
  // The coordinate could wrap to the max value for that plane
  case ToMax
  // The coordinate could remain unchanged
  case Maintained
  // The coordinate could be swapped for the other coordinate
  case Swapped

  def applyToCoordinate(coord: Int, otherCoord: Int, max: Int): Int = this match
    case CoordTransition.ToZero => 0
    case CoordTransition.ToMax => max
    case CoordTransition.Maintained => coord
    case CoordTransition.Swapped => otherCoord

// Describes how the player's position is mapped when moving from one plane to another
// Theory: the direction the player will be facing can be inferred from their existing direction
// and the position translation
// TODO: I NEED A CUBE
case class EdgeTransition(toPlane: Plane, x: CoordTransition, y: CoordTransition, newDir: Direction):
  protected def applyToPos(pos: Point2D): Point2D =
    (x.applyToCoordinate(pos.x, pos.y, toPlane.maxBound.x), y.applyToCoordinate(pos.y, pos.x, toPlane.maxBound.y)).pos

  // Take one step over the edge...
  def apply(pos: PlayerPos): PlayerPos =
    PlayerPos(toPlane, applyToPos(pos.pos), newDir)


//    (x, y, dir) match
//      // top to front edge, x value maintained on the new plane, y becomes 0, direction remains down
//      case (CoordTransition.Maintained, CoordTransition.ToZero, Direction.Down) => Direction.Down
//      // top to back edge, x value maintained, y becomes max, direction remains up
//      case (CoordTransition.Maintained, CoordTransition.ToMax, Direction.Up) => Direction.Up
//      // top to right edge, x value becomes y from the prev plane, y becomes 0, direction becomes down
//      case (CoordTransition.Swapped, CoordTransition.ToZero, Direction.Right) => Direction.Down
//      // top to left edge, x value becomes y from the prev plane, y becomes 0, direction becomes down
//      case (CoordTransition.Swapped, CoordTransition.ToZero, Direction.Left) => Direction.Down
//      case _ => throw Exception("No direction mapping for edge transition")


// For part 1 we just need to consider wrapping transitions on a 2D plane
def part1TransitionFor(planeGrid: PlaneGrid, fromPlane: Plane, dir: Direction): EdgeTransition =
  // Move along the direction, potentially wrapping
  var movedTo = (fromPlane.posInNet + dir.moveVec).floorMod(planeGrid.gridMaxBound + (1, 1).pos)
  while planeGrid(movedTo).isEmpty do
    movedTo = (movedTo + dir.moveVec).floorMod(planeGrid.gridMaxBound + (1, 1).pos)

  val toPlane = planeGrid(movedTo).get
  dir match
    case Direction.Left => EdgeTransition(toPlane, CoordTransition.ToMax, CoordTransition.Maintained, Direction.Left)
    case Direction.Right => EdgeTransition(toPlane, CoordTransition.ToZero, CoordTransition.Maintained, Direction.Right)
    case Direction.Up => EdgeTransition(toPlane, CoordTransition.Maintained, CoordTransition.ToMax, Direction.Up)
    case Direction.Down => EdgeTransition(toPlane, CoordTransition.Maintained, CoordTransition.ToZero, Direction.Down)


// A set of planes with information about how to map between them
case class GroveMapPt1(grid: PlaneGrid):
  val initialPosition: PlayerPos = PlayerPos(grid.startingPlane, (0, 0).pos, Direction.Right)

  // Attempt to step forward, failing if blocked by a wall
  private def applyStep(priorPos: PlayerPos): Option[PlayerPos] =
    var nextPos = priorPos.step

    // Transition to the new plane if we need to
    if !nextPos.pos.inBounds((0, 0).pos, nextPos.plane.maxBound) then
      // Position right before we transit between planes
      val posBeforeTransition: PlayerPos =
        nextPos.copy(pos = nextPos.pos.constrainToBounds((0, 0).pos, nextPos.plane.maxBound))
      // Transition one step to cross planes
      nextPos = part1TransitionFor(grid, priorPos.plane, priorPos.dir)(posBeforeTransition)

    // Whichever plane we are on now, see if we can actually move here
    if nextPos.plane.isWall(nextPos.pos) then
    // Hit a wall, cannot move
      None
    else
    // Move successfully to that spot
      Some(nextPos)
  @tailrec
  private def applySteps(priorPos: PlayerPos, steps: Int): PlayerPos =
    if steps <= 0 then
      priorPos
    else
      // Attempt to apply the next step, stopping if we hit a wall
      applyStep(priorPos) match
        case Some(nextPos) => applySteps(nextPos, steps - 1)
        case None => priorPos

  def applyInstruction(priorPos: PlayerPos, instruction: Instruction): PlayerPos =
    instruction match
      case Instruction.Forward(steps) => applySteps(priorPos, steps)
      case Instruction.Turn(td) => priorPos.turn(td)


  def execute(instructions: List[Instruction]): PlayerPos =
    instructions.foldLeft(initialPosition) { case (pos, instruction) =>
      applyInstruction(pos, instruction)
    }


object Day22 extends SolutionWithParser[(PlaneGrid, List[Instruction]), Long, Long]:
  override def dayNumber: Int = 22

  private object Parsing:
    private def turnDir: Parser[TurnDirection] =
      Parser.char('L').map(_ => TurnDirection.L) | Parser.char('R').map(_ => TurnDirection.R)

    private def instruction: Parser[Instruction] =
      turnDir.map(Instruction.Turn(_)) | CommonParsers.int.map(Instruction.Forward(_))

    def instructionList: Parser[List[Instruction]] =
      instruction.rep(1).map(_.toList)

    def rawTileParser: Parser[Option[Boolean]] =
      Parser.char(' ').map(_ => None) | Parser.char('.').map(_ => Some(false)) | Parser.char('#').map(_ => Some(true))

    def rawGridLineParser: Parser[Array[Option[Boolean]]] =
      rawTileParser.rep(1).map(l => Array.from(l.toList))

    def rawGridData: Parser[PlaneGrid] =
      CommonParsers.lineSeparated(rawGridLineParser).map { lines =>
        // First let's convert our lines into a raw array extending all lines to be of equal length
        val maxLength = lines.map(_.length).max
        val rawArr: Array[Array[Option[Boolean]]] = Array.from(
          lines.map { l =>
            // Copy the line we have, appending -1s to the max length
            // so we get a consistent 2D grid
            val res: Array[Option[Boolean]] = Array.fill(maxLength)(None)
            l.zipWithIndex.foreach((v, idx) => res.update(idx, v))
            res
          }
        )

        // We need to find the side length of the cube to be able to segment into planes
        // In test and input we have at least some section where the width is at most one side length
        // So if we find the min number of Some()s in a row we have the side length!
        val sideLength = lines.map(l => l.collect { case Some(b) => b }.length).min

        val numSquareRows = rawArr.length / sideLength
        val numSquareCols = rawArr.head.length / sideLength

        val squares: Array[Array[Array[Array[Option[Boolean]]]]] = Array.ofDim(numSquareRows, numSquareCols)

        for squareRowIdx <- 0 until numSquareRows do
          val tileRowStartIdx = squareRowIdx * sideLength
          for squareColIdx <- 0 until numSquareCols do
            val tileColStartIdx = squareColIdx * sideLength
            squares(squareRowIdx)(squareColIdx) =
              rawArr
                .slice(tileRowStartIdx, tileRowStartIdx + sideLength)
                .map(_.slice(tileColStartIdx, tileColStartIdx + sideLength))

        def isDefinedPlane(s: Array[Array[Option[Boolean]]]) = s.forall(row => row.forall(_.isDefined))
        def isEmptyPlane(s: Array[Array[Option[Boolean]]]) = s.forall(row => row.forall(_.isEmpty))

        // Each square is either a real plane, or empty
        var nextPlaneId = -1
        val arr = squares.zipWithIndex.map { (squareRow, rowIdx) =>
          squareRow.zipWithIndex.map { (square, colIdx) =>
            if isDefinedPlane(square) then
              // A plane
              nextPlaneId += 1
              Some(Plane(nextPlaneId, (colIdx, rowIdx).pos, square.map(row => row.map(_.get))))
            else if isEmptyPlane(square) then
              // An empty square
              None
            else
              // Invalid
              throw Exception("Partial plane encountered during parsing")
          }
        }

        PlaneGrid(arr)
      }

  override def parser: Parser[(PlaneGrid, List[Instruction])] =
    CommonParsers.pair(Parsing.rawGridData, Parsing.instructionList, CommonParsers.blankLine)

  override def solvePart1(input: (PlaneGrid, List[Instruction])): Long =
    GroveMapPt1(input(0)).execute(input(1)).score

  override def solvePart2(input: (PlaneGrid, List[Instruction])): Long = ???


@main def run(): Unit = Day22.run()
@main def test(): Unit = Day22.test()
@main def testParser(): Unit =
  Day22.testParser(Day22.parser)
@main def runParser(): Unit =
  Day22.runParser(Day22.parser)