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
  // The coordinate could be inverted (i.e. max - maintained)
  case Inverted
  // The coordinate could be swapped for the other coordinate
  case Swapped
  // The coordinate could be swapped and inverted (i.e. max - swapped)
  case SwappedInverted

  def applyToCoordinate(coord: Int, otherCoord: Int, maxThisCoord: Int, maxOtherCoord: Int): Int = this match
    case CoordTransition.ToZero => 0
    case CoordTransition.ToMax => maxThisCoord
    case CoordTransition.Maintained => coord
    case CoordTransition.Inverted => maxThisCoord - coord
    case CoordTransition.Swapped => otherCoord
    case CoordTransition.SwappedInverted => maxOtherCoord - otherCoord

// Describes how the player's position is mapped when moving from one plane to another
case class EdgeTransition(toPlane: Plane, x: CoordTransition, y: CoordTransition, newDir: Direction):
  protected def applyToPos(pos: Point2D): Point2D =
    (
      x.applyToCoordinate(pos.x, pos.y, toPlane.maxBound.x, toPlane.maxBound.y),
      y.applyToCoordinate(pos.y, pos.x, toPlane.maxBound.y, toPlane.maxBound.x)
    ).pos

  // Take one step over the edge...
  def apply(pos: PlayerPos): PlayerPos =
    PlayerPos(toPlane, applyToPos(pos.pos), newDir)


sealed trait GroveMap(val rawGrid: PlaneGrid):
  // The final grid, after any re-orientation etc. of planes
  val grid: PlaneGrid = rawGrid

  def transitionFor(fromPlane: Plane, dir: Direction): EdgeTransition

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
      nextPos = transitionFor(priorPos.plane, priorPos.dir)(posBeforeTransition)

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


// Map is a flat surface with wrapping
// No change to the input grid is required
class GroveMapPt1(override val grid: PlaneGrid) extends GroveMap(grid):
  // For part 1 we just need to consider wrapping transitions on a 2D plane
  override def transitionFor(fromPlane: Plane, dir: Direction): EdgeTransition =
    // Move along the direction, potentially wrapping
    var movedTo = (fromPlane.posInNet + dir.moveVec).floorMod(grid.gridMaxBound + (1, 1).pos)
    while grid(movedTo).isEmpty do
      movedTo = (movedTo + dir.moveVec).floorMod(grid.gridMaxBound + (1, 1).pos)

    val toPlane = grid(movedTo).get
    dir match
      case Direction.Left => EdgeTransition(toPlane, CoordTransition.ToMax, CoordTransition.Maintained, Direction.Left)
      case Direction.Right => EdgeTransition(toPlane, CoordTransition.ToZero, CoordTransition.Maintained, Direction.Right)
      case Direction.Up => EdgeTransition(toPlane, CoordTransition.Maintained, CoordTransition.ToMax, Direction.Up)
      case Direction.Down => EdgeTransition(toPlane, CoordTransition.Maintained, CoordTransition.ToZero, Direction.Down)


// Map is a cube, with the grid representing a cube net
class GroveMapPt2(rawGrid: PlaneGrid, isTest: Boolean) extends GroveMap(rawGrid):
  // We must choose a 'top' face and re-orient (rotate) other planes based on that
  override val grid: PlaneGrid = rawGrid

  private def planeFor(x: Int, y: Int) = grid((x, y).pos).get

  private case class FaceMapping(top: Plane, front: Plane, bottom: Plane, back: Plane, left: Plane, right: Plane)

  // Hardcoded transition mapping for the test input
  // Treating the top row's only plane as the 'top' of the cube
  private val testMapping: FaceMapping = FaceMapping(
    top = planeFor(2, 0),
    front = planeFor(2, 1),
    bottom = planeFor(2, 2),
    back = planeFor(0, 1),
    left = planeFor(1, 1),
    right = planeFor(3, 2)
  )

  // TODO: Hardcode or make generic
  private def realInputMapping: FaceMapping = ???

  private val faces: FaceMapping =
    if isTest then
      testMapping
    else
      realInputMapping

  // Transition mappings defined by case
  // TODO: Sadly specific to test mapping I think...
  private val testTransitions: Map[(Plane, Direction), EdgeTransition] = Map(
    // --- Rotation about x-axis counter-clockwise Top -> Front ---
    // Top -> Front
    (faces.top, Direction.Down) ->
      EdgeTransition(faces.front, CoordTransition.Maintained, CoordTransition.ToZero, Direction.Down),
    // Front -> Bottom
    (faces.front, Direction.Down) ->
      EdgeTransition(faces.bottom, CoordTransition.Maintained, CoordTransition.ToZero, Direction.Down),
    // Bottom -> Back
    (faces.bottom, Direction.Down) ->
      EdgeTransition(faces.back, CoordTransition.Inverted, CoordTransition.ToMax, Direction.Up),
    // Back -> Top
    (faces.back, Direction.Up) ->
      EdgeTransition(faces.top, CoordTransition.Inverted, CoordTransition.ToZero, Direction.Down),

    // --- Rotation about x-axis clockwise Top -> Back ---
    // Top -> Back
    (faces.top, Direction.Up) ->
      EdgeTransition(faces.back, CoordTransition.Inverted, CoordTransition.ToZero, Direction.Down),
    // Back -> Bottom
    (faces.back, Direction.Down) ->
      EdgeTransition(faces.bottom, CoordTransition.Inverted, CoordTransition.ToMax, Direction.Up),
    // Bottom -> Front
    (faces.bottom, Direction.Up) ->
      EdgeTransition(faces.front, CoordTransition.Maintained, CoordTransition.ToMax, Direction.Up),
    // Front -> Top
    (faces.front, Direction.Up) ->
      EdgeTransition(faces.top, CoordTransition.Maintained, CoordTransition.ToMax, Direction.Up),

    // --- Rotation about y-axis counter-clockwise Top -> Left ---
    // Top -> Left
    (faces.top, Direction.Left) ->
      EdgeTransition(faces.left, CoordTransition.Swapped, CoordTransition.ToZero, Direction.Down),
    // Left -> Bottom
    (faces.left, Direction.Down) ->
      EdgeTransition(faces.bottom, CoordTransition.ToZero, CoordTransition.SwappedInverted, Direction.Right),
    // Bottom -> Right
    (faces.bottom, Direction.Right) ->
      EdgeTransition(faces.right, CoordTransition.ToZero, CoordTransition.Maintained, Direction.Right),
    // Right -> Top
    (faces.right, Direction.Right) ->
      EdgeTransition(faces.top, CoordTransition.ToMax, CoordTransition.Inverted, Direction.Left),

    // --- Rotation about y-axis clockwise Top -> Right ---
    // Top -> Right
    (faces.top, Direction.Right) ->
      EdgeTransition(faces.right, CoordTransition.ToMax, CoordTransition.Inverted, Direction.Left),
    // Right -> Bottom
    (faces.right, Direction.Left) ->
      EdgeTransition(faces.bottom, CoordTransition.ToMax, CoordTransition.Maintained, Direction.Left),
    // Bottom -> Left
    (faces.bottom, Direction.Left) ->
      EdgeTransition(faces.left, CoordTransition.SwappedInverted, CoordTransition.ToMax, Direction.Up),
    // Left -> Top
    (faces.left, Direction.Up) ->
      EdgeTransition(faces.top, CoordTransition.ToZero, CoordTransition.Swapped, Direction.Right),

    // --- Rotation about z-axis counter-clockwise Front -> Right ---
    // Front -> Right
    (faces.front, Direction.Right) ->
      EdgeTransition(faces.right, CoordTransition.SwappedInverted, CoordTransition.ToZero, Direction.Down),
    // Right -> Back
    (faces.right, Direction.Down) ->
      EdgeTransition(faces.back, CoordTransition.ToZero, CoordTransition.SwappedInverted, Direction.Right),
    // Back -> Left
    (faces.back, Direction.Right) ->
      EdgeTransition(faces.left, CoordTransition.ToZero, CoordTransition.Maintained, Direction.Right),
    // Left -> Front
    (faces.left, Direction.Right) ->
      EdgeTransition(faces.front, CoordTransition.ToZero, CoordTransition.Maintained, Direction.Right),

    // --- Rotation about z-axis clockwise Front -> Left ---
    // Front -> Left
    (faces.front, Direction.Left) ->
      EdgeTransition(faces.left, CoordTransition.ToMax, CoordTransition.Maintained, Direction.Left),
    // Left -> Back
    (faces.left, Direction.Left) ->
      EdgeTransition(faces.back, CoordTransition.ToMax, CoordTransition.Maintained, Direction.Left),
    // Back -> Right
    (faces.back, Direction.Left) ->
      EdgeTransition(faces.right, CoordTransition.SwappedInverted, CoordTransition.ToMax, Direction.Up),
    // Right -> Front
    (faces.right, Direction.Up) ->
      EdgeTransition(faces.front, CoordTransition.ToMax, CoordTransition.SwappedInverted, Direction.Left),
  )

  override def transitionFor(fromPlane: Plane, dir: Direction): EdgeTransition =
    if isTest then
      testTransitions.getOrElse((fromPlane, dir), throw Exception("Invalid transition attempted"))
    else
      ???


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

  override def solvePart2(input: (PlaneGrid, List[Instruction])): Long =
    // TODO: TESTING
    GroveMapPt2(input(0), true).execute(input(1)).score


@main def run(): Unit = Day22.run()
@main def test(): Unit = Day22.test()
@main def testParser(): Unit =
  Day22.testParser(Day22.parser)
@main def runParser(): Unit =
  Day22.runParser(Day22.parser)