package aoc22.day22

import aoc22.common.{CommonParsers, Point2D, pos, SolutionWithParser}
import cats.parse.Parser


enum TurnDirection:
  case L
  case R


enum Direction:
  case Left
  case Right
  case Up
  case Down

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


// Raw grid data as a a 2d grid of square segments
case class RawGridData(arr: Array[Array[Option[Plane]]])


// Describes where the player is by the plane they're on, position in the plane and cur direction
case class PlayerPos(plane: Plane, pos: Point2D, dir: Direction):
  def score: Long =
    val netPos = plane.netPositionFor(pos)
    (netPos.y + 1) * 1000 + (netPos.x + 1) * 4 + dir.facingVal


// When moving across planes, a coordinate may change in one of several ways\
enum EdgeCoordTransition:
  // The coordinate could wrap to 0
  case ToZero
  // The coordinate could wrap to the max value for that plane
  case ToMax
  // The coordinate could remain unchanged
  case Maintained
  // The coordinate could be swapped for the other coordinate
  case Swapped

  def applyToCoordinate(coord: Int, otherCoord: Int, max: Int): Int = this match
    case EdgeCoordTransition.ToZero => 0
    case EdgeCoordTransition.ToMax => max
    case EdgeCoordTransition.Maintained => coord
    case EdgeCoordTransition.Swapped => otherCoord

// Describes how the player's position is mapped when moving from one plane to another
// Theory: the direction the player will be facing can be inferred from their existing direction
// and the position translation
// TODO: I NEED A CUBE
case class EdgeTransition(x: EdgeCoordTransition, y: EdgeCoordTransition):
  def applyToPos(pos: Point2D, maxBound: Point2D): Point2D =
    (x.applyToCoordinate(pos.x, pos.y, maxBound.x), y.applyToCoordinate(pos.y, pos.x, maxBound.y)).pos

  def applyToDirection(dir: Direction): Direction = (x, y) match
    case (EdgeCoordTransition.Maintained, EdgeCoordTransition.ToZero) => dir
    case _ => throw Exception("No direction mapping for edge transition")


// Maps the player position from a given plane id to another
case class EdgeMapping(fromPlane: Int, toPlane: Int, transition: EdgeTransition):
  def apply(p: PlayerPos): PlayerPos = ???


// A set of planes with information about how to map between them
case class GroveMap(planeList: List[Plane], edgeMappings: List[EdgeMapping]):
  val planes: Map[Int, Plane] = planeList.map(p => (p.id, p)).toMap


object Day22 extends SolutionWithParser[(RawGridData, List[Instruction]), Long, Long]:
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

    def rawGridData: Parser[RawGridData] =
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

        RawGridData(arr)
      }

  override def parser: Parser[(RawGridData, List[Instruction])] =
    CommonParsers.pair(Parsing.rawGridData, Parsing.instructionList, CommonParsers.blankLine)

  override def solvePart1(input: (RawGridData, List[Instruction])): Long = ???

  override def solvePart2(input: (RawGridData, List[Instruction])): Long = ???


@main def run(): Unit = Day22.run()
@main def test(): Unit = Day22.test()
@main def testParser(): Unit =
  Day22.testParser(Day22.parser)
@main def runParser(): Unit =
  Day22.runParser(Day22.parser)