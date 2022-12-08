package aoc22.day8

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Parser, Rfc5234}

import scala.collection.mutable

case class Surroundings(left: List[Int], right: List[Int], up: List[Int], down: List[Int]):
  def toList: List[List[Int]] = List(left, right, up, down)


case class TreeGrid(arr: Array[Array[Int]]):
  override def toString: String =
    val sb = mutable.StringBuilder()
    for row <- arr do
      for tree <- row do
        sb.append(tree)
      sb.append('\n')
    sb.mkString

  def getRow(rowIdx: Int): List[Int] =
    arr(rowIdx).toList

  def getCol(colIdx: Int): List[Int] =
    arr.map(_(colIdx)).toList

  def surroundings(rowIdx: Int, colIdx: Int): Surroundings =
    val row = getRow(rowIdx)
    val col = getCol(colIdx)
    Surroundings(
      // Look left
      row.take(colIdx).reverse,
      // Look right
      row.drop(colIdx + 1),
      // Look up
      col.take(rowIdx).reverse,
      // Look down
      col.drop(rowIdx + 1)
    )

  def zipWithIndices: List[(Int, (Int, Int))] =
    arr.zipWithIndex.flatMap { case (row, rowIdx) =>
      row.zipWithIndex.map { case (tree, colIdx) =>
        (tree, (rowIdx, colIdx))
      }
    }.toList

  def isVisible(rowIdx: Int, colIdx: Int): Boolean =
    surroundings(rowIdx, colIdx)
      .toList
      .map(_.forall(_ < this(rowIdx, colIdx)))
      .exists(identity)

  def viewingDistances(rowIdx: Int, colIdx: Int): List[Int] =
    surroundings(rowIdx, colIdx)
      .toList
      .map(dir => dir
        .zipWithIndex
        .find(_(0) >= this(rowIdx, colIdx))
        .map(_(1) + 1)
        .getOrElse(dir.size)
      )

  def scenicScore(rowIdx: Int, colIdx: Int): Int =
    viewingDistances(rowIdx, colIdx).product

  def apply(row: Int, col: Int): Int = arr(row)(col)


object Parsing:
  def treeParser: Parser[Int] = Rfc5234.digit.map(_.toInt - '0'.toInt)

  def rowParser: Parser[List[Int]] = treeParser.rep(1).map(_.toList)

  def finalParser: Parser[TreeGrid] = for
    rows <- CommonParsers.lineSeparated(rowParser)
  yield
    val arr: Array[Array[Int]] = Array.ofDim(rows.size, rows.head.size)
    rows.zipWithIndex.foreach { case (row, rowIdx) =>
      row.zipWithIndex.foreach { case (digit, colIdx) =>
        arr(rowIdx)(colIdx) = digit
      }
    }
    TreeGrid(arr)


object Day8 extends SolutionWithParser[TreeGrid, Int, Int]:
  override def dayNumber: Int = 8

  override def parser: Parser[TreeGrid] = Parsing.finalParser

  override def solvePart1(input: TreeGrid): Int =
    input
      .zipWithIndices
      .count { case (_, (row, col)) => input.isVisible(row, col) }

  override def solvePart2(input: TreeGrid): Int =
    input
      .zipWithIndices
      .map { case (_, (row, col)) => input.scenicScore(row, col) }
      .max


@main def run(): Unit = Day8.run()
@main def test(): Unit = Day8.test()
@main def testParser(): Unit =
  Day8.testParser(Day8.parser)
@main def runParser(): Unit =
  Day8.runParser(Day8.parser)