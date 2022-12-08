package aoc22.day8

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Parser, Rfc5234}

import scala.collection.mutable


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

  def zipWithIndices: List[(Int, (Int, Int))] =
    arr.zipWithIndex.flatMap { case (row, rowIdx) =>
      row.zipWithIndex.map { case (tree, colIdx) =>
        (tree, (rowIdx, colIdx))
      }
    }.toList

  def isVisible(rowIdx: Int, colIdx: Int): Boolean =
    val thisTree = this(rowIdx, colIdx)
    val row = getRow(rowIdx)
    val col = getCol(colIdx)
    val conds = List(
      // Row before this
      row.take(colIdx).forall(_ < thisTree),
      // Row after this
      row.drop(colIdx + 1).forall(_ < thisTree),
      // Col before this
      col.take(rowIdx).forall(_ < thisTree),
      // Col after this
      col.drop(rowIdx + 1).forall(_ < thisTree),
    )
    conds.exists(identity)


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
    input.zipWithIndices.count { case (_, (row, col)) => input.isVisible(row, col) }

  override def solvePart2(input: TreeGrid): Int = ???


@main def run(): Unit = Day8.run()
@main def test(): Unit = Day8.test()
@main def testParser(): Unit =
  Day8.testParser(Day8.parser)
@main def runParser(): Unit =
  Day8.runParser(Day8.parser)