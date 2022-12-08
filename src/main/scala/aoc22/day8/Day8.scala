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

  override def solvePart1(input: TreeGrid): Int = ???

  override def solvePart2(input: TreeGrid): Int = ???


@main def run(): Unit = Day8.run()
@main def test(): Unit = Day8.test()
@main def testParser(): Unit =
  Day8.testParser(Day8.parser)
@main def runParser(): Unit =
  Day8.runParser(Day8.parser)