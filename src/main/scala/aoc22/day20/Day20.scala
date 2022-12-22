package aoc22.day20

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


case class Ring(elements: List[(Long, Int)]):
  def move(origIdx: Int): Ring =
    val (elem, curIdx): ((Long, Int), Int) = elements
      .zipWithIndex
      .find(_(0)(1) == origIdx)
      .getOrElse(throw Exception("missing!"))

    val withMarked = elements.patch(curIdx, Nil, 1)

    val insertionPoint = Math.floorMod(curIdx + elem(0), withMarked.length).toInt

    val withDuplicate = withMarked.take(insertionPoint) ++ List(elem) ++ withMarked.drop(insertionPoint)

    val finalList = withDuplicate.filterNot(_(1) == -1)

    Ring(finalList)


  def mix: Ring =
    elements.indices.foldLeft(this) { case (r, idx) =>
//      println(r.elements.map(_(0)).mkString(", "))
      r.move(idx)
    }

  def coords: (Long, Long, Long) =
    val zeroIdx = elements.indexWhere(_(0) == 0)
    val x = elements((zeroIdx + 1000) % elements.length)(0)
    val y = elements((zeroIdx + 2000) % elements.length)(0)
    val z = elements((zeroIdx + 3000) % elements.length)(0)
    (x, y, z)


object Day20 extends SolutionWithParser[List[Long], Long, Long]:
  override def dayNumber: Int = 20

  override def parser: Parser[List[Long]] = CommonParsers.lineSeparated(CommonParsers.int.map(_.toLong))

  override def solvePart1(input: List[Long]): Long =
    val res = Ring(input.zipWithIndex).mix

//    println(s"Final ${res.elements.map(_(0)).mkString(", ")}")
    res.coords.productIterator.asInstanceOf[Iterator[Long]].sum

  override def solvePart2(input: List[Long]): Long = ???


@main def run(): Unit = Day20.run()
@main def test(): Unit = Day20.test()
@main def testParser(): Unit =
  Day20.testParser(Day20.parser)
@main def runParser(): Unit =
  Day20.runParser(Day20.parser)

