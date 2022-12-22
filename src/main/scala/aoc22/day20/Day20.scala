package aoc22.day20

import scala.collection.mutable.ArrayBuffer
import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


case class Ring(elements: List[(Long, Int)]):
  def move(origIdx: Int): Ring =
    val (elem, curIdx): ((Long, Int), Int) = elements
      .zipWithIndex
      .find(_(0)(1) == origIdx)
      .getOrElse(throw Exception("missing!"))

    val withoutElem = elements.patch(curIdx, Nil, 1)

    val insertionPoint = Math.floorMod(curIdx + elem(0), withoutElem.length).toInt

    val finalList = withoutElem.take(insertionPoint) ++ List(elem) ++ withoutElem.drop(insertionPoint)

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
    println((x, y, z))
    (x, y, z)


// Attempted better reimplementation with mutability
// It works for the test case but not the real input and I have NFI why
class RingBuffer(val initialElements: List[Long]):

  var elements: ArrayBuffer[Long] = ArrayBuffer.from(initialElements)

  def mix(): Unit =
    for v <- initialElements do
      // Find where that value is now
      val curIdx = elements.indexOf(v)
      // Take what we are moving out
      elements.remove(curIdx)
      // Figure out where to insert it back
      val insertionPoint = Math.floorMod(curIdx + v, elements.length).toInt
      // Insert it at that spot
      elements.insert(insertionPoint, v)


  def coords: (Long, Long, Long) =
    val zeroIdx = elements.indexWhere(_ == 0)
    println(elements.slice(zeroIdx, zeroIdx + 3))
    val x = elements((zeroIdx + 1000) % elements.length)
    val y = elements((zeroIdx + 2000) % elements.length)
    val z = elements((zeroIdx + 3000) % elements.length)
    println((x, y, z))
    (x, y, z)



object Day20 extends SolutionWithParser[List[Long], Long, Long]:
  override def dayNumber: Int = 20

  override def parser: Parser[List[Long]] = CommonParsers.lineSeparated(CommonParsers.int.map(_.toLong))

  override def solvePart1(input: List[Long]): Long =
    val res = Ring(input.zipWithIndex).mix
    res.coords.productIterator.asInstanceOf[Iterator[Long]].sum

  override def solvePart2(input: List[Long]): Long =
    var res = Ring(input.map(_ * 811589153).zipWithIndex)

    for _ <- 0 until 10 do
      res = res.mix

    res.coords.productIterator.asInstanceOf[Iterator[Long]].sum


@main def run(): Unit = Day20.run()
@main def test(): Unit = Day20.test()
@main def testParser(): Unit =
  Day20.testParser(Day20.parser)
@main def runParser(): Unit =
  Day20.runParser(Day20.parser)

