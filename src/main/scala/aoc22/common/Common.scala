package aoc22.common

import scala.io.Source
import cats.parse.Parser


extension [K, V] (m: Map[K, V])
  def putIfAbsent(k: K, v: V): Map[K, V] = if m.contains(k) then m else m.updated(k, v)


/**
 * Execute the function and time it in milliseconds
 *
 * @param f function to execute
 * @tparam A return type of the function
 * @return a pair of the result and the time taken
 */
def withTime[A](f: => A): (A, Long) =
  val start = System.nanoTime()
  val result = f
  val end = System.nanoTime()
  (result, (end - start) / 1_000_000)

def inputFilename(day: Int) = s"./data/input/day${day}.in"

def testFilename(day: Int, testSuffix: String = "") =
  val effectiveSuffix = if testSuffix == "" then testSuffix else s"-${testSuffix}"
  s"./data/test/day${day}${effectiveSuffix}.in"

trait Solution[I, O1, O2]:
  def dayNumber: Int

  def preprocess(rawInput: Source): I

  def solvePart1(input: I): O1

  def solvePart2(input: I): O2

  private def runAndPrint(source: Source, label: String = ""): Unit =
      val effectiveLabel = if label == "" then "" else s" ${label}"
      val input = preprocess(source)
      for (name, f) <- List((s"Part 1${effectiveLabel}", solvePart1), (s"Part 2${effectiveLabel}", solvePart2)) do
        println(s"Executing ${name}:")
        val (result, time) = withTime { f(input) }
        println(s"\tResult: ${result} (${time}ms)")

  private def runFromFile(filename: String, label: String = ""): Unit =
    runAndPrint(Source.fromFile(filename), label)

  final def run(): Unit =
    runFromFile(inputFilename(dayNumber))

  final def test(testSuffix: String = ""): Unit =
    val effectiveLabel = if testSuffix == "" then "[TEST]" else s"[TEST ${testSuffix}]"
    runFromFile(testFilename(dayNumber, testSuffix), effectiveLabel)


trait SolutionWithParser[I, O1, O2] extends Solution[I, O1, O2]:
  def parser: Parser[I]

  private def execParser[A](parser: Parser[A], source: Source): (String, A) =
    parser.parse(source.mkString).match {
      case Left(err) =>
        throw new Exception(s"Failed to parse due to error: ${err}")
      case Right(res) => res
    }

  override def preprocess(rawInput: Source): I = execParser(parser, rawInput)._2

  private def testParserOnSource[A](parser: Parser[A], source: Source): Unit =
    val (leftover, result) = execParser(parser, source)
    println("Parsed data:")
    println(result)
    println("!---\nLeftover:\n---")
    println(leftover.replace("\n","\\n"))
    println("!---")

  def testParserStr[A](parser: Parser[A], input: String): Unit =
    testParserOnSource(parser, Source.fromString(input))

  def testParser[A](parser: Parser[A], testSuffix: String = ""): Unit =
    testParserOnSource(parser, Source.fromFile(testFilename(dayNumber, testSuffix)))

  def runParser[A](parser: Parser[A]): Unit =
    testParserOnSource(parser, Source.fromFile(inputFilename(dayNumber)))