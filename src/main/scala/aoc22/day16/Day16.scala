package aoc22.day16

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser

case class ValveId(name: String)

case class Valve(id: ValveId, flowRate: Int, neighbours: Set[ValveId])

case class Network(definitions: List[Valve]):
  val valves: Map[ValveId, Valve] = Map.from(definitions.map(v => (v.id, v)))


object Parsing:
  def valveId: Parser[ValveId] =
    (Parser.charIn('A' to 'Z') ~ Parser.charIn('A' to 'Z'))
      .map((a, b) => ValveId(s"${a}${b}"))

  def valve: Parser[Valve] =
    for
      _ <- Parser.string("Valve ")
      id <- valveId
      _ <- Parser.string(" has flow rate=")
      flowRate <- CommonParsers.int
      _ <- Parser.string("; tunnels lead to valves ") | Parser.string("; tunnel leads to valve ")
      neighbours <- CommonParsers.separated(valveId, Parser.string(", "))
    yield
      Valve(id, flowRate, neighbours.toSet)

  def inputParser: Parser[Network] =
    CommonParsers.lineSeparated(valve).map(Network.apply)


object Day16 extends SolutionWithParser[Network, Int, Int]:
  override def dayNumber: Int = 16

  override def parser: Parser[Network] = Parsing.inputParser

  override def solvePart1(input: Network): Int = ???

  override def solvePart2(input: Network): Int = ???





@main def run(): Unit = Day16.run()
@main def test(): Unit = Day16.test()
@main def testParser(): Unit =
  Day16.testParser(Day16.parser)
@main def runParser(): Unit =
  Day16.runParser(Day16.parser)


