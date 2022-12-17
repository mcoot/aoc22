package aoc22.day16

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


val TIME_MAX = 30

case class ValveId(name: String)

case class Valve(id: ValveId, flowRate: Int, neighbours: Set[ValveId])

case class Network(definitions: List[Valve]):
  val vertices: Map[ValveId, Valve] = Map.from(definitions.map(v => (v.id, v)))
  val edges: Set[(ValveId, ValveId)] = definitions.flatMap(v => v.neighbours.map((v.id, _))).toSet

  // Pre-calculate paths from every valve to every other
  // This is always possible as in the input there are always reverse tunnels for every given tunnel
  val paths: Map[(ValveId, ValveId), List[ValveId]] = buildAllPairsShortestPaths

  // Calculating all-pairs shortest paths via Floyd-Warshall
  private def buildAllPairsShortestPaths: Map[(ValveId, ValveId), List[ValveId]] =
    // Map array indices back to the ValveId
    val indices: Map[ValveId, Int] = Map.from(definitions.map(_.id).zipWithIndex)
    // No initial paths
    val paths: Array[Array[Option[List[ValveId]]]] = Array.fill(indices.size, indices.size)(None)

    def curDist(from: ValveId, to: ValveId): Int =
      // Not using Int.MaxValue because of overflow, but confident we will never have 100k length paths
      // based on the size of the input graph
      paths(indices(from))(indices(to)).map(_.size).getOrElse(100000)

    // Distance to self is 0
    for v <- vertices.keys do
      paths(indices(v))(indices(v)) = Some(List())

    // Direct edges have distance 1
    for (u, v) <- edges do
      paths(indices(u))(indices(v)) = Some(List(v))

    // Apply Floyd-Warshall to calculate the shortest distances
    for
      k <- vertices.keys
      i <- vertices.keys
      j <- vertices.keys
    do
      if curDist(i, j) > curDist(i, k) + curDist(k, j) then
        // For the latter path to ever be better we must have already found those routes
        // I.e. they can't be None
        paths(indices(i))(indices(j)) = Some(paths(indices(i))(indices(k)).get ++ paths(indices(k))(indices(j)).get)

    Map.from(
      for
        i <- vertices.keys
        j <- vertices.keys
      yield
        // Confident here we do in fact have paths, graph is connected
        ((i, j), paths(indices(i))(indices(j)).get)
    )

  def apply(id: ValveId): Valve = vertices(id)

case class NetworkState(network: Network, openValves: Set[ValveId], currentlyAt: ValveId):
  def totalFlowPerMinute: Int =
    openValves.map(network(_).flowRate).sum

  def timeToMoveTo(valveId: ValveId): Int =
    ???


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
    CommonParsers.lineSeparated(valve).map(Network(_))


object Day16 extends SolutionWithParser[Network, Int, Int]:
  override def dayNumber: Int = 16

  override def parser: Parser[Network] = Parsing.inputParser

  override def solvePart1(input: Network): Int =
    ???

  override def solvePart2(input: Network): Int =
    ???





@main def run(): Unit = Day16.run()
@main def test(): Unit = Day16.test()
@main def testParser(): Unit =
  Day16.testParser(Day16.parser)
@main def runParser(): Unit =
  Day16.runParser(Day16.parser)


