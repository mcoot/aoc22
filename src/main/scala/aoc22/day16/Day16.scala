package aoc22.day16

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


// 0-indexed!
val TIME_MAX = 29

case class ValveId(name: String)


object ValveId:
  def initial: ValveId = ValveId("AA")


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

  def distance(from: ValveId, to: ValveId): Int = paths((from, to)).size


enum Action:
  // Open the valve you're currently at
  case OpenValve
  // Move to another valve (may take multiple minutes)
  case MoveTo(id: ValveId)
  // End acting and just wait out the remaining time
  case End


// For useful debugging in case we try to take actions that aren't possible
enum ActionError:
  case ValveAlreadyOpen
  case NotEnoughTimeToComplete


case class NetworkState(network: Network, openValves: Set[ValveId], currentlyAt: ValveId):
  def currentValve: Valve = network.vertices(currentlyAt)

  def isOpen(v: ValveId): Boolean = openValves.contains(v)

  def isCurrentValveOpen: Boolean = isOpen(currentlyAt)

  def totalFlowPerMinute: Int =
    openValves.map(network.vertices(_).flowRate).sum

  def timeCostOfAction(action: Action, currentTime: Int): Int = action match
    // Opening a valve always takes one minute
    case Action.OpenValve => 1
    // Moving to a valve takes one minute per valve you need to traverse, including the end
    case Action.MoveTo(id) => network.distance(currentlyAt, id)
    // Ending your actions consumes the rest of the time available
    case Action.End => TIME_MAX - currentTime

  def flowRateIncreaseOfAction(action: Action): Int = action match
    case Action.OpenValve if !isCurrentValveOpen => currentValve.flowRate
    case _ => 0


object NetworkState:
  def initial(network: Network): NetworkState = NetworkState(network, Set.empty, ValveId.initial)


case class NetworkStateAtTime(state: NetworkState, time: Int):
  // Figure out how an action would fail, if it would
  def actionFailureMode(action: Action): Option[ActionError] =
    if time + state.timeCostOfAction(action, time) > TIME_MAX then
      Some(ActionError.NotEnoughTimeToComplete)
    else
      action match
        case Action.OpenValve if state.isCurrentValveOpen => Some(ActionError.ValveAlreadyOpen)
        case _ => None

  // Apply the action, without considering failure
  private def applyAction(action: Action): NetworkStateAtTime =
    action match
      // Opening a valve adds to the open set but doesn't move you
      case Action.OpenValve =>
        NetworkStateAtTime(
          state.copy(openValves = state.openValves + state.currentlyAt),
          time + state.timeCostOfAction(action, time)
        )
      // Moving moves you to the end location, and increases time potentially by more than one
      case Action.MoveTo(id) =>
        NetworkStateAtTime(
          state.copy(currentlyAt = id),
          time + state.timeCostOfAction(action, time)
        )
      // Giving up increments time but does nothing else
      case Action.End =>
        NetworkStateAtTime(state, time + state.timeCostOfAction(action, time))


  // Generate the new state if this action is performed
  // May fail because the action is not possible
  def act(action: Action): Either[ActionError, NetworkStateAtTime] =
    val failureMode = actionFailureMode(action)
    if failureMode.isDefined then
      Left(failureMode.get)
    else
      Right(applyAction(action))


object NetworkStateAtTime:
  def initial(network: Network): NetworkStateAtTime = NetworkStateAtTime(NetworkState.initial(network), 0)


case class NetworkSolution(network: Network, actions: List[Action]):
  // The list of states the network passes through when the actions are applied
  val states: List[NetworkStateAtTime] =
    val ss = actions
      .foldLeft(List(NetworkStateAtTime.initial(network))) { (s, a) =>
        s.head.act(a).getOrElse {
          throw Exception("Invalid action!")
        } :: s
      }
    ss.reverse

  // Full listing of actions taken and the state at time
  // With interpolations while moving or after ending
  val interpolatedStates: List[(NetworkStateAtTime, Option[Action])] =
  states
    // Zip with the action taken from this state
    .zip(actions.appended(Action.End))
    // Zip with the states again shifted by one so we get (before, after, action)
    // For the last state there is no after
    .zip(states.drop(1).map(Some(_)).appended(None))
    .flatMap {
      // For the final state, we have no 'after' so nothing to interpolate regardless
      // The 'action' here is our fake End action, drop it
      case ((startState, _), None) => List((startState, None))
      case ((startState, action), Some(endState)) => action match
        // Opening a valve only takes one minute so no interpolation needed
        case Action.OpenValve =>
          List((startState, Some(action)))
        // Moving takes as many interpolations as there are steps in the path between the valves
        case Action.MoveTo(id) =>
          val steps = network.paths((startState.state.currentlyAt, id))

          // Include the state we're at
          (startState.state.currentlyAt :: steps)
            // Zip with the index, i.e. time difference from the initial state
            .zipWithIndex
            // Zip with the 'end state' of each interpolated step
            .zip(steps)
            // Get tuples of (fromValve, toValve, timeAtValve) for each interpolated step
            .map { case ((fromValve, idx), toValve) =>
              val interpolatedTime = startState.time + idx
              val interpolatedState = startState.state.copy(currentlyAt = fromValve)
              (startState.copy(state = interpolatedState, time = interpolatedTime), Some(Action.MoveTo(toValve)))
            }
        // Ending should just interpolate the same state through to the after point (i.e. the end time)
        case Action.End =>
          val timeRange = startState.time until endState.time
          timeRange.map(t => (startState.copy(time = t), Some(Action.End))).toList
    }

  def totalPressureReleased: Int =
    interpolatedStates.map { case (s, _) => s.state.totalFlowPerMinute }.sum


class Solver(val network: Network):
  // Solve the network to find the best sequence of actions that can be taken
  // maximising flow integrated over t=0 to t=29
  def solve: NetworkSolution = ???


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
    CommonParsers.lineSeparated(valve).map(vs => Network(vs))


object Testing:
  def testSolutionForPart1(network: Network): NetworkSolution =
    NetworkSolution(
      network,
      List(
        Action.MoveTo(ValveId("DD")),
        Action.OpenValve,
        Action.MoveTo(ValveId("BB")),
        Action.OpenValve,
        Action.MoveTo(ValveId("JJ")),
        Action.OpenValve,
        Action.MoveTo(ValveId("HH")),
        Action.OpenValve,
        Action.MoveTo(ValveId("EE")),
        Action.OpenValve,
        Action.MoveTo(ValveId("CC")),
        Action.OpenValve,
        Action.End,
      )
    )

  def genTimeString(time: Int): String = s"== Minute ${time + 1} =="

  def genNetworkStateString(s: NetworkState): String =
    val valveStr = s.openValves.size match
      case 0 => "No valves are open."
      case 1 => s"Valve ${s.openValves.map(_.name).toList.head} is open"
      case _ =>
        val sortedNames = s.openValves.map(_.name).toList.sorted
        val listStr = s"${sortedNames.dropRight(1).mkString(", ")} and ${sortedNames.last}"
        s"Valves ${listStr} are open"
    val flowStr =
      if s.openValves.nonEmpty then
        s", releasing ${s.totalFlowPerMinute} pressure."
      else
        ""
    s"${valveStr}${flowStr}"

  def genActionString(a: Action, currentValve: ValveId): String =
    a match
      case Action.OpenValve => s"You open valve ${currentValve.name}."
      case Action.MoveTo(id) => s"You move to valve ${id.name}."
      case Action.End => ""

  def printStateLog(sol: NetworkSolution): Unit =
    // Add an extra Action.End to the action list since we don't take an action at t=TIME_MAX
    for (s, a) <- sol.interpolatedStates do
      println(genTimeString(s.time))
      println(genNetworkStateString(s.state))
      val aStr = a.map(genActionString(_, s.state.currentlyAt)).getOrElse("")
      if aStr.nonEmpty then
        println(genActionString(a.get, s.state.currentlyAt))
      println()

    println(s"Total pressure released: ${sol.totalPressureReleased}")

  def verifyTestSolution(network: Network): Unit = printStateLog(testSolutionForPart1(network))



object Day16 extends SolutionWithParser[Network, Int, Int]:
  override def dayNumber: Int = 16

  override def parser: Parser[Network] = Parsing.inputParser

  override def solvePart1(input: Network): Int =
    // TESTING
    Testing.verifyTestSolution(input)

    Solver(input)
      .solve
      .totalPressureReleased

  override def solvePart2(input: Network): Int =
    ???





@main def run(): Unit = Day16.run()
@main def test(): Unit = Day16.test()
@main def testParser(): Unit =
  Day16.testParser(Day16.parser)
@main def runParser(): Unit =
  Day16.runParser(Day16.parser)