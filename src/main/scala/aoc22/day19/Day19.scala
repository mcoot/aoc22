package aoc22.day19

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Parser, Parser0}

import scala.util.control.Breaks._
import scala.annotation.targetName
import scala.collection.mutable
import scala.collection.mutable.Map as MutableMap


val TIME_MAX = 24


enum ResourceKind:
  case Ore
  case Clay
  case Obsidian
  case Geode


case class Resources(ore: Int, clay: Int, obsidian: Int, geode: Int):
  def hasMoreGeodesThan(other: Resources): Boolean = geode > other.geode


object Resources:
  val initial: Resources = Resources(0, 0, 0, 0)


sealed trait RobotTemplate(val gathers: ResourceKind, val gathersPerMinute: Int):
  def canBuild(resources: Resources): Boolean

  def build(resources: Resources): Resources

case class OreRobot(oreCost: Int) extends RobotTemplate(ResourceKind.Ore, 1):
  override def canBuild(resources: Resources): Boolean = oreCost <= resources.ore
  override def build(resources: Resources): Resources =
    resources.copy(ore = resources.ore - oreCost)

case class ClayRobot(oreCost: Int) extends RobotTemplate(ResourceKind.Clay, 1):
  override def canBuild(resources: Resources): Boolean = oreCost <= resources.ore
  override def build(resources: Resources): Resources =
    resources.copy(ore = resources.ore - oreCost)

case class ObsidianRobot(oreCost: Int, clayCost: Int) extends RobotTemplate(ResourceKind.Obsidian, 1):
  override def canBuild(resources: Resources): Boolean = oreCost <= resources.ore && clayCost <= resources.clay
  override def build(resources: Resources): Resources =
    resources.copy(ore = resources.ore - oreCost, clay = resources.clay - clayCost)

case class GeodeRobot(oreCost: Int, obsidianCost: Int) extends RobotTemplate(ResourceKind.Geode, 1):
  override def canBuild(resources: Resources): Boolean =
    oreCost <= resources.ore && obsidianCost <= resources.obsidian
  override def build(resources: Resources): Resources =
    resources.copy(ore = resources.ore - oreCost, obsidian = resources.obsidian - obsidianCost)


case class RobotTemplates(ore: OreRobot,
                          clay: ClayRobot,
                          obsidian: ObsidianRobot,
                          geode: GeodeRobot):
  val kindToTemplate: Map[ResourceKind, RobotTemplate] =
    List(
      (ResourceKind.Ore, ore),
      (ResourceKind.Clay, clay),
      (ResourceKind.Obsidian, obsidian),
      (ResourceKind.Geode, geode)
    ).toMap

  def build(kind: ResourceKind, resources: Resources): Resources =
    kindToTemplate(kind).build(resources)

  def gather(kind: ResourceKind): Int = kindToTemplate(kind).gathersPerMinute


case class Blueprint(id: Int, templates: RobotTemplates)


case class Robots(templates: RobotTemplates, ore: Int, clay: Int, obsidian: Int, geode: Int):
  def completeProduction(kind: ResourceKind): Robots = kind match
    case ResourceKind.Ore => copy(ore = ore + 1)
    case ResourceKind.Clay => copy(clay = clay + 1)
    case ResourceKind.Obsidian => copy(obsidian = obsidian + 1)
    case ResourceKind.Geode => copy(geode = geode + 1)

  // Gather the resources for a step
  def gather(prev: Resources): Resources =
    Resources(
      ore = prev.ore + ore * templates.gather(ResourceKind.Ore),
      clay = prev.clay + clay * templates.gather(ResourceKind.Clay),
      obsidian = prev.obsidian + obsidian * templates.gather(ResourceKind.Obsidian),
      geode = prev.geode + geode * templates.gather(ResourceKind.Geode),
    )

  def buildableRobots(resources: Resources): List[ResourceKind] =
    templates.kindToTemplate.collect {
      case (k, t) if t.canBuild(resources) =>
        k
    }.toList

object Robots:
  def initial(templates: RobotTemplates): Robots = Robots(templates, 1, 0, 0, 0)


case class State(time: Int, resources: Resources, robots: Robots):
  def isPastTimeMax: Boolean = time >= TIME_MAX

  // Based on the current time, could we conceivably reach the given number of geodes?
  def impossibleToGetToGeodes(proposedGeodes: Int): Boolean =
    val timeLeft = TIME_MAX + 1 - time
    // Hypothetically, if we go
    ???

object State:
  def initial(blueprint: Blueprint): State = State(0, Resources.initial, Robots.initial(blueprint.templates))

class Solver(val blueprint: Blueprint):
  private def step(state: State, robotToBuild: Option[ResourceKind]): State =
    // Put a resource into production if we are going to
    // Collect resources
    // Add any robots we built
    // Step time
    if robotToBuild.isDefined then
      val newResources = state.robots.gather(blueprint.templates.build(robotToBuild.get, state.resources))
      val newRobots = state.robots.completeProduction(robotToBuild.get)
      State(state.time + 1, newResources, newRobots)
    else
      State(state.time + 1, state.robots.gather(state.resources), state.robots)

  def getMaxGeodes: Int =
    val statesToVisit = mutable.Queue.from(List(State.initial(blueprint)))
    var bestTerminalState: Option[State] = None
    var lastTime = -1
    var mostGeodesAtCurrentTime = 0
    var totalIters = 0

    while statesToVisit.nonEmpty do
      totalIters += 1
      if totalIters % 10000000 == 0 then
        println(s"Iteration ${totalIters}")

      val state = statesToVisit.dequeue()

      breakable {
        if state.time > lastTime then
          lastTime = state.time
          println(s"== time=${state.time} ==")
        //      if state.resources.geode > 0 then
        //        println(s"State t=${state.time} | r=${state.resources} | ${state.robots.geode}")

        // If we're past the end-time, this is a terminal state so check how many geodes we got
        if state.isPastTimeMax then
          // If this is the most we've found, make this our best
          val isBest = bestTerminalState.isEmpty || state.resources.hasMoreGeodesThan(bestTerminalState.get.resources)
          if isBest then
          //          println(s"New best at time ${state.time} with ${state.resources}")
            bestTerminalState = Some(state)
          break()

        if state.resources.geode > mostGeodesAtCurrentTime then
          mostGeodesAtCurrentTime = state.resources.geode
        else if state.resources.geode < mostGeodesAtCurrentTime then
          // Behind the 8-ball, give up lol
          break()

        // Robots we could build
        val couldBuild = state.robots.buildableRobots(state.resources)
        val nextStates = if couldBuild.contains(ResourceKind.Geode) then
        // If we could build a geode robot, we should always do that and can avoid considering the others
          List(step(state, Some(ResourceKind.Geode)))
        else
        // Consider doing nothing and just building resources
          step(state, None) :: couldBuild.map(k => step(state, Some(k)))
        nextStates.filter(_.resources.geode >= mostGeodesAtCurrentTime).foreach(statesToVisit.enqueue)
      }

    bestTerminalState.getOrElse(throw Exception("Failed to find any terminal states")).resources.geode

  def qualityLevel: Int = blueprint.id * getMaxGeodes


object Day19 extends SolutionWithParser[List[Blueprint], Int, Int]:
  override def dayNumber: Int = 19

  private object Parsing:
    def lineEndingParser: Parser0[Unit] =
      (CommonParsers.spaceOrNewline ~ CommonParsers.spaces.?).map(_ => ())

    def blueprintIdParser: Parser[Int] =
      for
        _ <- Parser.string("Blueprint ")
        id <- CommonParsers.int
        _ <- Parser.char(':')
      yield
        id

    def oreParser: Parser[Int] =
      CommonParsers.int <* Parser.string(" ore")

    def clayParser: Parser[Int] =
      CommonParsers.int <* Parser.string(" clay")

    def obsidianParser: Parser[Int] =
      CommonParsers.int <* Parser.string(" obsidian")

    def robotTemplatesParser: Parser[RobotTemplates] =
      for
        _ <- Parser.string("Each ore robot costs ")
        oreRobotCost <- oreParser
        _ <- Parser.string(".") ~ lineEndingParser
        _ <- Parser.string("Each clay robot costs ")
        clayRobotCost <- oreParser
        _ <- Parser.string(".") ~ lineEndingParser
        _ <- Parser.string("Each obsidian robot costs ")
        obsidianRobotOreCost <- oreParser
        _ <- Parser.string(" and ")
        obsidianRobotClayCost <- clayParser
        _ <- Parser.string(".") ~ lineEndingParser
        _ <- Parser.string("Each geode robot costs ")
        geodeRobotOreCost <- oreParser
        _ <- Parser.string(" and ")
        geodeRobotObsidianCost <- obsidianParser
        _ <- Parser.string(".")
      yield
        RobotTemplates(OreRobot(oreRobotCost),
          ClayRobot(clayRobotCost),
          ObsidianRobot(obsidianRobotOreCost, obsidianRobotClayCost),
          GeodeRobot(geodeRobotOreCost, geodeRobotObsidianCost))

    def blueprintParser: Parser[Blueprint] =
      for
        id <- blueprintIdParser <* lineEndingParser
        costs <- robotTemplatesParser
      yield
        Blueprint(id, costs)

  override def parser: Parser[List[Blueprint]] =
    CommonParsers.lineSeparated(Parsing.blueprintParser)

  override def solvePart1(input: List[Blueprint]): Int =
    val results = input.map { b =>
      val r = Solver(b).qualityLevel
      println(s"Result for ${b.id}: ${r}")
      r
    }
    results.sum

  override def solvePart2(input: List[Blueprint]): Int =
    ???


@main def run(): Unit = Day19.run()
@main def test(): Unit = Day19.test()
@main def testParser(): Unit =
  Day19.testParser(Day19.parser)
@main def runParser(): Unit =
  Day19.runParser(Day19.parser)