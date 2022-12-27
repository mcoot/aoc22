package aoc22.day19

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Parser, Parser0}

import scala.annotation.targetName


val TIME_MAX = 24

sealed trait Resource[A <: Resource[A]](val amount: Int) extends Ordered[A]:
  override def compare(that: A): Int = amount compare that.amount

  @targetName("add")
  def +(other: A): A

  @targetName("subtract")
  def -(other: A): A

case class Ore(override val amount: Int) extends Resource[Ore](amount):
  @targetName("add")
  override def +(other: Ore): Ore = Ore(amount - other.amount)
  @targetName("subtract")
  override def -(other: Ore): Ore = Ore(amount - other.amount)

case class Clay(override val amount: Int) extends Resource[Clay](amount):
  @targetName("add")
  override def +(other: Clay): Clay = Clay(amount - other.amount)

  @targetName("subtract")
  override def -(other: Clay): Clay = Clay(amount - other.amount)

case class Obsidian(override val amount: Int) extends Resource[Obsidian](amount):
  @targetName("add")
  override def +(other: Obsidian): Obsidian = Obsidian(amount - other.amount)

  @targetName("subtract")
  override def -(other: Obsidian): Obsidian = Obsidian(amount - other.amount)

case class Geode(override val amount: Int) extends Resource[Geode](amount):
  @targetName("add")
  override def +(other: Geode): Geode = Geode(amount - other.amount)

  @targetName("subtract")
  override def -(other: Geode): Geode = Geode(amount - other.amount)


// Extension to allow easier construction of resource units
extension (i: Int)
  def ore: Ore = Ore(i)
  def clay: Clay = Clay(i)
  def obsidian: Obsidian = Obsidian(i)
  def geode: Geode = Geode(i)


case class Resources(ore: Ore, clay: Clay, obsidian: Obsidian, geode: Geode)

object Resources:
  val initial: Resources = Resources(0.ore, 0.clay, 0.obsidian, 0.geode)


sealed trait RobotTemplate[A <: Resource[A]](val producesPerMinute: Resource[A])

case class OreRobot(oreCost: Ore) extends RobotTemplate(Ore(1))

case class ClayRobot(oreCost: Ore) extends RobotTemplate(Clay(1))

case class ObsidianRobot(oreCost: Ore, clayCost: Clay) extends RobotTemplate(Obsidian(1))

case class GeodeRobot(oreCost: Ore, obsidianCost: Obsidian) extends RobotTemplate(Geode(1))


case class RobotTemplates(ore: OreRobot,
                          clay: ClayRobot,
                          obsidian: ObsidianRobot,
                          geode: GeodeRobot)


case class Blueprint(id: Int, templates: RobotTemplates)


case class Robots(ore: Int, clay: Int, obsidian: Int, geode: Int)

case class State(blueprint: Blueprint, robots: Robots, resources: Resources)


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

    def oreParser: Parser[Ore] =
      (CommonParsers.int <* Parser.string(" ore")).map(Ore.apply)

    def clayParser: Parser[Clay] =
      (CommonParsers.int <* Parser.string(" clay")).map(Clay.apply)

    def obsidianParser: Parser[Obsidian] =
      (CommonParsers.int <* Parser.string(" obsidian")).map(Obsidian.apply)

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
    ???

  override def solvePart2(input: List[Blueprint]): Int =
    ???


@main def run(): Unit = Day19.run()
@main def test(): Unit = Day19.test()
@main def testParser(): Unit =
  Day19.testParser(Day19.parser)
@main def runParser(): Unit =
  Day19.runParser(Day19.parser)