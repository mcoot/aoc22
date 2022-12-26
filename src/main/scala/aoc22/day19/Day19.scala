package aoc22.day19

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Parser, Parser0}

enum Resource:
  case Ore(amount: Int)
  case Clay(amount: Int)
  case Obsidian(amount: Int)
  case Geode(amount: Int)


case class RobotCosts(ore: Resource.Ore,
                      clay: Resource.Ore,
                      obsidian: (Resource.Ore, Resource.Clay),
                      geode: (Resource.Ore, Resource.Obsidian))



case class Blueprint(id: Int, costs: RobotCosts)

object Day19 extends SolutionWithParser[List[Blueprint], Int, Int]:
  override def dayNumber: Int = 19

  private def lineEndingParser: Parser0[Unit] =
    (CommonParsers.spaceOrNewline ~ CommonParsers.spaces.?).map(_ => ())

  private def blueprintIdParser: Parser[Int] =
    for
      _ <- Parser.string("Blueprint ")
      id <- CommonParsers.int
      _ <- Parser.char(':')
    yield
      id

  private def oreParser: Parser[Resource.Ore] =
    (CommonParsers.int <* Parser.string(" ore")).map(Resource.Ore.apply)

  private def clayParser: Parser[Resource.Clay] =
    (CommonParsers.int <* Parser.string(" clay")).map(Resource.Clay.apply)

  private def obsidianParser: Parser[Resource.Obsidian] =
    (CommonParsers.int <* Parser.string(" obsidian")).map(Resource.Obsidian.apply)

  private def robotCostsParser: Parser[RobotCosts] =
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
      RobotCosts(oreRobotCost,
        clayRobotCost,
        (obsidianRobotOreCost, obsidianRobotClayCost),
        (geodeRobotOreCost, geodeRobotObsidianCost))


  private def blueprintParser =
    for
      id <- blueprintIdParser <* lineEndingParser
      costs <- robotCostsParser
    yield
      Blueprint(id, costs)

  override def parser: Parser[List[Blueprint]] =
    CommonParsers.lineSeparated(blueprintParser)

  override def solvePart1(input: List[Blueprint]): Int = ???

  override def solvePart2(input: List[Blueprint]): Int = ???


@main def run(): Unit = Day19.run()
@main def test(): Unit = Day19.test()
@main def testParser(): Unit =
  Day19.testParser(Day19.parser)
@main def runParser(): Unit =
  Day19.runParser(Day19.parser)