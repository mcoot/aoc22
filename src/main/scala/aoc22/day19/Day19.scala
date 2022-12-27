package aoc22.day19

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.{Parser, Parser0}


sealed trait Resource[A <: Resource[A]](val amount: Int) extends Ordered[A]:
  override def compare(that: A): Int = amount compare that.amount

case class Ore(override val amount: Int) extends Resource[Ore](amount)

case class Clay(override val amount: Int) extends Resource[Clay](amount)

case class Obsidian(override val amount: Int) extends Resource[Obsidian](amount)

case class Geode(override val amount: Int) extends Resource[Geode](amount)

case class RobotCosts(oreRobotCost: Ore,
                      clayRobotCost: Ore,
                      obsidianRobotCost: (Ore, Clay),
                      geodeRobotCost: (Ore, Obsidian))


case class Blueprint(id: Int, costs: RobotCosts)


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

    def robotCostsParser: Parser[RobotCosts] =
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

    def blueprintParser: Parser[Blueprint] =
      for
        id <- blueprintIdParser <* lineEndingParser
        costs <- robotCostsParser
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