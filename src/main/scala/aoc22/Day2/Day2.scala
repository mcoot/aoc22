package aoc22.Day2

import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser
import cats.parse.Rfc5234


enum Outcome:
  case Win
  case Lose
  case Draw


sealed trait Action:
  def beatenBy: Action
  def beats: Action

  def outcome(opponent: Action): Outcome = opponent match
    case opp if beats == opp => Outcome.Win
    case opp if beatenBy == opp => Outcome.Lose
    case _ => Outcome.Draw

  def inherentScore = this match
    case Actions.Rock => 1
    case Actions.Paper => 2
    case Actions.Scissors => 3

  def outcomeScore(opponent: Action) = outcome(opponent) match
    case Outcome.Win => 6
    case Outcome.Draw => 3
    case Outcome.Lose => 0

  def totalScore(opponent: Action) = inherentScore + outcomeScore(opponent)


object Actions:
  case object Rock extends Action :
    override def beats = Scissors
    override def beatenBy = Paper

  case object Paper extends Action :
    override def beats = Rock
    override def beatenBy = Scissors

  case object Scissors extends Action :
    override def beats = Paper
    override def beatenBy = Rock


enum MyAction:
  case X extends MyAction
  case Y extends MyAction
  case Z extends MyAction

  def p1MapToAction(): Action = this match
    case X => Actions.Rock
    case Y => Actions.Paper
    case Z => Actions.Scissors

  def p2MapToAction(opponent: Action) = this match
    case X => opponent.beats
    case Y => opponent
    case Z => opponent.beatenBy


object Parsing:
  val opponentParser = Parser.charIn("ABC").map { c =>
    c match
      case 'A' => Actions.Rock
      case 'B' => Actions.Paper
      case 'C' => Actions.Scissors
  }

  val meParser = Parser.charIn("XYZ").map { c =>
    c match
      case 'X' => MyAction.X
      case 'Y' => MyAction.Y
      case 'Z' => MyAction.Z
  }

  val roundParser: Parser[(Action, MyAction)] = ((opponentParser <* CommonParsers.spaces) ~ meParser)

  val parser: Parser[List[(Action, MyAction)]] = CommonParsers.lineSeparated(roundParser)


object Day2 extends SolutionWithParser[List[(Action, MyAction)], Int, Int] {
  override def dayNumber: Int = 2

  override def parser = Parsing.parser

  override def solvePart1(input: List[(Action, MyAction)]) =
    input.map { case (opponent, me) =>
      me.p1MapToAction().totalScore(opponent)
    }.sum

  override def solvePart2(input: List[(Action, MyAction)]) =
    input.map { case (opponent, me) =>
      me.p2MapToAction(opponent).totalScore(opponent)
    }.sum
}

@main def run(): Unit = Day2.run()

@main def test(): Unit = Day2.test()

@main def testParser(): Unit = Day2.testParser(Parsing.parser)