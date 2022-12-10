package aoc22.day10

import scala.collection.mutable.Map as MutableMap
import aoc22.common.{CommonParsers, SolutionWithParser}
import cats.parse.Parser


enum Instruction(val requiredCycles: Int):
  case Noop extends Instruction(1)
  case AddX(value: Int) extends Instruction(2)

  def updateRegister(register: Int): Int =
    this match {
      case Noop => register
      case AddX(value) => register + value
    }


class CrtSystem(program: List[Instruction]):
  private val cycleToRegister: MutableMap[Int, Int] = MutableMap()

  private var register = 1
  private var cycleNumber = 1
  private var currentInstruction = program.head
  private var remainingProgram = program.tail
  private var cyclesUntilInstructionComplete = currentInstruction.requiredCycles

  // Terminated when no instructions left and the last instruction is finished
  private def isTerminated = remainingProgram.isEmpty && cyclesUntilInstructionComplete == 0

  private def step(): Unit =
    // Do nothing if we already terminated
    if isTerminated then
      return ()
    // -- Cycle start
    // Tick down cycles remaining on current instruction
    cyclesUntilInstructionComplete -= 1

    // -- Update cycle register mapping
    cycleToRegister(cycleNumber) = register

    // -- Cycle end - finish instruction if it is complete
    if cyclesUntilInstructionComplete == 0 then
      // Update register
      register = currentInstruction.updateRegister(register)
      if remainingProgram.nonEmpty then
        // Move onto the next instruction
        currentInstruction = remainingProgram.head
        remainingProgram = remainingProgram.tail
        cyclesUntilInstructionComplete = currentInstruction.requiredCycles

    // Move cycle count along
    cycleNumber += 1

  def exec(): Unit =
    while !isTerminated do
      step()

  def getCycleNumber: Int = cycleNumber
  def getRegisterForCycle(cycle: Int): Int = cycleToRegister.getOrElse(cycle, register)

  def signalStrength(cycle: Int): Int = getRegisterForCycle(cycle) * cycle

  def drawSpriteForCycle(cycle: Int): Boolean =
    val xPos = (cycle - 1) % 40
    val spriteCentre = getRegisterForCycle(cycle) % 40
    Math.abs(spriteCentre - xPos) <= 1


object Parsing:
  private def noopParser = Parser.string("noop").map(_ => Instruction.Noop)

  private def addxParser = for
    _ <- Parser.string("addx ")
    value <- CommonParsers.int
  yield
    Instruction.AddX(value)

  private def instructionParser: Parser[Instruction] = noopParser | addxParser

  def inputParser: Parser[List[Instruction]] = CommonParsers.lineSeparated(instructionParser)


object Day10 extends SolutionWithParser[List[Instruction], Int, Int]:
  override def dayNumber: Int = 10

  override def parser: Parser[List[Instruction]] = Parsing.inputParser

  override def solvePart1(input: List[Instruction]): Int =
    val system = CrtSystem(input)
    system.exec()
    List(20, 60, 100, 140, 180, 220)
      .map(system.signalStrength)
      .sum

  override def solvePart2(input: List[Instruction]): Int =
    val system = CrtSystem(input)
    system.exec()
    val resultString = (1 to system.getCycleNumber)
      .grouped(40)
      .flatMap { row =>
        row.map { cycleNumber =>
          if system.drawSpriteForCycle(cycleNumber) then
            '#'
          else
            '.'
        }.appended('\n').mkString
      }.mkString
    println(resultString)
    0


@main def run(): Unit = Day10.run()
@main def test(): Unit = Day10.test()
@main def testLarge(): Unit = Day10.test("large")
@main def testParser(): Unit =
  Day10.testParser(Day10.parser)
@main def runParser(): Unit =
  Day10.runParser(Day10.parser)