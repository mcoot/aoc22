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


case class InstructionState(instruction: Instruction, cyclesLeft: Int):
  def isComplete = cyclesLeft <= 0


case class SystemState(currentInstruction: InstructionState, cycleNumber: Int, register: Int):
  def signalStrength: Int = cycleNumber * register


object SystemState:
  def initial: SystemState = SystemState(InstructionState(Instruction.Noop, 0), 0, 1)


class CrtSystem(initialState: SystemState, program: List[Instruction]):
  private var currentState = initialState
  private val pastStates: MutableMap[Int, SystemState] = MutableMap()

  private var executingProgram = program

  def isTerminated: Boolean = executingProgram.isEmpty

  def getStateAtCycle(cycleNumber: Int): SystemState =
    pastStates.getOrElse(cycleNumber, currentState)

  private def step(): Unit =
    // Store this state as a known past state
    pastStates.addOne((currentState.cycleNumber, currentState))
    // Do nothing if we have finished our program
    if isTerminated then
      return ()

    // If we finished our last instruction, update the register and feed a new one in
    if currentState.currentInstruction.isComplete then
      val newRegisterValue = currentState.currentInstruction.instruction.updateRegister(currentState.register)
      // Remove the next instruction from our remaining program
      val nextInstruction = executingProgram.head
      executingProgram = executingProgram.tail
      // Update the state to add the new instruction
      currentState = SystemState(
        InstructionState(nextInstruction, nextInstruction.requiredCycles),
        currentState.cycleNumber,
        newRegisterValue
      )

    // Step the current instruction (including a newly-started one)
    val newInstructionState = InstructionState(
      currentState.currentInstruction.instruction,
      currentState.currentInstruction.cyclesLeft - 1
    )
    currentState = SystemState(newInstructionState, currentState.cycleNumber + 1, currentState.register)
//    println(currentState)

  def exec(): Unit =
    while !isTerminated do
      step()


object Parsing:
  private def noopParser = Parser.string("noop").map(_ => Instruction.Noop)

  private def addxParser = for
    _ <- Parser.string("addx ")
    value <- CommonParsers.int
  yield
    Instruction.AddX(value)

  private def instructionParser: Parser[Instruction] = noopParser | addxParser

  def inputParser = CommonParsers.lineSeparated(instructionParser)


object Day10 extends SolutionWithParser[List[Instruction], Int, Int]:
  override def dayNumber: Int = 10

  override def parser: Parser[List[Instruction]] = Parsing.inputParser

  override def solvePart1(input: List[Instruction]): Int =
    val system = CrtSystem(SystemState.initial, input)
    system.exec()
    List(20, 60, 100, 140, 180, 220)
      .map(system.getStateAtCycle(_).signalStrength)
      .sum


  override def solvePart2(input: List[Instruction]): Int = ???


@main def run(): Unit = Day10.run()
@main def test(): Unit = Day10.test()
@main def testLarge(): Unit = Day10.test("large")
@main def testParser(): Unit =
  Day10.testParser(Day10.parser)
@main def runParser(): Unit =
  Day10.runParser(Day10.parser)