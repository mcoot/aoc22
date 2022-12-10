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


case class ProcessorState(instruction: Instruction, cyclesLeft: Int):
  def isIdle: Boolean = cyclesLeft <= 0

  def step: ProcessorState =
    // Bounds check to ensure we don't accidentally continue processing after instr complete
    if isIdle then
      throw new Exception(s"Attempted to continue processing after instruction complete: ${this}")
    ProcessorState(instruction, cyclesLeft - 1)

//  def updateRegister(register: Int): Int =
//    // Update the register if we have completed the current instruction
//    if isIdle then
//      instruction.updateRegister(register)
//    else
//      register


case class SystemState(processor: ProcessorState, cycleNumber: Int, register: Int):
  def signalStrength: Int = cycleNumber * register

  // Returns the new state, and whether it has started the next instruction
  def step(nextInstruction: Instruction): (SystemState, Boolean) =
    // Check if we completed the prior instruction and should therefore take a new one
    val completedLastInstruction = processor.isIdle

    // Update the register if needed
    val newRegister = if completedLastInstruction then
      processor.instruction.updateRegister(register)
    else
      register

    // Take the new instruction if required
    val newProcessor = if completedLastInstruction then
      ProcessorState(nextInstruction, nextInstruction.requiredCycles)
    else
      processor

    // Return the new state, and whether we took an instruction or not
    (SystemState(newProcessor.step, cycleNumber + 1, newRegister), completedLastInstruction)


case class ProgramState(system: SystemState, instructions: List[Instruction]):
  def isTerminated: Boolean = instructions.isEmpty

  def step: ProgramState =
    if isTerminated then
      // If we have completed our instructions do nothing
      this
    else
      val (nextSystem, tookInstruction) = system.step(instructions.head)
      val nextInstructions = if tookInstruction then
        instructions.tail
      else
        instructions
      ProgramState(nextSystem, nextInstructions)


object ProgramState:
  def initial(instructions: List[Instruction]): ProgramState =
    // Start with state where cycle number is 1, register is 1, processor has a completed noop (i.e. is idle)
    ProgramState(SystemState(ProcessorState(Instruction.Noop, 0), 0, 1), instructions)


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
    val pastStates = MutableMap[Int, ProgramState]()
    var state = ProgramState.initial(input)
    pastStates.addOne((state.system.cycleNumber, state))
    while !state.isTerminated do
      state = state.step
      pastStates.addOne((state.system.cycleNumber, state))
    List(20, 60, 100, 140, 180, 220)
      .map(pastStates.getOrElse(_, state).system.signalStrength)
      .sum

  override def solvePart2(input: List[Instruction]): Int = ???


@main def run(): Unit = Day10.run()
@main def test(): Unit = Day10.test()
@main def testLarge(): Unit = Day10.test("large")
@main def testParser(): Unit =
  Day10.testParser(Day10.parser)
@main def runParser(): Unit =
  Day10.runParser(Day10.parser)