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


class CrtSystem2(program: List[Instruction]):
  private val cycleToRegister: MutableMap[Int, Int] = MutableMap()

  private var register = 1
  private var cycleNumber = 1
  private var currentInstruction = program.head
  private var remainingProgram = program.tail
  private var cyclesUntilInstructionComplete = currentInstruction.requiredCycles

  // Terminated when no instructions left and the last instruction is finished
  private def isTerminated = remainingProgram.isEmpty && cyclesUntilInstructionComplete == 0

  private def step(): Unit =
//    println(s"Cycle ${cycleNumber} | ${currentInstruction} | ${remainingProgram}")
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




case class InstructionState(instruction: Instruction, cyclesLeft: Int):
  def isComplete: Boolean = cyclesLeft <= 0

  def step: InstructionState = InstructionState(instruction, cyclesLeft - 1)

  def updateRegisterIfFinished(register: Int): Int =
    if isComplete then
      instruction.updateRegister(register)
    else
      register


case class SystemState(currentInstruction: InstructionState, cycleNumber: Int, register: Int):
  def finishInstruction(newInstruction: Instruction): SystemState =
    SystemState(
      InstructionState(newInstruction, newInstruction.requiredCycles),
      cycleNumber,
      currentInstruction.updateRegisterIfFinished(register)
    )

  def step: SystemState =
    SystemState(currentInstruction.step, cycleNumber + 1, register)

  def signalStrength: Int = cycleNumber * register

  private def xPosition = cycleNumber % 40
  private def spriteCentre: Int = register % 40

  def spriteHasPixel: Boolean = Math.abs(spriteCentre - xPosition) <= 1


object SystemState:
  def initial: SystemState = SystemState(InstructionState(Instruction.Noop, 0), 0, 1)


class CrtSystem(initialState: SystemState, program: List[Instruction]):
  private var currentState = initialState
  private val pastStates: MutableMap[Int, SystemState] = MutableMap()

  private var executingProgram = program

  def isTerminated: Boolean = executingProgram.isEmpty

  def getStateAtCycle(cycleNumber: Int): SystemState =
    pastStates.getOrElse(cycleNumber, currentState)

  def cyclesProcessed: Int = currentState.cycleNumber

  private def step(): Unit =
    // Store this state as a known past state
    pastStates.addOne((currentState.cycleNumber, currentState))

    // Do nothing if we have finished our program
    if isTerminated then
      return ()

    // Increment cycle and update register if instruction complete
    currentState = currentState.step

    // If we finished our last instruction, update the register and feed a new one in
    if currentState.currentInstruction.isComplete then
      // Update the state to add the new instruction
      currentState = currentState.finishInstruction(executingProgram.head)
      executingProgram = executingProgram.tail

    val spriteVal = if currentState.spriteHasPixel then '#' else '.'
    println(
      s"cycle ${currentState.cycleNumber} " +
        s"| Instr ${currentState.currentInstruction.instruction} rem ${currentState.currentInstruction.cyclesLeft} " +
        s"| Register ${currentState.register} | Sprite ${spriteVal}"
    )

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
    val system = CrtSystem2(input)
    system.exec()
    List(20, 60, 100, 140, 180, 220)
      .map(system.signalStrength)
      .sum

  override def solvePart2(input: List[Instruction]): Int =
    val system = CrtSystem2(input)
    system.exec()
//    println((1 to 22).map(c => (c, system.getStateAtCycle(c).register)))
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