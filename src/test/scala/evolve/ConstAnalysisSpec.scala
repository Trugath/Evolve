package evolve

import evolve.core.{ConstAnalysis, Instruction, Program}
import org.scalatest._
import flatspec._
import matchers._

/**
 * Created by Elliot on 11/04/2017.
 */
class ConstAnalysisSpec extends AnyFlatSpec {

  "A single constant instruction program" should "correctly identify as fully constant and not shrink on constant filling" in {
    import evolve.functions.NeuralFunctions._
    val program = Program(Nop.instructionSize, createConstant(functions)(1.0) :: Nil, 0, 1, 1)

    val analysis = ConstAnalysis(program)
    assert(analysis.forall(a => a))
    assert(ConstAnalysis.fillConstants(program).shrink === program)
  }

  "A nopped Constant output program" should "collapse into a single instruction" in {
    import evolve.functions.NeuralFunctions._
    val program = Program(Nop.instructionSize, createConstant(functions)(1.0) :: Nil, 0, 1, 1)
    val nopped = program.nopOutputs.nopOutputs.nopOutputs

    val analysis = ConstAnalysis(nopped)
    assert(analysis.forall( a => a ))

    val shrunk = ConstAnalysis.fillConstants(nopped).shrink
    assert(shrunk.length === 1)
    assert(shrunk === program)
  }
}
