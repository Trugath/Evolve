package evolve.benchmark

import evolve.core._
import org.scalameter.api.{Gen, _}

/**
  * Created by Elliot on 13/02/2017.
  */
object ExecutionBenchmark extends Bench.LocalTime {

  import evolve.functions.BooleanFunctions.functions

  private [this] val testCases = TestCases(List(
    TestCase(List(false, false, false), List(false, false)),
    TestCase(List(false, false, true), List(false, true)),
    TestCase(List(false, true, false), List(false, true)),
    TestCase(List(false, true, true), List(true, false)),
    TestCase(List(true, false, false), List(false, true)),
    TestCase(List(true, false, true), List(true, false)),
    TestCase(List(true, true, false), List(true, false)),
    TestCase(List(true, true, true), List(true, true))
  ))

  private [this] val inputs = testCases.cases.map( _.inputs )

  performance of "Three bit adder" config (
    exec.minWarmupRuns -> 1000,
    exec.maxWarmupRuns -> 4500
  ) in {
    val instr = Seq(Instruction(201326593), Instruction(134217729), Instruction(402653186), Instruction(16384), Instruction(201359366), Instruction(24576), Instruction(134275080), Instruction(8192), Instruction(402694154), Instruction(90112), Instruction(134275080), Instruction(90112))
    val a = Program(6,instr,3,2,instr.length)
    measure method "apply" in {
      using(Gen.unit("testCases")) in { _ =>
        inputs.foreach( i => a.apply(i, List.fill(a.length)(false)) )
      }
    }
  }

  performance of "Random three input, two output, boolean programs" in {
    using(Gen.range("length")(32, 4096, 32)) in { size =>
      val program = Generator(6, size, 3, 2)(evolve.functions.BooleanFunctions.functions)
      measure method "apply" in {
        inputs.foreach(i => program.apply(i, List.fill(program.length)(false)))
      }
    }
  }
}
