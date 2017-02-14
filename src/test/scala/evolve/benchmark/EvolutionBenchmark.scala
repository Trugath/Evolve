package evolve.benchmark

import java.util.concurrent.Executors

import evolve.core.Evolver.EvolverStrategy
import evolve.core._
import org.scalameter.api.{Gen, _}

import scala.concurrent.ExecutionContext

/**
  * Created by Elliot on 13/02/2017.
  */
object EvolutionBenchmark extends Bench.LocalTime {

  import evolve.functions.BooleanFunctions._

  private [this] implicit val ec = ExecutionContext.fromExecutor( Executors.newFixedThreadPool( Runtime.getRuntime.availableProcessors() * 2 ) )

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

  performance of "Evolving 24 three input, two output, boolean programs" config (
    exec.minWarmupRuns -> 2,
    exec.maxWarmupRuns -> 8
  ) in {

    implicit val evolveStrategy = EvolverStrategy(24, 0.00025, optimiseForPipeline = true)

    using(Gen.range("length")(32, 4096, 32)) in { size =>
      val program = Generator(6, size, 3, 2)(evolve.functions.BooleanFunctions.functions)
      measure method "apply" in {
        Evolver(program, testCases, optimise = false)
        Evolver(program, testCases, optimise = true)
      }
    }
  }
}
