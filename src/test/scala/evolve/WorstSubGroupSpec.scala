package evolve

import java.util.concurrent.Executors

import evolve.core.Evolver.EvolverStrategy
import evolve.core.{Generator, TestCase, TestCases}
import evolve.util.EvolveUtil
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

/**
  * Created by Elliot on 17/08/2016.
  */
class WorstSubGroupSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  private implicit val evolveStrategy: EvolverStrategy = EvolverStrategy( children = Math.max(4, Runtime.getRuntime.availableProcessors()), factor = 0.005, optimiseForPipeline = false )
  private implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor( Executors.newFixedThreadPool( Runtime.getRuntime.availableProcessors() ) )

  "The worstSubGroup utility function" should "evolve normally when subgroup is equal or larger the test case list size" in {

    import evolve.functions.BooleanFunctions._

    val testCases = TestCases(List(
      TestCase(List(false, false, false), List(false, false)),
      TestCase(List(false, false, true), List(false, true)),
      TestCase(List(false, true, false), List(false, true)),
      TestCase(List(false, true, true), List(true, false)),
      TestCase(List(true, false, false), List(false, true)),
      TestCase(List(true, false, true), List(true, false)),
      TestCase(List(true, true, false), List(true, false)),
      TestCase(List(true, true, true), List(true, true))
    ))

    forAll { seed: Int =>
      val initial = Generator( Nop.instructionSize, size = 16, inputCount = 3, outputCount = 2, seed )
      val initialScore = testCases.score( initial )

      val evolved = EvolveUtil.worstSubGroup(initial, groupSize = 8, 100, testCases)
      val evolvedScore = testCases.score( evolved )

      assert( evolvedScore <= initialScore )
    }
  }

  it should "evolve a worst-case program into a better one when given any group size >= 1" in {

    import evolve.functions.BooleanFunctions._

    val testCases = TestCases(List(
      TestCase(List(false, false, false), List(false, false)),
      TestCase(List(false, false, true), List(false, true)),
      TestCase(List(false, true, false), List(false, true)),
      TestCase(List(false, true, true), List(true, false)),
      TestCase(List(true, false, false), List(false, true)),
      TestCase(List(true, false, true), List(true, false)),
      TestCase(List(true, true, false), List(true, false)),
      TestCase(List(true, true, true), List(true, true))
    ))

    val incorrectTestCases = TestCases(List(
      TestCase(List(false, false, false), List(true, true)),
      TestCase(List(false, false, true), List(true, false)),
      TestCase(List(false, true, false), List(true, false)),
      TestCase(List(false, true, true), List(false, true)),
      TestCase(List(true, false, false), List(true, false)),
      TestCase(List(true, false, true), List(false, true)),
      TestCase(List(true, true, false), List(false, true)),
      TestCase(List(true, true, true), List(false, false))
    ))

    // evolve a worst case scoring program to re-evolve into a better one
    val startup = EvolveUtil.startup( Generator(Nop.instructionSize, size = 32, inputCount = 3, outputCount = 2), incorrectTestCases )
    val worstCaseProgram = EvolveUtil.fitness( startup, fitness = 0, limit = Long.MaxValue, incorrectTestCases )
    val worstCaseScore = testCases.score( worstCaseProgram )
    assert( worstCaseScore === 16000 )

    forAll( Gen.choose[Int](1, testCases.cases.length * 2) ) { groupSize =>
      val evolved = EvolveUtil.worstSubGroup(worstCaseProgram, groupSize, 100, testCases )
      val evolvedScore = testCases.score( evolved )
      assert( evolvedScore < worstCaseScore )
    }
  }
}
