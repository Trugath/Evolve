package evolve

import evolve.core.Evolver.EvolverStrategy
import evolve.core._
import evolve.util.EvolveUtil
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

/**
  * Created by Elliot on 09/08/2016.
  */
class EvolveFuncSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  import evolve.functions.BooleanFunctions._

  private implicit val evolveStrategy = EvolverStrategy( children = Math.max(4, Runtime.getRuntime.availableProcessors()), factor = 0.005, optimiseForPipeline = false )

  "The Evolve system" should "evolve a basic program (Output True)" in {
    val testCases = TestCases(List(
      TestCase(Nil, List(true))
    ))

    val startup = EvolveUtil.startup( Generator(Nop.instructionSize, size = 4, inputCount = 0, outputCount = 1), testCases )

    val solution = EvolveUtil.fitness( startup, fitness = 0, limit = Long.MaxValue, testCases )
    assert( testCases.score( solution ) === 0L )
    assert( solution(Nil).result(1) === List( true ) )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 100, optimise = true, testCases)
    assert( testCases.score( optimised ) === 0L )
    assert( optimised(Nil).result(1) === List( true ) )

    assert( solution.cost >= optimised.cost )
  }

  it should "evolve a basic program (Output False)" in {
    val testCases = TestCases(List(
      TestCase(Nil, List(false))
    ))

    val startup = EvolveUtil.startup( Generator(Nop.instructionSize, size = 4, inputCount = 0, outputCount = 1), testCases )

    val solution = EvolveUtil.fitness( startup, fitness = 0, limit = Long.MaxValue, testCases )
    assert( testCases.score( solution ) === 0L )
    assert( solution(Nil).result(1) === List( false ) )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 100, optimise = true, testCases)
    assert( testCases.score( optimised ) === 0L )
    assert( optimised(Nil).result(1) === List( false ) )

    assert( solution.cost >= optimised.cost )
  }

  it should "evolve a basic program (Output Input)" in {
    val testCases = TestCases(List(
      TestCase(List(true), List(true)),
      TestCase(List(false), List(false))
    ))

    val startup = EvolveUtil.startup( Generator(Nop.instructionSize, size = 4, inputCount = 1, outputCount = 1), testCases )

    val solution = EvolveUtil.fitness( startup, fitness = 0, limit = Long.MaxValue, testCases )
    assert( testCases.score( solution ) === 0L )
    assert( solution( List( true ) ).result(1) === List( true ) )
    assert( solution( List( false ) ).result(1) === List( false ) )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 100, optimise = true, testCases)
    assert( testCases.score( optimised ) === 0L )
    assert( optimised( List( true ) ).result(1) === List( true ) )
    assert( optimised( List( false ) ).result(1) === List( false ) )

    assert( solution.cost >= optimised.cost )
  }

  it should "evolve a basic program (Output NOT Input)" in {
    val testCases = TestCases(List(
      TestCase(List(true), List(false)),
      TestCase(List(false), List(true))
    ))

    val startup = EvolveUtil.startup( Generator(Nop.instructionSize, size = 4, inputCount = 1, outputCount = 1), testCases )

    val solution = EvolveUtil.fitness( startup, fitness = 0, limit = Long.MaxValue, testCases )
    assert( testCases.score( solution ) === 0L )
    assert( solution( List( true ) ).result(1) === List( false ) )
    assert( solution( List( false ) ).result(1) === List( true ) )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 100, optimise = true, testCases)
    assert( testCases.score( optimised ) === 0L )
    assert( optimised( List( true ) ).result(1) === List( false ) )
    assert( optimised( List( false ) ).result(1) === List( true ) )

    assert( solution.cost >= optimised.cost )
  }

  it should "evolve a basic program (Output AND Input)" in {
    val testCases = TestCases(List(
      TestCase(List(false, false), List(false)),
      TestCase(List(false, true), List(false)),
      TestCase(List(true, false), List(false)),
      TestCase(List(true, true), List(true))
    ))

    val startup = EvolveUtil.startup( Generator(Nop.instructionSize, size = 32, inputCount = 2, outputCount = 1), testCases )

    val solution = EvolveUtil.fitness( startup, fitness = 0, limit = Long.MaxValue, testCases )
    assert( testCases.score( solution ) === 0L )
    assert( solution( List( false, false ) ).result(1) === List( false ) )
    assert( solution( List( false, true ) ).result(1) === List( false ) )
    assert( solution( List( true, false ) ).result(1) === List( false ) )
    assert( solution( List( true, true ) ).result(1) === List( true ) )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 100, optimise = true, testCases)
    assert( testCases.score( optimised ) === 0L )
    assert( optimised( List( false, false ) ).result(1) === List( false ) )
    assert( optimised( List( false, true ) ).result(1) === List( false ) )
    assert( optimised( List( true, false ) ).result(1) === List( false ) )
    assert( optimised( List( true, true ) ).result(1) === List( true ) )

    assert( solution.cost >= optimised.cost )
  }

  it should "evolve a three bit adder" in {
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

    val startup = EvolveUtil.startup( Generator(Nop.instructionSize, size = 32, inputCount = 3, outputCount = 2), testCases )
    val solution = EvolveUtil.fitness( startup, fitness = 0, limit = Long.MaxValue, testCases )
    assert( testCases.score( solution ) === 0L )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 1000, optimise = true, testCases)
    assert( testCases.score( optimised ) === 0L )

    assert( solution.cost >= optimised.cost )
  }

  "optimising" should "never make the score worse" in {
    val testCases = TestCases(List(
      TestCase(List(false, false), List(false)),
      TestCase(List(false, true), List(false)),
      TestCase(List(true, false), List(false)),
      TestCase(List(true, true), List(true))
    ))

    forAll { seed: Int =>
      val initial = Generator( Nop.instructionSize, size = 8, inputCount = 2, outputCount = 1, seed )
      val initialScore = testCases.score( initial )

      val optimised = EvolveUtil.counted(initial, 100, optimise = true, testCases)
      val optimisedScore = testCases.score( optimised )

      assert( optimisedScore <= initialScore )
    }
  }

  "optimising a perfect scoring program" should "not make its cost worse" in {
    val testCases = TestCases(List(
      TestCase(List(false, false), List(false)),
      TestCase(List(false, true), List(false)),
      TestCase(List(true, false), List(false)),
      TestCase(List(true, true), List(true))
    ))

    forAll { seed: Int =>
      val solution = EvolveUtil.fitness( Generator(Nop.instructionSize, size = 8, inputCount = 2, outputCount = 1, seed), fitness = 0, limit = Long.MaxValue, testCases)
      assert( testCases.score(solution) === 0L )
      val optimised = EvolveUtil.counted(solution, 100, optimise = true, testCases)
      assert( testCases.score(optimised) === 0L )
      assert( optimised.cost <= solution.cost )
    }
  }
}
