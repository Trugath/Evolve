package evolve

import evolve.core.Evolver.EvolverStrategy
import evolve.core._
import evolve.util.EvolveUtil
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

import scala.annotation.tailrec

/**
  * Created by Elliot on 09/08/2016.
  */
class EvolveFuncSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  import evolve.functions.BooleanFunctions._

  private implicit val evolveStrategy = EvolverStrategy(4, 0.005)

  private def evolveBoolean(program: Program, testCases: TestCases[Boolean]): Program = {
    @tailrec def go(program: Program, generation: Long, improvements: Long): Program = {
      Evolver(program, testCases, optimise = false) match {
        case Some(evolved) =>
          val score = testCases.score(evolved)
          if (score == 0) {
            evolved
          } else {
            go(evolved, generation + 1, improvements + 1)
          }

        case None =>
          go(program, generation + 1, improvements)
      }
    }

    go( program, 0, 0 )
  }

  "The Evolve system" should "evolve a basic program (Output True)" in {
    val testCases = TestCases(List(
      TestCase(Nil, List(true))
    ))

    val solution = evolveBoolean( Generator(Nop.instructionSize, 4, inputCount = 0, outputCount = 1), testCases )
    assert( solution(Nil).result(1) === List( true ) )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 100, optimise = true, testCases)
    assert( optimised(Nil).result(1) === List( true ) )

    assert( solution.cost >= optimised.cost )
  }

  it should "evolve a basic program (Output False)" in {
    val testCases = TestCases(List(
      TestCase(Nil, List(false))
    ))

    val solution = evolveBoolean( Generator(Nop.instructionSize, 4, inputCount = 0, outputCount = 1), testCases )
    assert( solution(Nil).result(1) === List( false ) )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 100, optimise = true, testCases)
    assert( optimised(Nil).result(1) === List( false ) )

    assert( solution.cost >= optimised.cost )
  }

  it should "evolve a basic program (Output Input)" in {
    val testCases = TestCases(List(
      TestCase(List(true), List(true)),
      TestCase(List(false), List(false))
    ))

    val solution = evolveBoolean( Generator(Nop.instructionSize, 4, inputCount = 1, outputCount = 1), testCases )
    assert( solution( List( true ) ).result(1) === List( true ) )
    assert( solution( List( false ) ).result(1) === List( false ) )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 100, optimise = true, testCases)
    assert( optimised( List( true ) ).result(1) === List( true ) )
    assert( optimised( List( false ) ).result(1) === List( false ) )

    assert( solution.cost >= optimised.cost )
  }

  it should "evolve a basic program (Output NOT Input)" in {
    val testCases = TestCases(List(
      TestCase(List(true), List(false)),
      TestCase(List(false), List(true))
    ))

    val solution = evolveBoolean( Generator(Nop.instructionSize, 4, inputCount = 1, outputCount = 1), testCases )
    assert( solution( List( true ) ).result(1) === List( false ) )
    assert( solution( List( false ) ).result(1) === List( true ) )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 100, optimise = true, testCases)
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

    val solution = evolveBoolean( Generator(Nop.instructionSize, 4, inputCount = 2, outputCount = 1), testCases )
    assert( solution( List( false, false ) ).result(1) === List( false ) )
    assert( solution( List( false, true ) ).result(1) === List( false ) )
    assert( solution( List( true, false ) ).result(1) === List( false ) )
    assert( solution( List( true, true ) ).result(1) === List( true ) )

    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 100, optimise = true, testCases)
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

    val solution = evolveBoolean( Generator(Nop.instructionSize, 64, inputCount = 3, outputCount = 2), testCases )
    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 1000, optimise = true, testCases)

    assert( solution.cost >= optimised.cost )
  }
}
