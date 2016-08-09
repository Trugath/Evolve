package evolve

import evolve.core.Evolver.EvolverStrategy
import evolve.core._
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

import scala.annotation.tailrec

/**
  * Created by Elliot on 09/08/2016.
  */
class EvolveFuncSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {
  "The Evolve system" should "a basic program (Output True)" in {
    import evolve.functions.BooleanFunctions._

    implicit val evolveStrategy = EvolverStrategy(4, 0.005)

    val testCases = TestCases(List(
      TestCase(Nil, List(true))
    ))

    @tailrec def function(program: Program, generation: Long, improvements: Long): Program = {
      Evolver(program, testCases, optimise = false) match {
        case Some(evolved) =>
          val score = testCases.score(evolved)
          if (score == 0) {
            evolved
          } else {
            function(evolved, generation + 1, improvements + 1)
          }

        case None =>
          function(program, generation + 1, improvements)
      }
    }

    val solution = function( Generator(Nop.instructionSize, 4, inputCount = 0, outputCount = 1), 0, 0)
    assert( solution(Nil).result(1) === List( true ) )
  }

  it should "a basic program (Output False)" in {
    import evolve.functions.BooleanFunctions._

    implicit val evolveStrategy = EvolverStrategy(4, 0.005)

    val testCases = TestCases(List(
      TestCase(Nil, List(false))
    ))

    @tailrec def function(program: Program, generation: Long, improvements: Long): Program = {
      Evolver(program, testCases, optimise = false) match {
        case Some(evolved) =>
          val score = testCases.score(evolved)
          if (score == 0) {
            evolved
          } else {
            function(evolved, generation + 1, improvements + 1)
          }

        case None =>
          function(program, generation + 1, improvements)
      }
    }

    val solution = function( Generator(Nop.instructionSize, 4, inputCount = 0, outputCount = 1), 0, 0)
    assert( solution(Nil).result(1) === List( false ) )
  }

  it should "a basic program (Output Input)" in {
    import evolve.functions.BooleanFunctions._

    implicit val evolveStrategy = EvolverStrategy(4, 0.005)

    val testCases = TestCases(List(
      TestCase(List(true), List(true)),
      TestCase(List(false), List(false))
    ))

    @tailrec def function(program: Program, generation: Long, improvements: Long): Program = {
      Evolver(program, testCases, optimise = false) match {
        case Some(evolved) =>
          val score = testCases.score(evolved)
          if (score == 0) {
            evolved
          } else {
            function(evolved, generation + 1, improvements + 1)
          }

        case None =>
          function(program, generation + 1, improvements)
      }
    }

    val solution = function( Generator(Nop.instructionSize, 4, inputCount = 1, outputCount = 1), 0, 0)
    assert( solution( List( true ) ).result(1) === List( true ) )
    assert( solution( List( false ) ).result(1) === List( false ) )
  }
}
