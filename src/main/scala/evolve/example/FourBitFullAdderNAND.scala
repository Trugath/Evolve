/*
  Copyright (c) 2016, Elliot Stirling
  All rights reserved.

  Redistribution and use in source and binary forms, with or without modification,
  are permitted provided that the following conditions are met:

  * Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

  * Redistributions in binary form must reproduce the above copyright notice, this
  list of conditions and the following disclaimer in the documentation and/or
  other materials provided with the distribution.

  * Neither the name of the {organization} nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
  ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
  ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

package evolve.example

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import evolve.core.Evolver.EvolverStrategy
import evolve.core._
import evolve.util.EvolveUtil

import scala.annotation.tailrec
import scala.util.Random

object FourBitFullAdderNAND {

  def main(args: Array[String]): Unit = {

    import evolve.functions.BooleanFunctions.zero
    import evolve.functions.BooleanFunctions.scoreFunc

    object Nop extends Function[Boolean]  {
      override def instructionSize: Int = 3
      override def argumentSize: Int = 9
      override def arguments: Int = 1
      override def cost: Int = 1
      override def getLabel(inst: Instruction): String = "Nop"

      override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
        arguments.head
      }
    }

    object NAnd1 extends Function[Boolean]  {
      override def instructionSize: Int = 3
      override def argumentSize: Int = 9
      override def cost: Int = 5
      override def arguments: Int = 1
      override def getLabel(inst: Instruction): String = "!&"
      override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
        !arguments.head
      }
    }

    object NAnd2 extends Function[Boolean]  {
      override def instructionSize: Int = 3
      override def argumentSize: Int = 9
      override def cost: Int = 6
      override def arguments: Int = 2
      override def getLabel(inst: Instruction): String = "!&"
      override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
        val a = arguments.head
        val b = arguments(1)
        !(a&b)
      }
    }

    object NAnd3 extends Function[Boolean]  {
      override def instructionSize: Int = 3
      override def argumentSize: Int = 9
      override def cost: Int = 7
      override def arguments: Int = 3
      override def getLabel(inst: Instruction): String = "!&"
      override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
        val a = arguments.head
        val b = arguments(1)
        val c = arguments(2)
        !(a&b&c)
      }
    }

    implicit val functions = Seq[Function[Boolean]](
      Nop, NAnd1, NAnd2, NAnd3
    )

    implicit val evolveStrategy = EvolverStrategy(24, 0.00025)

    def bitsToBools(value: Int, bits: Int): List[Boolean] = {
      require(value >= 0 && value <= math.pow(2, bits))
      (0 until bits)
        .map( i => ((0x1 << i) & value) != 0x0 )
        .reverse
        .toList
    }

    val testCases = TestCases((for{
      l <- 0 until 16
      r <- 0 until 16
    } yield TestCase[Boolean](bitsToBools(l, 4) ::: bitsToBools(r, 4), bitsToBools(l + r, 5))).toList )


    @tailrec def function(program: Program, generation: Long, optimise: Boolean = false): Program = {

      /*
       * Evolving against the whole list of test cases caused the evolver to be risk averse. As such evolution would
       * stall at about a score of 64k and still be going after 1 million generations. To combat this the test cases
       * are randomly shuffled into 4 groups of 64 cases and scored against the current program. The worst group of
       * 64 is selected for a short run of evolution. This allows the evolver to evolve to selectively solve some
       * cases at the expense of others. CGP lends itself towards this as 'unused-genes' persist between generations.
       * Result: Once implemented this evolve function regularly solves in under 200k generations.
       */
      val partial =  EvolveUtil.worstSubGroup(program, 64, 100, testCases, optimise = false)

      val result =  EvolveUtil.fitness(partial, 0, 900, testCases, optimise)
      val score = testCases.score(result)
      if (score == 0) {
        println( s"Solution found after $generation generations." )
        result
      } else {
        println( s"Processed $generation generations. Current score: $score. Current size: ${program.data.length}" )
        if( generation % 10000 == 0 ) {
          function(result, generation + 1000, !optimise)
        } else {
          function(result, generation + 1000, optimise)
        }
      }
    }

    val solution = function(EvolveUtil.startup(Generator(Nop.instructionSize, 504, 8, 5), testCases), 0)
    Files.write(Paths.get("solution.dot"), DotGraph(solution).getBytes(StandardCharsets.UTF_8) )

    // three rounds of optimisation and shrinking
    val optimised1 = EvolveUtil.counted(solution, 100000, optimise = true, testCases).shrink
    val optimised2 = EvolveUtil.counted(optimised1, 100000, optimise = true, testCases).shrink
    val optimised3 = EvolveUtil.counted(optimised2, 100000, optimise = true, testCases).shrink
    Files.write(Paths.get("optimised.dot"), DotGraph(optimised3).getBytes(StandardCharsets.UTF_8) )
  }
}
