/*
  Copyright (c) 2015, Elliot Stirling
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

object FourBitFullAdderNAND {

  def main(args: Array[String]): Unit = {

    import evolve.functions.BooleanFunctions.zero
    import evolve.functions.BooleanFunctions.scoreFunc
    import evolve.functions.BooleanFunctions.Nop

    object NAnd extends Function[Boolean]  {
      override def cost: Int = 2
      override def getLabel(inst: Instruction): String = "!&"
      override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
        val a = arguments.head
        val b = arguments(1)
        !(a&b)
      }
    }

    implicit val functions = Seq[Function[Boolean]](
      Nop, NAnd
    )

    implicit val evolveStrategy = EvolverStrategy(12, 0.001)

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


    @tailrec def function(program: Program, generation: Long, improvements: Long): Program = {
      Evolver(program, testCases, optimise = false) match {
        case Some(evolved) =>
          val score = testCases.score(evolved)
          if (score == 0) {
            println( s"Solution found after $generation generations with $improvements mutations." )
            evolved
          } else {
            if( generation % 100 == 0) {
              println( s"Processed $generation generations with $improvements mutations. Current score: $score" )
            }
            function(evolved, generation + 1, improvements + 1)
          }

        case None =>
          function(program, generation + 1, improvements)
      }
    }

    val solution = function(Generator(Nop.instructionSize, 512, 8, 5), 0, 0)
    Files.write(Paths.get("solution.dot"), DotGraph(solution).getBytes(StandardCharsets.UTF_8) )
    val optimised = EvolveUtil.counted(solution.shrink.spread(2), 10000, optimise = true, testCases)
    Files.write(Paths.get("optimised.dot"), DotGraph(optimised).getBytes(StandardCharsets.UTF_8) )
  }
}
