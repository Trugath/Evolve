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
import java.nio.file.{Paths, Files}

import evolve.core.Evolver.EvolverStrategy
import evolve.core._
import evolve.util.EvolveUtil

import scala.annotation.tailrec

object DivideByEight {

  def main(args: Array[String]): Unit = {

    import evolve.functions.IntegerFunctions._

    implicit val evolveStrategy = EvolverStrategy(128, 0.01)

    val testCases = TestCases(
      (0 until 2147483647 by 65535000)
        .map( i => TestCase(List(i), List(i / 8)) )
        .toList
    )

    @tailrec def function(program: Program, generation: Long, improvements: Long): Program = {
      if(generation >= 5000)
        return program

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

    val solution = EvolveUtil.counted(function(Generator(Nop.instructionSize, 8, 1, 1), 0, 0), 5000, optimise = false, testCases)
    Files.write(Paths.get("solution.dot"), DotGraph(solution).getBytes(StandardCharsets.UTF_8) )
    val optimised = EvolveUtil.counted(solution.shrink, 5000, optimise = true, testCases)
    Files.write(Paths.get("optimised.dot"), DotGraph(optimised).getBytes(StandardCharsets.UTF_8) )
  }
}
