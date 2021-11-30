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
import java.util.concurrent.Executors

import evolve.core.Evolver.EvolverStrategy
import evolve.core._
import evolve.util.EvolveUtil

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object ThreeBitAdderNAND {

  def main(args: Array[String]): Unit = {

    import evolve.functions.BooleanFunctions.scoreFunc

    object Nop extends Function[Boolean]  {
      override val instructionSize: Int = 3
      override val argumentSize: Int = 9
      override val arguments: Int = 1
      override val cost: Int = 1
      override def getLabel(inst: Instruction): String = "Nop"

      override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
        arguments.head
      }
    }

    object NAnd1 extends Function[Boolean]  {
      override val instructionSize: Int = 3
      override val argumentSize: Int = 9
      override val cost: Int = 5
      override val arguments: Int = 1
      override def getLabel(inst: Instruction): String = "!&"
      override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
        !arguments.head
      }
    }

    object NAnd2 extends Function[Boolean]  {
      override val instructionSize: Int = 3
      override val argumentSize: Int = 9
      override val cost: Int = 6
      override val arguments: Int = 2
      override def getLabel(inst: Instruction): String = "!&"
      override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
        val a = arguments.head
        val b = arguments(1)
        !(a&b)
      }
    }

    object NAnd3 extends Function[Boolean]  {
      override val instructionSize: Int = 3
      override val argumentSize: Int = 9
      override val cost: Int = 7
      override val arguments: Int = 3
      override def getLabel(inst: Instruction): String = "!&"
      override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
        val a = arguments.head
        val b = arguments(1)
        val c = arguments(2)
        !(a&b&c)
      }
    }

    implicit val functions: Seq[Function[Boolean]] = Seq[Function[Boolean]](
      Nop, NAnd1, NAnd2, NAnd3
    )

    implicit val evolveStrategy: EvolverStrategy = EvolverStrategy(12, 0.005, optimiseForPipeline = true)
    implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor( Executors.newFixedThreadPool( Runtime.getRuntime.availableProcessors() ) )

    val testCases = TestCases(List(
      TestCase(List(false, false, false), List(false, false)),
      TestCase(List(false, false, true), List(false, true)),
      TestCase(List(false, true, false), List(false, true)),
      TestCase(List(false, true, true), List(true, false)),
      TestCase(List(true, false, false), List(false, true)),
      TestCase(List(true, false, true), List(true, false)),
      TestCase(List(true, true, false), List(true, false)),
      TestCase(List(true, true, true), List(true, true))
    ))(Manifest.Boolean)

    @tailrec def function(program: Program, generation: Long, improvements: Long): Program = {
      Evolver(program, testCases, optimise = false) match {
        case Some(evolved) =>
          val score = testCases.score(evolved)
          if (score == 0) {
            println( s"Solution found after $generation generations with $improvements mutations." )
            evolved
          } else {
            function(evolved, generation + 1, improvements + 1)
          }

        case None =>
          function(program, generation + 1, improvements)
      }
    }

    val solution = function(Generator(Nop.instructionSize, 32, 3, 2), 0, 0).denop
    Files.write(Paths.get("solution.dot"), DotGraph(solution).getBytes(StandardCharsets.UTF_8) )
    val optimised = EvolveUtil.counted(solution.nopInputs.nopOutputs.spread(3), 1000000, optimise = true, testCases).denop
    Files.write(Paths.get("optimised.dot"), DotGraph(optimised).getBytes(StandardCharsets.UTF_8) )
    val pipelined = optimised.pipeline.deduplicate.pipeline.shrink
    Files.write(Paths.get("pipelined.dot"), DotGraph(pipelined).getBytes(StandardCharsets.UTF_8) )

    System.exit(0)
  }
}
