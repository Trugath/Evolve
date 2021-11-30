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

object PerimeterElipse {

  def exact(a: Double, b: Double, terms: Int): Double = {
    val h = math.pow(a - b, 2) / math.pow(a + b, 2)

    def term(n: Int): Double = {
      math.pow(0.5 / n, 2) * math.pow(h, n)
    }

    math.Pi * (a + b) * (1.0 + (1 to terms).map(term).sum)
  }

  def main(args: Array[String]): Unit = {

    import evolve.functions.DoubleFunctions._

    object SquareRoot extends Function[Double] {
      override val arguments: Int = 1
      override val cost: Int = 10

      override def getLabel(inst: Instruction): String = "SquareRoot"

      override def apply(inst: Instruction, arguments: List[Double]): Double = {
        val a = arguments.head
        math.sqrt(a)
      }
    }

    implicit val functions: Seq[Function[Double]] = evolve.functions.DoubleFunctions.functions.take(7) :+ SquareRoot

    implicit val evolveStrategy: EvolverStrategy = EvolverStrategy(24, 0.05, optimiseForPipeline = true)

    implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors()))

    // calculate maximum term count for this machine
    @tailrec def termCount(index: Int): Int = {
      if (exact(20, 1, index) == exact(1, 20, index + 1)) {
        index
      } else {
        termCount(index + 1)
      }
    }

    val exactTermCount = termCount(0) + 1

    val testCases = TestCases(
      List[TestCase[Double]](
        TestCase(List(0.0, 0.0, math.Pi), List(0)),
        TestCase(List(0.0, 1.0, math.Pi), List(2.0)),
        TestCase(List(1.0, 0.0, math.Pi), List(2.0)),
        TestCase(List(1.0, 1.0, math.Pi), List(2.0 * math.Pi)),
        TestCase(List(0.0, 10.0, math.Pi), List(20.0)),
        TestCase(List(10.0, 0.0, math.Pi), List(20.0)),
        TestCase(List(10.0, 10.0, math.Pi), List(20.0 * math.Pi)),
      ) ++ (1 until 10).flatMap(index => {
        val exactValue = exact(index, 1.0, exactTermCount)
        List(
          TestCase(List(index, 1.0, math.Pi), List(exactValue)),
          TestCase(List(1.0, index, math.Pi), List(exactValue)),
        )
      }
      )
    )(Manifest.Double)

    val solution = EvolveUtil.fitness(Generator(Nop.instructionSize, 128, 3, 1), 0, 1000000, testCases)
    Files.write(Paths.get("solution.dot"), DotGraph(solution).getBytes(StandardCharsets.UTF_8))
    val optimised = EvolveUtil.counted(solution.nopInputs.nopOutputs.spread(4), 500000, optimise = true, testCases).denop.shrink.deduplicate
    Files.write(Paths.get("optimised.dot"), DotGraph(optimised).getBytes(StandardCharsets.UTF_8))
    println(optimised.shrink.denop)
    val pipelined = optimised.pipeline.deduplicate.pipeline.shrink
    Files.write(Paths.get("pipelined.dot"), DotGraph(pipelined).getBytes(StandardCharsets.UTF_8))
    System.exit(0)
  }
}
