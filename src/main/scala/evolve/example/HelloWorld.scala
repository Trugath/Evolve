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

object HelloWorld {
  def main(args: Array[String]): Unit = {

    import evolve.functions.StringIntFunctions._

    implicit val evolveStatagy = EvolverStrategy(12, 0.01)

    val testCases = TestCases(List(
      TestCase(List(("abcdefghijklmnopqrstuvwxyz", 26), ("123456789", 9)), List(("Abcdefghijklmnopqrstuvwxyz 123456789!",27))),
      TestCase(List(("a", 1), ("b", 1)), List(("A B!",4))),
      TestCase(List(("c", 1), ("d", 1)), List(("C D!",4))),
      TestCase(List(("e", 1), ("f", 1)), List(("E F!",4))),
      TestCase(List(("g", 1), ("h", 1)), List(("G H!",4))),
      TestCase(List(("i", 1), ("j", 1)), List(("I J!",4))),
      TestCase(List(("hello", 5), ("Steve", 5)), List(("Hello Steve!",12))),
      TestCase(List(("hello", 5), ("Bob", 3)), List(("Hello Bob!",10))),
      TestCase(List(("hello", 5), ("Cat", 3)), List(("Hello Cat!",10))),
      TestCase(List(("hello", 5), ("Dog", 3)), List(("Hello Dog!",10)))
    ))

    // speed tracker
    val oneSecond = 1000000000
    var speedMarkTime = System.nanoTime
    var generations = 0
    var bestScore = Long.MaxValue





    /**
     * steady state evolution till time or fitness prevail
     */
    @tailrec def function(program: Program, generation: Long): Program = {
      if(generation >= 10000000)
        return program

      val now = System.nanoTime
      // fps tracker

      if (now > speedMarkTime + oneSecond) {
        println(s"score: $bestScore generations: $generations (${generations * evolveStatagy.children} evaluated children)")
        speedMarkTime += oneSecond
        generations = 100
      } else {
        generations += 100
      }

      val evolved = EvolveUtil.counted(program, 100, optimise = false, testCases)
      bestScore = testCases.score(evolved)
      if (bestScore == 0) {
        evolved
      } else {
        function(evolved, generation + 100)
      }
    }

    val initial = EvolveUtil.startup(Generator(Nop.instructionSize, 128, 2, 1), testCases).shrink.grow(32)
    println(s"Start program selected. length: ${initial.data.length}")
    speedMarkTime = System.nanoTime
    val solution = EvolveUtil.counted(function(initial, 0), 10000, optimise = true, testCases)
    Files.write(Paths.get("solution.dot"), DotGraph(solution).getBytes(StandardCharsets.UTF_8) )

    val optimised = EvolveUtil.counted(solution.shrink, 10000, optimise = true, testCases)
    println( optimised(List(("hello", 0), ("world", 0))).result(optimised.outputCount) )
    Files.write(Paths.get("optimised.dot"), DotGraph(optimised).getBytes(StandardCharsets.UTF_8) )
  }


}
