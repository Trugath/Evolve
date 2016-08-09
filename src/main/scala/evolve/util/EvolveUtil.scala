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

package evolve.util

import evolve.core.Evolver.EvolverStrategy
import evolve.core.{Function, TestCases, Evolver, Program}

import scala.annotation.tailrec

/**
 * Just some pre-canned evolution functions
 */
object EvolveUtil {

  /**
   * Startup the evolution using a wide search strategy ( large population, heavy mutation )
   */
  def startup[A](program: Program, testCases: TestCases[A])( implicit scoreFunc: (Option[A], Option[A]) => Long, functions: Seq[Function[A]] ): Program = {
    @tailrec def evolve(program: Program, iteration: Int, strat: EvolverStrategy): Program = {
      if (iteration <= 0) {
        program
      } else {
        Evolver(program, testCases, optimise = false)(strat, scoreFunc, functions) match {
          case Some(evolved) => evolve(evolved, iteration - 1, strat)
          case None          => evolve(program, iteration, strat.copy( factor = strat.factor * 0.9 ))
        }
      }
    }

    evolve(program, 100, EvolverStrategy(256, 1.0))
  }

  /**
   * Evolve (successfully) a set number of generations, every failed generation reduces the mutation rate by 10% and grows the program by one gene
    * A failed generation is where none of the children had equal or better fitness than the parent. This indicates either a lack of inactive genes
    * or too high a mutation rate
   */
  def counted[A](program: Program, generations: Int, optimise: Boolean, testCases: TestCases[A])( implicit strategy: EvolverStrategy, score: (Option[A], Option[A]) => Long, functions: Seq[Function[A]] ): Program = {
    /**
     * Run the evolution for a fixed number of generations
     */
    @tailrec def evolve(program: Program, generations: Int, strat: EvolverStrategy, optimise: Boolean): Program = {
      if (generations <= 0) {
        program
      } else {
        Evolver(program, testCases, optimise)(strat, score, functions) match {
          case Some(evolved) => evolve(evolved, generations - 1, strat, optimise)
          case None          => evolve(program.grow( program.data.length + 1 ), generations - 1, strat.copy( factor = strat.factor * 0.9 ), optimise)
        }
      }
    }

    evolve(program, generations, strategy, optimise)
  }

  /**
   * Evolve until a set fitness or a generation limit has been reached
   */
  def fitness[A](program: Program, fitness: Long, limit: Long, testCases: TestCases[A], optimise: Boolean = false)( implicit strategy: EvolverStrategy, score: (Option[A], Option[A]) => Long, functions: Seq[Function[A]] ): Program = {

    @tailrec def evolve(program: Program, generation: Long): Program = {
      if(generation >= limit)
        return program

      Evolver(program, testCases, optimise) match {
        case Some(evolved) =>
          val scores = for {
            testCase <- testCases.cases
          } yield testCase.score(evolved(testCase.inputs).result(testCase.outputs.length))
          if (scores.sum <= fitness) {
            evolved
          } else {
            evolve(evolved, generation + 1)
          }

        case None =>
          evolve(program, generation + 1)
      }
    }

    evolve(program, 0)
  }
}
