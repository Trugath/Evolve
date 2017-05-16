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

package evolve.core

import scala.concurrent.ExecutionContext

object Evolver {

  case class EvolverStrategy( children: Int, factor: Double, optimiseForPipeline: Boolean )

  /**
   * Given a program, test cases and a scoring function will attempt to evolve a passed program
   *
   * @param program the program to evolve
   * @param score the scoring function to check progress
   * @param optimise whether we use the execution cost in the evolution
   * @param functions list of functions to map to the program opcodes
   * @tparam A the data type we work against
   * @return A new program that is not worse than the parent
   */
  def apply[A]( program: Program, score: Program => Double, optimise: Boolean )( implicit strategy: EvolverStrategy, functions: Seq[Function[A]], ec: ExecutionContext ): Option[Program] = {
    apply(program, _ => true, score, optimise)
  }

  /**
    * Given a program, test cases and a scoring function will attempt to evolve a passed program
    *
    * @param program the program to evolve
    * @param instFilter be specific about which instructions to evolve
    * @param score the scoring function to check progress
    * @param optimise whether we use the execution cost in the evolution
    * @param functions list of functions to map to the program opcodes
    * @tparam A the data type we work against
    * @return A new program that is not worse than the parent
    */
  def apply[A]( program: Program, instFilter: Instruction => Boolean, score: Program => Double, optimise: Boolean )( implicit strategy: EvolverStrategy, functions: Seq[Function[A]], ec: ExecutionContext ): Option[Program] = {
    import scala.concurrent._
    import scala.concurrent.duration.Duration._
    import scala.language.postfixOps

    // score the parent
    val programScoreF: Future[Double] = Future {
      score(program)
    }

    // create mutant children
    val popF: Future[Seq[(Program, Double)]] = Future.sequence( Seq.fill(strategy.children)( Future {
      val child = Generator.repair( Mutator( program, strategy.factor ) )
      (child, score(child))
    } ) )

    val programScore = Await.result( programScoreF, Inf )

    // get children not worse than the parent
    val childResults: Seq[(Program, Double)] = blocking {
      Await.result(popF.map( _.filter(_._2 <= programScore) ), Inf)
    }

    val optimiseForPipeline = strategy.optimiseForPipeline

    // returns the best child not worse than the parent
    if(optimise) {
      (( program, programScore ) +: childResults)
        .map( a => a.copy( _2 = (a._2 + a._1.cost) * ( if( optimiseForPipeline ) a._1.maxPipelineLength else 1.0 ) ) )
        .reduceOption[(Program, Double)] {
        case (a, b) => if( a._2 < b._2 ) a else b
      }.map( _._1 ).filterNot( _ == program )
    } else if(optimiseForPipeline) {
      childResults
        .map( a => a.copy( _2 = a._2 * a._1.maxPipelineLength ) )
        .reduceOption[(Program, Double)] {
        case (a, b) => if( a._2 < b._2 ) a else b
      }.map( _._1 )
    } else {
      childResults
        .reduceOption[(Program, Double)] {
        case (a, b) => if( a._2 < b._2 ) a else b
      }.map( _._1 )
    }
  }
}
