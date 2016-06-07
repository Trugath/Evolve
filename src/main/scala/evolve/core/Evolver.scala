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

package evolve.core


object Evolver {

  case class EvolverStrategy( children: Int, factor: Double )

  /**
   * Given a program, test cases and a scoring function will attempt to evolve a passed program
   *
   * @param program the program to evolve
   * @param testCases the test cases to run against
   * @param optimise whether we use the execution cost in the evolution
   * @param score the scoring function to check for success
   * @param functions list of functions to map to the program opcodes
   * @tparam A the data type we work against
   * @return A new program that is not worse than the parent
   */
  def apply[A, B]( program: Program, testCases: TestCases[A, B], optimise: Boolean )( implicit strategy: EvolverStrategy, score: (Option[A], Option[B]) => Long, functions: Seq[Function[A]] ): Option[Program] = {
    import scala.concurrent._
    import scala.concurrent.duration._
    import ExecutionContext.Implicits.global
    import scala.language.postfixOps

    val inputCount = testCases.cases.head.inputs.length
    require( testCases.cases.forall( _.inputs.length == inputCount ) )

    // create mutant children
    val popF = Future { program } +: Seq.fill(strategy.children)( Future { Generator.repair( Mutator( program, strategy.factor ) ) } )

    // score the children
    val resultsF = popF.map( _.map( individual => testCases.score( individual ) ) )

    // get children not worse than the parent
    val popResults = (popF zip resultsF).map( { case (a, b) => a.zip(b) } ).map( Await.result( _, 60 seconds ) )
    val childResults = popResults.tail.filter( _._2 <= popResults.head._2 )

    // returns the best child not worse than the parent
    if(optimise) {
      childResults
        .map( a => a.copy( _2 = a._2 + a._1.cost ) )
        .reduceOption[(Program, Long)] {
        case (a, b) => if( a._2 < b._2 ) a else b
      }.map( _._1 )
    } else {
      childResults
        .reduceOption[(Program, Long)] {
        case (a, b) => if( a._2 < b._2 ) a else b
      }.map( _._1 )
    }
  }
}
