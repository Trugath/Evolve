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

import scala.annotation.tailrec
import scala.reflect.ClassTag

object TestCases {

  import scala.language.implicitConversions

  implicit def testCasesToScore[A]( testCases: TestCases[A] )(implicit scoreFunc: (A, A) => Double, functions: Seq[Function[A]]): Program => Double = {
    (p: Program) => testCases.score(p)
  }
}

/**
 * a set of testcases used in evolving/testing a program
 *
 * @param cases the test cases to use
 * @tparam A the datatype used
 */
case class TestCases[A: Manifest](cases: List[TestCase[A]]) {

  /**
   * Given a program score it against the test cases
   *
   * @param program   The program to score
   * @param scoreFunc scoring function
   * @param functions The functions to map to the programs operators
   * @return the summed score
   */
  def score[S](program: Program)(implicit scoreFunc: (A, A) => Double, functions: Seq[Function[A]]): Double = {

    val total: (List[A], Double) =
      cases.foldLeft((new Array[A](program.length).toList, 0.0: Double)) { case ((state, score), testCase) =>
        val exec = program(testCase.inputs, state)
        (exec._2, score + testCase.score(exec._1.result(program.outputCount)))
      }

      assert( total._2 >= 0.0 )
      total._2
  }
}

/**
 * Individual test case used in evolving and testing a program
 * @param inputs inputs to be used on the program
 * @param outputs expected outputs
 * @tparam A the data type used
 */
case class TestCase[A](inputs: List[A], outputs: List[A]) {

  private [this] val outputsLength = outputs.length

  /**
   * Given a result from a program, score the rest
   * @param results the results from the program
   * @param scoreFunc the scoring function to use
   * @return the final score
   */
  def score( results: Seq[A] )( implicit scoreFunc: (A, A) => Double ): Double = {

    @tailrec
    def score( index: Int, acc: Double ): Double = if( index < outputsLength ) {
      score( index + 1, acc + scoreFunc( results(index), outputs(index) ) )
    } else acc

    val total = score( 0, 0 )
    if(total < 0) {
      Double.MaxValue
    } else total
  }
}
