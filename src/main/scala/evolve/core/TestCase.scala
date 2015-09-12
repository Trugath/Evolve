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

/**
 * a set of testcases used in evolving/testing a program
 * @param cases the test cases to use
 * @tparam A the datatype used
 */
case class TestCases[A, B](cases: List[TestCase[A, B]]) {

  /**
   * Given a programs result list this will score it against the testcases
   * @param result list of results for each test case
   * @param scoreFunc scoring function
   * @return
   */
  def score( result: List[List[A]] )( implicit scoreFunc: (Option[A], Option[B]) => Long ): Long = {
    assert(result.length == cases.length)
    val total = cases
      .zip(result)
      .map { case (testCase, output) => testCase.score(output) }
      .sum
    assert( total >= 0 )
    total
  }
}

/**
 * Individual test case used in evolving and testing a program
 * @param inputs inputs to be used on the program
 * @param outputs expected outputs
 * @tparam A the data type used
 */
case class TestCase[A, B](inputs: List[A], outputs: List[B]) {

  /**
   * Given a result from a program, score the rest
   * @param results the results from the program
   * @param scoreFunc the scoring function to use
   * @return the final score
   */
  def score( results: List[A] )( implicit scoreFunc: (Option[A], Option[B]) => Long ): Long = {
    val maxLength = math.max( results.length, outputs.length )
    val resultCompare =
      results
        .map( Option(_) )
        .padTo(maxLength, None)
        .zip(
          outputs
            .map( Option(_) )
            .padTo(maxLength, None)
        )

    val totals = resultCompare
      .map( a => scoreFunc( a._1, a._2 ) )
    assert( totals.sum >= 0 )
    totals.sum
  }
}
