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

/**
 * Function takes instruction, state and arguments and returns the new state and the result
 */
trait Function[A] {
  require( instructionSize + argumentSize * arguments <= 32, "Instruction must fit into 32 bits" )

  // bits used by the opcode
  val instructionSize: Int = 6

  // number of arguments
  val arguments: Int = 2

  // bits per argument
  val argumentSize: Int = 13

  // If this instruction stores a constant this is its bit location
  val constantRegionStart: Int = instructionSize

  // bits allocated to the constant
  val constantRegionSize: Int = 0

  // does this function use its state memory
  val usesState = false

  // execution cost
  val cost: Int

  // label for printing/graphing
  def getLabel(inst: Instruction): String

  // are the inputs order dependant
  def ordered: Boolean = false

  // the function itself
  def apply(inst: Instruction, state: A, arguments: List[A]): (A, A) = {
    (state, apply(inst, arguments))
  }

  // the function itself
  def apply(inst: Instruction, arguments: List[A]): A = {
    throw new RuntimeException("Function with state called as stateless")
  }
}
