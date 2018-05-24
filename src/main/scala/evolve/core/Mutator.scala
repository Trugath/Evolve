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

import java.util.concurrent.ThreadLocalRandom

import scala.annotation.{switch, tailrec}

object Mutator {

  /**
    * Given a program and a mutation factor this function mutates the function by the set amount
    * the factor 'should' be between 0.0 and 1.0 non-inclusive
    *
    * random bits mutated = number of program bits * 0.75 * factor
    * random instructions mutated = number of program instructions * 0.25 * factor
    *
    * @param program the program to mutate
    * @param factor the amount of bits to mutate
    * @return the newly mutated program
    */
  def apply( program: Program, factor: Double ): Program = {
    apply( program, _ => true, factor )
  }

  /**
   * Given a program and a mutation factor this function mutates the function by the set amount
   * the factor 'should' be between 0.0 and 1.0 non-inclusive
   *
   * random bits mutated = number of program bits * 0.75 * factor
   * random instructions mutated = number of program instructions * 0.25 * factor
   *
   * @param program the program to mutate
   * @param instFilter be specific about which instructions to mutate
   * @param factor the amount of bits to mutate
   * @return the newly mutated program
   */
  def apply( program: Program, instFilter: Instruction => Boolean, factor: Double ): Program = {

    /**
     * flips, 'adds' or 'subtracts' a specified bit in the instruction
     */
    def mutateBit(bit: Int, instr: Array[Instruction]): Unit = {
      val index = bit / 32
      if(instFilter(instr(index))) {
        val old = instr(index).value
        val mask = 0x80000000 >>> (bit % 32)
        (ThreadLocalRandom.current().nextInt(3): @switch) match {
          case 0 => instr(index) = Instruction(old ^ mask)
          case 1 => instr(index) = Instruction(old + mask)
          case 2 => instr(index) = Instruction(old - mask)
        }
      }
    }

    def mutateInstr(index: Int, instr: Array[Instruction]): Unit = if (instFilter(instr(index))) {
      instr(index) = Instruction( ThreadLocalRandom.current().nextInt() )
    }

    val totalBits = program.length * 32
    val mutatedBits = (totalBits * factor * 0.75).ceil.toInt
    val totalInstructions = program.length
    val mutatedInstructions = (totalInstructions * factor * 0.25).ceil.toInt
    val array = program.data.toArray

    @tailrec def go1(count: Int): Unit =  if(count > 0) {
      mutateBit( ThreadLocalRandom.current().nextInt( totalBits ), array )
      go1(count - 1)
    }
    go1(mutatedBits)

    @tailrec def go2(count: Int): Unit =  if(count > 0) {
      mutateInstr( ThreadLocalRandom.current().nextInt( totalInstructions ), array )
      go2(count - 1)
    }
    go2(mutatedInstructions)

    program.copy(
      data = array
    )
  }
}
