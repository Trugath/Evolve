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

import evolve.util.MersenneTwister

import scala.annotation.tailrec
import java.util.concurrent.ThreadLocalRandom

/**
 * Created by Elliot on 07/09/2015.
 */
object Generator {

  /**
   * Generate a randomly initialised valid program
   * @param instructionSize bits per opcode
   * @param size number of instructions
   * @param inputCount number of inputs
   * @param outputCount number of outputs
   * @param functions list of functions to map to opcodes
   * @return the freshly baked program
   */
  def apply(instructionSize: Int, size: Int, inputCount: Int, outputCount: Int, seed: Int = ThreadLocalRandom.current().nextInt())( implicit functions: Seq[Function[_]] ): Program = {

    import scala.language.existentials

    val random = new MersenneTwister(seed)

    val data = (0 until size).map { index =>
      val (inst, func) = if( index + inputCount > 0 ) {
        val inst = random.nextInt(functions.length)
        val func = functions(inst)
        (inst, func)
      } else {
        val startFunctions = functions.filter( _.arguments == 0 )
        val func = startFunctions(random.nextInt(startFunctions.length))
        val inst = functions.indexOf(func)
        (inst, func)
      }

      def randomWire: Int = {
        require( inputCount + index > 0 )
        random.nextInt( inputCount + index )
      }

      // randomly wire the inputs of the instruction into earlier outputs
      @tailrec def wire(arguments: Int, i: Instruction): Instruction = if(arguments > 0) {
        wire(arguments - 1, i.pointer( randomWire, func.instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize ) )
      } else i

      require(func.instructionSize + func.argumentSize * func.arguments <= 32, "Need enough bits to pack operator and arguments")
      wire(func.arguments, Instruction(inst, func.instructionSize))
    }

    Program(instructionSize, data, inputCount, outputCount)
  }

  /**
   * When given a program will adjust the opcode and arguments in each instruction to make it able to execute without crashing
   * @param program the program to repair
   * @param functions list of functions to map to opcodes
   * @return the newly repaired program
   */
  def repair(program: Program)( implicit functions: Seq[Function[_]] ): Program = {

    import scala.language.existentials

    val instructionSize = program.instructionSize
    val inputCount = program.inputCount
    val outputCount = program.outputCount
    val instructions = program
      .data
      .zipWithIndex
      .map { case (inst, originalIndex) =>
      val index = originalIndex + inputCount
      val (operator, func) = if( originalIndex + inputCount > 0 ) {
        val operator = inst.instruction(instructionSize) % functions.length
        val func = functions(operator)
        (operator, func)
      } else {
        val operator = inst.instruction(instructionSize) % functions.length
        val func = functions(operator)
        if(func.arguments == 0) {
          (operator, func)
        } else {
          val startFunctions = functions.filter( _.arguments == 0 )
          val func = startFunctions( inst.instruction(instructionSize) % startFunctions.length )
          val operator = functions.indexOf(func)
          assert( functions(operator) == func )
          assert( func.arguments == 0 )
          (operator, func)
        }
      }

      // ensure that argument inputs are wired into preceding instruction outputs
      @tailrec def repair(arguments: Int, acc: Instruction): Instruction = if(arguments > 0) {
        repair(arguments - 1, acc.pointer( inst.pointer(instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize) % index, instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize ) )
      } else acc
      repair(func.arguments, inst.instruction(operator, instructionSize))
    }

    assert( instructions.forall( _.instruction(instructionSize) < functions.length ) )
    Program( program.instructionSize, instructions, inputCount, outputCount)
  }
}
