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

import scala.annotation.switch
import scala.concurrent.forkjoin.ThreadLocalRandom

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
   * @tparam A the data type the program will manipulate
   * @return the freshly baked program
   */
  def apply[A](instructionSize: Int, size: Int, inputCount: Int, outputCount: Int)(implicit functions: Seq[Function[A]]): Program = {
    val data = (0 until size).map { index =>
      val inst = ThreadLocalRandom.current().nextInt(functions.length)
      val func = functions(inst)
      def randomWire: Int = {
        ThreadLocalRandom.current().nextInt( inputCount + index )
      }

      require(func.instructionSize + func.argumentSize * func.arguments <= 32, "Need enough bits to pack operator and arguments")
      (func.arguments: @switch) match {
        case 0 =>
          Instruction(0)
            .instruction(inst, func.instructionSize)
        case 1 =>
          Instruction(0)
            .instruction(inst, func.instructionSize)
            .pointer( randomWire, func.instructionSize, func.argumentSize )
        case 2 =>
          Instruction(0)
            .instruction(inst, func.instructionSize)
            .pointer( randomWire, func.instructionSize, func.argumentSize )
            .pointer( randomWire, func.instructionSize + func.argumentSize, func.argumentSize )
        case 3 =>
          Instruction(0)
            .instruction(inst, func.instructionSize)
            .pointer( randomWire, func.instructionSize, func.argumentSize )
            .pointer( randomWire, func.instructionSize + func.argumentSize, func.argumentSize )
            .pointer( randomWire, func.instructionSize + func.argumentSize + func.argumentSize, func.argumentSize )
        case _ => throw new IllegalArgumentException
      }
    }

    Program(instructionSize, data, inputCount, outputCount)
  }

  /**
   * When given a program will adjust the opcode and arguments in each instruction to make it able to execute without crashing
   * @param program the program to repair
   * @param functions list of functions to map to opcodes
   * @tparam A the data type the program will work against
   * @return the newly repaired program
   */
  def repair[A](program: Program)(implicit functions: Seq[Function[A]]): Program = {

    val instructionSize = program.instructionSize
    val inputCount = program.inputCount
    val outputCount = program.outputCount
    val instructions = program
      .data
      .zipWithIndex
      .map { case (inst, originalIndex) =>
      val index = originalIndex + inputCount
      val operator = inst.instruction(instructionSize) % functions.length
      val func = functions(operator)

      (func.arguments: @switch) match {
        case 0 => inst.instruction(operator, instructionSize)
        case 1 =>
          inst
            .instruction(operator, instructionSize)
            .pointer( inst.pointer(instructionSize, func.argumentSize) % index, instructionSize, func.argumentSize )
        case 2 =>
          inst
            .instruction(operator, instructionSize)
            .pointer( inst.pointer(instructionSize, func.argumentSize) % index, instructionSize, func.argumentSize )
            .pointer( inst.pointer(instructionSize + func.argumentSize, func.argumentSize) % index, instructionSize + func.argumentSize, func.argumentSize )
        case 3 =>
          inst
            .instruction(operator, instructionSize)
            .pointer( inst.pointer(instructionSize, func.argumentSize) % index, instructionSize, func.argumentSize )
            .pointer( inst.pointer(instructionSize + func.argumentSize, func.argumentSize) % index, instructionSize + func.argumentSize, func.argumentSize )
            .pointer( inst.pointer(instructionSize + func.argumentSize + func.argumentSize, func.argumentSize) % index, instructionSize + func.argumentSize + func.argumentSize, func.argumentSize )
        case _ => throw new IllegalArgumentException
      }
    }

    assert( instructions.forall( _.instruction(instructionSize) < functions.length ) )
    Program( program.instructionSize, instructions, inputCount, outputCount)
  }
}
