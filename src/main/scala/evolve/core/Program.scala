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

import scala.annotation.{switch, tailrec}
import java.util.concurrent.ThreadLocalRandom

import evolve.core.Memory.ZeroValueMemory

/**
 * A program is a list of instructions to execute
 */
case class Program( instructionSize: Int, data: Seq[Instruction], inputCount: Int, outputCount: Int ) {
  require( outputCount > 0, "A program must have an output" )
  require( data.size >= outputCount, "A program must be able to produce a result" )

  /**
   * Execute this program
   * @param inputs parameter list
   * @param functions functions to map into the op codes
   * @tparam A The data type to manipulate
   * @return the final memory state of the program
   */
  def apply[A]( inputs: List[A] )( implicit functions: Seq[Function[A]], zero: ZeroValueMemory[A] ): Memory[A] = {
    require( inputs.length == inputCount )

    // extracts arguments from memory
    def arguments(func: Function[A], inst: Instruction, memory: Memory[A]): List[A] = {

      @tailrec def extract( index: Int, acc: List[A] ): List[A] = if (index >= 0) {
        extract( index - 1, memory(inst.pointer(instructionSize + func.argumentSize * index, func.argumentSize)) :: acc )
      } else acc

      (func.arguments: @switch) match {
          case 0 => Nil
          case 1 => memory(inst.pointer(instructionSize, func.argumentSize)) :: Nil
          case 2 => memory(inst.pointer(instructionSize, func.argumentSize)) ::
                    memory(inst.pointer(instructionSize + func.argumentSize, func.argumentSize)) :: Nil
          case _ => extract( func.arguments - 1, Nil )
        }
    }

    @tailrec def execute(index: Int, usage: Seq[Boolean], memory: Memory[A]): Memory[A] = if(index < data.length) {
      if(usage(index)) {
        val inst = data(index)
        val func = functions( inst.instruction( instructionSize ) )
        val args = arguments(func, inst, memory)
        execute(index + 1, usage, memory.append( func( inst, args ) ) )
      } else {
        execute(index + 1, usage, memory.append( zero.value ))
      }
    } else memory

    execute(0, used.drop(inputCount), Memory(inputs, data.length))
  }

  /**
   * Returns the total 'bit' distance between the programs
   * IE: Total number of bits would need to be flipped to get the programs to match
   * @param other Program to compare with
   * @return The bit distance
   */
  def difference( other: Program ): Int = {
    val maxLength = math.max(data.length, other.data.length)
    data
      .padTo(maxLength, Instruction(0))
      .zip(other.data.padTo(maxLength, Instruction(0)))
      .map { case (a, b) =>  Integer.bitCount(a.value ^ b.value) }
      .sum
  }


  private var used_memo: Map[(Seq[Function[_]]), Seq[Boolean]] = Map.empty

  /**
   * Returns a boolean representation showing which instructions are in use in this program
   * @param functions functions to map into the op codes
   * @return the boolean map of used instructions
   */
  def used(implicit functions: Seq[Function[_]]): Seq[Boolean] = {

    if(used_memo.contains(functions)) {
      used_memo(functions)
    } else {
      val used: Array[Boolean] = Array.ofDim(inputCount + data.length)

      for( i <- used.length - outputCount until used.length ) {
        used( i ) = true
      }

      for {
        (inst, index) <- data.zipWithIndex.reverse
        func = functions( inst.instruction( instructionSize ) )
        input <- 0 until func.arguments
        pointer = inst.pointer( instructionSize + ( func.argumentSize * input ), func.argumentSize )
      } {
        if( used( index + inputCount ) ) {
          used( pointer ) = true
        }
      }

      val res = used.toSeq
      used_memo += ((functions, res))
      res
    }
  }

  /**
   * Uses the usage data to calculate the total execution cost for this program
   * @param functions functions to map to the opcodes
   * @return the total execution cost
   */
  def cost( implicit functions: Seq[Function[_]] ): Long = {

    val u = used

    data
      .zipWithIndex
      .filter( a => u( a._2 + inputCount ) )
      .map { case (inst, _) => functions( inst.instruction(instructionSize)).cost }
      .sum
  }

  /**
    * Walks all possible datapaths through this program and measures the maximum pipeline length.
    * @param functions list of functions to map the opcodes to
    * @return The longest possible pipeline through this program
    */
  def maxPipelineLength( implicit functions: Seq[Function[_]] ): Long = {

    val u = used
    val pipeline: Array[Long] = Array.ofDim(inputCount + data.length)

    @tailrec def pipelineLength(arguments: Int, i: Instruction, f: Function[_], max: Long): Long = if (arguments >= 0) {
      val pointer = i.pointer( instructionSize + f.argumentSize * arguments, f.argumentSize )
      pipelineLength( arguments - 1, i, f, math.max( max, pipeline(pointer) + 1 ) )
    } else {
      max
    }

    data
      .zipWithIndex
      .filter( a => u( a._2 + inputCount ) )
      .foreach { case (inst, index) =>
        val func = functions( inst.instruction(instructionSize) )
        pipeline(index + inputCount) = pipelineLength( func.arguments - 1, inst, func, 0L )
      }

    pipeline.max
  }

  /**
   * Uses the usage data to shrink and remap the instructions to a minimal set
   * @param functions list of functions to map the opcodes to
   * @return the shrunk program
   */
  def shrink( implicit functions: Seq[Function[_]] ): Program = {
    val u = used

    val indexSeq =
      (0 until inputCount)
        .map( a => (a, a) ) ++
      (inputCount until data.length + inputCount)
        .filter( a => u( a ) )
        .zipWithIndex
        .map { case (oldIndex, newIndex) => (oldIndex, newIndex + inputCount) }

    val indexMap = indexSeq.toMap

    val shrunkData = data
      .zipWithIndex
      .filter( a => u( a._2 + inputCount ) )
      .map { case (inst, _) =>
        val operator = inst.instruction(instructionSize)
        val func = functions(operator)
        @tailrec def rewire(arguments: Int, i: Instruction): Instruction = if(arguments > 0) {
          val index = inst.pointer(instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize)
          rewire(arguments - 1, i.pointer( indexMap(index), func.instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize ) )
        } else i
        rewire(func.arguments, inst)
      }
    assert( shrunkData.length == u.drop(inputCount).count( a => a ) )
    copy( data = shrunkData )
  }

  /**
   * Ensures the program is atleast the requested length.
   * It does this by adding instructions before the existing program
   * @param size the desired minimum length of the program
   * @param functions Functions which map to the instruction opcodes
   * @return The new program
   */
  def grow( size: Int )( implicit functions: Seq[Function[_]] ): Program = {
    if( data.length >= size ) {
      this
    } else {
      val growth = size - data.length
      val indexSeq = (0 until inputCount) ++ ((inputCount + growth) until (data.length + inputCount + growth))
      assert(indexSeq.length == inputCount + data.length)

      val remapped = data.map { inst => {
        val operator = inst.instruction(instructionSize)
        val func = functions(operator)
        @tailrec def remap(arguments: Int, i: Instruction): Instruction = if(arguments > 0) {
          val index = inst.pointer(instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize)
          remap(arguments - 1, i.pointer( indexSeq(index), func.instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize ) )
        } else i
        remap(func.arguments, inst)
      }}

      val grown = Seq.fill(growth)(Instruction(0)) ++ remapped

      assert(grown.length == size)
      copy(data = grown)
    }
  }

  /**
    * Interleaves random instructions into the current program, increasing its size.
    * Program grows by ( program size - output count ) * ( multiplier - 1 ) Instructions.
    * @param multiplier Changes the amount the program grows by.
    * @param functions Functions which map to the instruction opcodes
    * @return The new program
    */
  def spread(multiplier: Int = 2)( implicit functions: Seq[Function[_]] ): Program = {
    val indexes: Array[Int] = {
      val b = inputCount + (data.length - outputCount) * multiplier
      val c = b + outputCount
      ((0 until inputCount) ++
      (inputCount until b by multiplier ) ++
      (b until c)).toArray
    }
    assert(indexes.length == inputCount + data.length)

    @tailrec def go(read: Int, write: Int, acc: List[Instruction]): List[Instruction] = if(read < data.length) {
      if(inputCount + write == indexes(inputCount + read)) {
        val operator = data(read).instruction(instructionSize)
        val func = functions(operator)
        @tailrec def remap(arguments: Int, i: Instruction): Instruction = if(arguments > 0) {
          val index = data(read).pointer(instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize)
          remap(arguments - 1, i.pointer( indexes(index), func.instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize ) )
        } else i
        go( read + 1, write + 1, remap(func.arguments, data(read)) :: acc )
      } else {
        go( read, write + 1, Instruction(ThreadLocalRandom.current().nextInt()) :: acc )
      }
    } else acc.reverse

    // fix any invalid instructions before returning the program
    Generator.repair( copy( data = go( 0, 0, Nil ) ) )
  }
}
