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

import scala.annotation.{switch, tailrec}
import scala.concurrent.forkjoin.ThreadLocalRandom

/**
 * A program is a list of instructions to execute
 */
case class Program( instructionSize: Int, data: Seq[Instruction], inputCount: Int, outputCount: Int ) {
  require( outputCount > 0, "A program must have an output" )
  require( data.size + inputCount >= outputCount, "A program must be able to produce a result" )

  /**
   * Execute this program
   * @param inputs parameter list
   * @param functions functions to map into the op codes
   * @tparam A The data type to manipulate
   * @return the final memory state of the program
   */
  def apply[A]( inputs: List[A] )( implicit functions: Seq[Function[A]] ): Memory[A] = {
    require( inputs.length == inputCount )
    @tailrec def execute(instructions: List[Instruction], used: List[Boolean], memory: Memory[A]): Memory[A] = instructions match {
      case head :: tail if  used.head   => execute( tail, used.tail, functions( head.instruction( instructionSize ) )( head, memory ) )
      case head :: tail                 => execute( tail, used.tail, memory.append( memory.apply(0) ) )
      case Nil                          => memory
    }
    execute(data.toList, used.toList, Memory(inputs))
  }

  /**
   * Returns the total 'bit' distance between the programs
   * IE: Total number of bits would need to be flipped to get the programs to match
   * @param other Program to compare with
   * @return The bit distance
   */
  def difference( other: Program ): Int = {

    // get the total number of set bits in an integer
    def popCount( i: Int ): Int = {
      val a = i - ((i >> 1) & 0x55555555)
      val b = (a & 0x33333333) + ((a >> 2) & 0x33333333)
      (((b + (b >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24
    }

    val maxLength = math.max(data.length, other.data.length)
    data
      .padTo(maxLength, Instruction(0))
      .zip(other.data.padTo(maxLength, Instruction(0)))
      .map { case (a, b) => popCount(a.value ^ b.value) }
      .sum
  }

  /**
   * Returns a boolean representation showing which instructions are in use in this program
   * @param functions functions to map into the op codes
   * @return the boolean map of used instructions
   */
  def used(implicit functions: Seq[Function[_]]): Seq[Boolean] = {

    val used: Array[Boolean] = Array.fill(inputCount + data.length)(false)

    for( i <- 0 until outputCount ) {
      used( used.length - 1 - i ) = true
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

    used.toSeq
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
   * Uses the usage data to shrink and remap the instructions to a minimal set
   * @param functions list of functions to map the opcodes to
   * @return the shrunk program
   */
  def shrink( implicit functions: Seq[Function[_]] ): Program = {
    val u = used

    val indexSeq = (0 until inputCount)
      .map( a => (a, a)) ++
      (inputCount until data.length + inputCount)
        .filter( a => u( a ) )
        .zipWithIndex
        .map { case (oldIndex, newIndex) => (oldIndex, newIndex + inputCount) }

    val indexMap = indexSeq.toMap

    val shrunkData = data
      .zipWithIndex
      .filter( a => u( a._2 + inputCount ) )
      .map( _._1 )
      .map { inst => {
      val operator = inst.instruction(instructionSize)
      val func = functions(operator)
      (func.arguments: @switch) match {
        case 0 => inst
        case 1 =>
          val a = inst.pointer(instructionSize, func.argumentSize)
          inst
            .pointer( indexMap( a ), instructionSize, func.argumentSize )
        case 2 =>
          val a = inst.pointer(instructionSize, func.argumentSize)
          val b = inst.pointer(instructionSize + func.argumentSize, func.argumentSize)
          inst
            .pointer( indexMap(a), instructionSize, func.argumentSize )
            .pointer( indexMap(b), instructionSize + func.argumentSize, func.argumentSize )
        case 3 =>
          val a = inst.pointer(instructionSize, func.argumentSize)
          val b = inst.pointer(instructionSize + func.argumentSize, func.argumentSize)
          val c = inst.pointer(instructionSize + func.argumentSize + func.argumentSize, func.argumentSize)
          inst
            .pointer( indexMap(a), instructionSize, func.argumentSize )
            .pointer( indexMap(b), instructionSize + func.argumentSize, func.argumentSize )
            .pointer( indexMap(c), instructionSize + func.argumentSize + func.argumentSize, func.argumentSize )
        case _ => throw new IllegalArgumentException
      }
    }}
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
      val indexSeq = (0 until inputCount) ++ (inputCount + growth until data.length + inputCount + growth)
      val grown = Seq.fill(growth)(Instruction(ThreadLocalRandom.current().nextInt())) ++ data.map { inst => {
        val operator = inst.instruction(instructionSize)
        val func = functions(operator)
        (func.arguments: @switch) match {
          case 0 => inst
          case 1 =>
            val a = inst.pointer(instructionSize, func.argumentSize)
            inst
              .pointer( indexSeq( a ), instructionSize, func.argumentSize )
          case 2 =>
            val a = inst.pointer(instructionSize, func.argumentSize)
            val b = inst.pointer(instructionSize + func.argumentSize, func.argumentSize)
            inst
              .pointer( indexSeq(a), instructionSize, func.argumentSize )
              .pointer( indexSeq(b), instructionSize + func.argumentSize, func.argumentSize )
          case 3 =>
            val a = inst.pointer(instructionSize, func.argumentSize)
            val b = inst.pointer(instructionSize + func.argumentSize, func.argumentSize)
            val c = inst.pointer(instructionSize + func.argumentSize + func.argumentSize, func.argumentSize)
            inst
              .pointer( indexSeq(a), instructionSize, func.argumentSize )
              .pointer( indexSeq(b), instructionSize + func.argumentSize, func.argumentSize )
              .pointer( indexSeq(c), instructionSize + func.argumentSize + func.argumentSize, func.argumentSize )
          case _ => throw new IllegalArgumentException
        }
      }}

      assert(grown.length >= size)
      Generator.repair( copy(data = grown) )
    }
  }
}
