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

object Program {

  /**
    * Actually do the pipeline operations here
    * @see Program.pipeline
    */
  @tailrec private def pipelineImpl( program: Program )( implicit functions: Seq[Function[_]] ): Program = {
    val u = program.used
    val p = program.pipelineInfo

    // find first node that has inputs aren't pipelined correctly
    program
      .data
      .zipWithIndex
      .find {
        case (inst, index) if u( index + program.inputCount ) =>
          val func = functions( inst.instruction( program.instructionSize ) )
          val pipeL = p( index + program.inputCount ) - 1
          def inputsPipelined( arguments: Int ): Boolean = if( arguments > 0 ) {
            val index = inst.pointer(program.instructionSize + (func.argumentSize * (arguments - 1)), func.argumentSize)
            if( p( index ) == pipeL ) {
              inputsPipelined( arguments - 1 )
            } else true
          } else false
          inputsPipelined( func.arguments )
        case _ => false
      } match {

      // pipeline an input to this node
      case Some((inst, index)) =>
        val func = functions( inst.instruction( program.instructionSize ) )
        val pipeL = p( index + program.inputCount ) - 1
        @tailrec def pipeline( arguments: Int ): Program = if( arguments > 0 ) {
          val argStart = program.instructionSize + (func.argumentSize * (arguments - 1))
          val input = inst.pointer(argStart, func.argumentSize)
          if( p( input ) == pipeL ) {
            pipeline( arguments - 1 )
          } else {

            // the pipelined instruction
            val pipelined = inst.pointer( index + program.inputCount, argStart, func.argumentSize)

            // do the actual insert then fix all the argument pointers of all subsequent instructions
            val inserted: Seq[Instruction] =
            (program.data.take(index) :+ Program.getNop( input ) :+ pipelined) ++
              program.data.drop(index + 1)
                .map( inst => {
                  adjustArguments( inst, i => if( i >= index + program.inputCount ) {
                    i + 1
                  } else {
                    i
                  })
                })

            assert( inserted.length == program.data.length + 1 )
            program.copy( data = inserted )
          }
        } else throw new RuntimeException()
        pipelineImpl( pipeline( func.arguments ) )

      // instructions are pipelined to outputs
      case None =>

        //check outputs
        val outputsP = p.takeRight( program.outputCount )
        if( outputsP.foldLeft( true ) {
          case (equal, p) => if( p == outputsP.head ) equal else false
        } ) {
          // all done
          program
        } else  {
          // fix outputs
          val shortOutputIndex = outputsP
            .zipWithIndex
            .reduce[(Long, Int)]( {
              case (l, r) => if( l._1 < r._1 ) l else r
            })._2

          val shortOutput = program.data( program.data.length - program.outputCount + shortOutputIndex )

          val insertIndex = program.inputCount + program.data.length - program.outputCount

          // the instruction to insert
          val nop = Program.getNop( shortOutput.pointer( program.instructionSize, functions.head.argumentSize ) )

          val outputs = program.data.takeRight( program.outputCount )
          pipelineImpl( program.copy( data = ( program.data.dropRight(program.outputCount) :+ nop ) ++ outputs.updated(shortOutputIndex, shortOutput.pointer( insertIndex, program.instructionSize, functions.head.argumentSize ) ) ) )
        }
    }
  }

  /**
    * Remaps the arguments of a function to new indexes
    * @param instruction The instruction to modify
    * @param f A function that takes the old argument index and returns the new one
    * @param functions the function list to use for the instruction
    * @return The new instruction
    */
  private def adjustArguments( instruction: Instruction, f: (Int) => (Int) )( implicit functions: Seq[Function[_]] ): Instruction = {
    val operator = instruction.instruction( functions.head.instructionSize )
    val func = functions(operator)
    @tailrec def remap(arguments: Int, i: Instruction): Instruction = if (arguments > 0) {
      val argStart = func.instructionSize + (func.argumentSize * (arguments - 1))
      val index = instruction.pointer(argStart, func.argumentSize)
      remap(arguments - 1, i.pointer(f(index), argStart, func.argumentSize))
    } else i

    remap(func.arguments, instruction)
  }

  /**
    * Returns a new Nop instruction with the argument pointing at the source index
    * @param source The source node to Nop
    * @param functions The list of functions to extract the Nop from
    * @return The new Nop instruction
    */
  private def getNop( source: Int )( implicit functions: Seq[Function[_]] ): Instruction = {
    val nopF = functions.find( _.getLabel(Instruction(0)) == "Nop" ).getOrElse( functions.head)
    adjustArguments( Instruction(0).instruction( functions.indexOf(nopF), nopF.instructionSize ), _ => source )
  }
}

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

  /**
    * Removes all unused bits from a program
    * @param functions functions to map into the op codes
    * @return
    */
  def clean(implicit functions: Seq[Function[_]]): Program = {

    val u = used

    val cleaned = data
      .zipWithIndex
      .map { case (inst, index) =>
        if( u( index + inputCount ) ) {
          val func = functions( inst.instruction( instructionSize ) )
          @tailrec def clean(argument: Int, i: Instruction): Instruction = if(argument > 0) {
            val argStart = func.instructionSize + (func.argumentSize * (argument - 1))
            clean( argument - 1, i.pointer( inst.pointer( argStart, func.argumentSize), argStart, func.argumentSize ) )
          } else i
          clean(func.arguments, Instruction(0).instruction( inst.instruction( instructionSize ), instructionSize ) )
        } else {
          Instruction(0)
        }
    }

    this.copy( data = cleaned )
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
/*
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
*/

      // mark used instructions, only check known used instructions for its arguments
      def stackBased( stack: List[Int] ): Unit = stack match {
        case head :: tail if head >= inputCount =>
          val inst = data( head - inputCount )
          val func = functions( inst.instruction( instructionSize ) )
          val unique: Seq[Int] = for {
            input <- 0 until func.arguments
            pointer: Int = inst.pointer( instructionSize + ( func.argumentSize * input ), func.argumentSize )
            if !used( pointer )
          } yield {
            used(pointer) = true
            pointer
          }
          stackBased( unique.toList ::: tail )

        case head :: tail    =>
          stackBased( tail )

        case Nil          =>
      }
      stackBased( (used.length - outputCount until used.length).toList )

      val res = used.toSeq
      used_memo += ((functions, res))
      res
    }
  }

  private var cost_memo: Map[(Seq[Function[_]]), Long] = Map.empty
  /**
   * Uses the usage data to calculate the total execution cost for this program
   * @param functions functions to map to the opcodes
   * @return the total execution cost
   */
  def cost( implicit functions: Seq[Function[_]] ): Long = {
    if( cost_memo.contains( functions ) ) {
      cost_memo( functions )
    } else {
      val u = used
      val result = data
        .zipWithIndex
        .filter( a => u( a._2 + inputCount ) )
        .map { case (inst, _) => functions( inst.instruction(instructionSize)).cost }
        .sum

      cost_memo += ((functions, result))
      result
    }
  }

  /**
    * Returns the max distance from input at each node
    * @return Vector of distances of all nodes in the program (including inputs)
    */
  def pipelineInfo( implicit functions: Seq[Function[_]] ): Seq[Long] = {
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

    pipeline
  }

  /**
    * Walks all possible data paths through this program and measures the maximum pipeline length.
    * @param functions list of functions to map the opcodes to
    * @return The longest possible pipeline through this program
    */
  def maxPipelineLength( implicit functions: Seq[Function[_]] ): Long = {
    pipelineInfo.max
  }

  /**
    * Insert Nops into the program so that every stage is matched with its previous stages in pipeline delay
    * @param functions the function list to use in pipelining
    * @return newly pipelined function
    */
  def pipeline( implicit functions: Seq[Function[_]] ): Program = {

    val nopF = functions.indexOf( functions.find( _.getLabel(Instruction(0)) == "Nop" ).getOrElse( functions.head ) )

    val inputNopped = data.take( inputCount ).forall( _.instruction( instructionSize ) == nopF )
    val outputNopped = data.takeRight( outputCount ).forall( _.instruction( instructionSize ) == nopF )

    val a = if( inputNopped ) this else this.nopInputs
    val b = if( outputNopped ) a else a.nopOutputs

    // hand off to the implementation
   Program.pipelineImpl( b )
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
        Program.adjustArguments( inst, indexMap(_) )
      }
    assert( shrunkData.length == u.drop(inputCount).count( a => a ) )
    copy( data = shrunkData )
  }

  /**
    * Inserts Nop nodes for each input at the start of the program, rewires instructions to use these
    *
    * @param functions
    * @return
    */
  def nopInputs( implicit functions: Seq[Function[_]] ): Program = {

    @tailrec def inputNops( inputs: Int, acc: List[Instruction] ): Seq[Instruction] = if(inputs > 0) {
      inputNops( inputs - 1, Program.getNop( inputs - 1 ) :: acc )
    } else acc

    copy( data = inputNops( inputCount, Nil ) ++ data.map( inst => {
      Program.adjustArguments( inst, _ + inputCount )
    }) )
  }

  /**
    * Inserts Nop nodes for each output at the end of the program wiring them into the existing outputs
    *
    * @param functions
    * @return
    */
  def nopOutputs( implicit functions: Seq[Function[_]] ): Program = {
    @tailrec def outputNops( outputs: Int, acc: List[Instruction] ): Seq[Instruction] = if(outputs > 0) {
      outputNops( outputs - 1, Program.getNop( inputCount + data.length - outputCount + outputs - 1 ) :: acc )
    } else acc.reverse

    this.copy( data = data ++ outputNops( outputCount, Nil ) )
  }

  /**
    * Inserts a Nop instruction at the indicated instruction shifting all subsequent indexes to point 1 index to the
    * right.
    *
    * @param index the instruction to add an Nop code after
    */
  def insertNop( index: Int )( implicit functions: Seq[Function[_]] ): Program = {
    require( index >= 0 )
    require( index <= data.length - outputCount, "cannot insert into output nodes" )

    // insert then fix all the argument pointers of all subsequent instructions
    val inserted: Seq[Instruction] =
      (data.take(index + 1) :+ Program.getNop( index + inputCount )) ++
        data.drop(index + 1)
          .map( inst => {
            Program.adjustArguments( inst, i => {
              if( i >= index + inputCount ) {
                i + 1
              } else {
                i
              }
            } )
          })

    copy( data = inserted )
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

      val grown = Seq.fill(growth)(Instruction(0)) ++ data.map { inst => {
        Program.adjustArguments( inst, indexSeq(_) )
      }}

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
    val indexes: Seq[Int] = {
      val b = inputCount + (data.length - outputCount) * multiplier
      val c = b + outputCount
      (0 until inputCount) ++
        (inputCount until b by multiplier) ++
        (b until c)
    }
    assert(indexes.length == inputCount + data.length)

    @tailrec def generate(read: Int, write: Int, acc: List[Instruction]): List[Instruction] = if(read < data.length) {
      if(inputCount + write == indexes(inputCount + read)) {
        generate( read + 1, write + 1, Program.adjustArguments( data(read), indexes(_) ) :: acc )
      } else {
        generate( read, write + 1, Instruction(ThreadLocalRandom.current().nextInt()) :: acc )
      }
    } else acc.reverse

    // fix any invalid instructions before returning the program
    Generator.repair( copy( data = generate( 0, 0, Nil ) ) )
  }
}
