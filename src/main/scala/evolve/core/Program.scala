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

import scala.annotation.tailrec
import scala.collection.immutable.ArraySeq

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

            assert( inserted.lengthCompare(program.length + 1) == 0 )
            program.copy( data = inserted, length = program.length + 1 )
          }
        } else throw new RuntimeException()
        pipelineImpl( pipeline( func.arguments ) )

      // instructions are pipelined to outputs
      case None =>

        //check outputs
        val outputsP = p.takeRight( program.outputCount )
        if( outputsP.foldLeft( true ) {
          case (equal, q) => if( q == outputsP.head ) equal else false
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

          val shortOutput = program.data( program.length - program.outputCount + shortOutputIndex )

          val insertIndex = program.inputCount + program.length - program.outputCount

          // the instruction to insert
          val nop = Program.getNop( shortOutput.pointer( program.instructionSize, functions.head.argumentSize ) )

          val outputs =
            program
              .data
              .takeRight( program.outputCount )
              .updated( shortOutputIndex, shortOutput.pointer( insertIndex, program.instructionSize, functions.head.argumentSize ) )

          pipelineImpl( program.copy( data = ( program.data.dropRight(program.outputCount) :+ nop ) ++ outputs, length = program.length + 1 ) )
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
  def adjustArguments( instruction: Instruction, f: Int => Int )( implicit functions: Seq[Function[_]] ): Instruction = {
    val operator = instruction.instruction( functions.head.instructionSize )
    val func = functions(operator)
    @tailrec def remap(arguments: Int, i: Instruction): Instruction = if (arguments > 0) {
      val argStart = func.instructionSize + (func.argumentSize * (arguments - 1))
      val index = instruction.pointer(argStart, func.argumentSize)
      remap(arguments - 1, i.pointer(f(index), argStart, func.argumentSize))
    } else i

    remap(func.arguments, instruction)
  }

  private var nop_memo: Map[Seq[Function[_]], Function[_]] = Map.empty

  /**
    * Returns a new Nop instruction with the argument pointing at the source index
    * @param source The source node to Nop
    * @param functions The list of functions to extract the Nop from
    * @return The new Nop instruction
    */
  private def getNop( source: Int )( implicit functions: Seq[Function[_]] ): Instruction = {
    val nopF = getNopF
    adjustArguments( Instruction(0).instruction( functions.indexOf(nopF), nopF.instructionSize ), _ => source )
  }

  /**
    * Retrieves the current Nop instruction if it exists
    * @param functions list of function to extract nop from
    * @return the nop function
    */
  private def getNopF( implicit functions: Seq[Function[_]] ): Function[_] = {
    if( nop_memo.contains( functions ) ) {
      nop_memo( functions )
    } else {
      val nopF: Function[_] = functions.find( _.getLabel(Instruction(0)) == "Nop" ).getOrElse( functions.head )
      nop_memo += (( functions, nopF ))
      nopF
    }
  }
}

/**
 * A program is a list of instructions to execute
 */
final case class Program( instructionSize: Int, data: Seq[Instruction], inputCount: Int, outputCount: Int, length: Int ) {
  require( outputCount > 0, "A program must have an output" )
  require( length >= outputCount, "A program must be able to produce a result" )
  require( data.lengthCompare(length) == 0 )

  /**
    * Execute this program
    * @param inputs parameter list
    * @param default the default memory cell state (usually zero or one for numbers)
    * @param functions functions to map into the op codes
    * @tparam A The data type to manipulate
    * @return the final memory state of the program
    */
  def apply[A:Manifest]( inputs: List[A], default: A )( implicit functions: Seq[Function[A]] ): (Memory[A], List[A]) = {
    apply( inputs, List.fill(length)(default) )
  }

  /**
   * Execute this program
   * @param inputs parameter list
   * @param state the initial memory state of the program
   * @param functions functions to map into the op codes
   * @tparam A The data type to manipulate
   * @return the final memory state of the program
   */
  def apply[A:Manifest]( inputs: List[A], state: List[A] )( implicit functions: Seq[Function[A]] ): (Memory[A], List[A]) = {
    require( inputs.lengthCompare(inputCount) == 0 )
    require( state.lengthCompare(length) == 0 )

    // extracts arguments from memory
    def arguments( func: Function[A], inst: Instruction, memory: Memory[A] ): List[A] = {

      @tailrec def extract( index: Int, acc: List[A] ): List[A] = if (index >= 0) {
        extract( index - 1, memory(inst.pointer(instructionSize + func.argumentSize * index, func.argumentSize)) :: acc )
      } else acc

      func.arguments match {
          case 0 => Nil
          case 1 => memory( inst.pointer(instructionSize, func.argumentSize) ) :: Nil
          case 2 => memory( inst.pointer(instructionSize, func.argumentSize) ) ::
                    memory( inst.pointer(instructionSize + func.argumentSize, func.argumentSize) ) :: Nil
          case _ => extract( func.arguments - 1, Nil )
        }
    }

    @tailrec def execute(index: Int, skip: Int, states: List[A], usage: Seq[Boolean], memory: Memory[A], acc: List[A]): (Memory[A], List[A]) = if(index < length) {
      if(usage(index+inputCount)) {
        val inst = data(index)
        val func = functions( inst.instruction( instructionSize ) )
        val res = func( inst, states.head, arguments(func, inst, memory) )
        execute(index + 1, 0, states.tail, usage, memory.skip(skip).append( res._2 ), res._1 :: acc )
      } else {
        execute(index + 1, skip + 1, states.tail, usage, memory, states.head :: acc)
      }
    } else (memory, acc.reverse)

    execute(0, 0, state, used, Memory(inputs, length), Nil)
  }

  /**
   * Returns the total 'bit' distance between the programs
   * IE: Total number of bits would need to be flipped to get the programs to match
   * @param other Program to compare with
   * @return The bit distance
   */
  def difference( other: Program ): Int = {
    val maxLength = math.max(length, other.length)
    data
      .padTo(maxLength, Instruction(0))
      .zip(other.data.padTo(maxLength, Instruction(0)))
      .map { case (a, b) => Integer.bitCount(a.value ^ b.value) }
      .sum
  }

  /**
    * Sets all un-used bits in a program to zero
    * @param functions functions to map into the op codes
    * @return
    */
  def clean(implicit functions: Seq[Function[_]]): Program = {

    val u = used

    val cleaned = data
      .zipWithIndex
      .map { case (inst, index) =>
        if( u( index + inputCount ) ) {
          inst.clean
        } else {
          Instruction(0)
        }
    }

    this.copy( data = cleaned )
  }

  private var used_memo: Map[Seq[Function[_]], Seq[Boolean]] = Map.empty

  /**
   * Returns a boolean representation showing which instructions are in use in this program
   * @param functions functions to map into the op codes
   * @return the boolean map of used instructions
   */
  def used(implicit functions: Seq[Function[_]]): Seq[Boolean] = {

    if(used_memo.contains(functions)) {
      used_memo(functions)
    } else {
      val used: Array[Boolean] = Array.ofDim(inputCount + length)

      for( i <- used.length - outputCount until used.length ) {
        used( i ) = true
      }

      // mark used instructions, only check known used instructions for its arguments
      @tailrec def stackBased( stack: List[Int] ): Unit = {
        if(stack.nonEmpty) {
          val head = stack.head
          if (head >= inputCount) {
            val inst = data( head - inputCount )
            val func = inst.function

            @tailrec def inner( arg: Int, tail: List[Int] ): List[Int] = if( arg < func.arguments) {
              val pointer = inst.pointer( instructionSize + ( func.argumentSize * arg ), func.argumentSize )
              if(!used( pointer )) {
                used(pointer) = true
                inner( arg + 1, pointer :: tail )
              } else {
                inner( arg + 1, tail )
              }
            } else tail

            stackBased( inner(0, stack.tail) )
          } else {
            stackBased( stack.tail )
          }
        }
      }

      stackBased( (used.length - outputCount until used.length).toList )

      val res = used.toSeq
      used_memo += ((functions, res))
      res
    }
  }

  private var cost_memo: Map[Seq[Function[_]], Long] = Map.empty
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
    val pipeline: Array[Long] = Array.ofDim(inputCount + length)

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

    ArraySeq.unsafeWrapArray(pipeline)
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
   Program.pipelineImpl( b ).unNopInputs.unNopOutputs
  }

  /**
    * Collapses duplicate nodes into each other
    * @param functions list of functions to use for comparasons
    * @return deduplicated program
    */
  @tailrec
  def deduplicate( implicit functions: Seq[Function[_]] ): Program = {
    val u = used

    // rewire all duplcate nodes to the first duplicate node
    var deduplicated = false
    val duplicates: Array[Int] = Array.ofDim(length + inputCount)
    ( 0 until inputCount ).foreach( i => duplicates(i) = i )
    ( inputCount until length + inputCount - outputCount ).foreach { index =>
      if( u( index ) && duplicates(index) == 0 ) {
        duplicates(index) = index
        ( index + 1 until length + inputCount - outputCount ).foreach { inner =>
          if( u( inner ) && data( index - inputCount ).clean == data( inner - inputCount ).clean ) {
            deduplicated = true
            duplicates( inner ) = index
          }
        }
      }
    }

    val result = copy( data = data.map( Program.adjustArguments(_, duplicates(_) ) ) )
    if( deduplicated ) {
      result.deduplicate
    } else {
      result
    }
  }

  /**
    * Removes as many nops as possible from a programs data path. Either rerouting data paths around them or overiding them with source
    * @param functions list of functions to map the opcodes to
    * @return denopped program
    */
  def denop( implicit functions: Seq[Function[_]] ): Program = {
    val nopF = Program.getNopF
    val nopOp = functions.indexOf( nopF )

    // new nodes rewired to route data passed nops
    val remapped: Seq[Instruction] = {
      // given the index of a node, if the node is a NOP find the first non-NOP its data comes from and return its pointer
      @tailrec def deref( arg: Int ): Int = {
        val op = data( arg ).instruction( instructionSize )
        if( op == nopOp ) {
          val res = data( arg ).pointer( instructionSize, nopF.argumentSize )
          if( res >= inputCount ) {
            deref(res - inputCount)
          } else {
            res
          }
        } else {
          arg + inputCount
        }
      }

      val remap = (0 until inputCount) ++ data.indices.map { deref }
      data.map( Program.adjustArguments( _, remap(_) ) )
    }

    // as output nops cant be bypassed, replace them with their source node
    copy( data = remapped.dropRight( outputCount ) ++ remapped.takeRight( outputCount ).map { inst =>
      if( inst.instruction( instructionSize ) == nopOp ) {
        val p = inst.pointer( instructionSize, nopF.argumentSize )
        if( p >= inputCount )
          remapped( p - inputCount )
        else
          inst
      } else {
        inst
      }
    } )
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
        (inputCount until length + inputCount)
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
    assert( shrunkData.lengthCompare(u.drop(inputCount).count(a => a)) == 0 )

    copy( data = shrunkData, length = shrunkData.length )
  }

  /**
    * Inserts Nop nodes for each input at the start of the program, rewires instructions to use these
    *
    * @param functions list of functions to map the opcodes to
    * @return
    */
  def nopInputs( implicit functions: Seq[Function[_]] ): Program = {

    @tailrec def inputNops( inputs: Int, acc: List[Instruction] ): Seq[Instruction] = if(inputs > 0) {
      inputNops( inputs - 1, Program.getNop( inputs - 1 ) :: acc )
    } else acc

    copy( data = inputNops( inputCount, Nil ) ++ data.map( inst => {
      Program.adjustArguments( inst, _ + inputCount )
    }), length = length + inputCount )
  }

  @tailrec
  def unNopInputs( implicit functions: Seq[Function[_]] ): Program = {
    val nopped = (0 until inputCount).forall { index =>
      data(index).clean == Program.getNop( index )
    }

    val can_unnop = data.drop(inputCount).forall((inst: Instruction) => {
      val operator = inst.instruction(functions.head.instructionSize)
      val func = functions(operator)

      @tailrec def can_unnop(arguments: Int): Boolean = if (arguments > 0) {
        val argStart = func.instructionSize + (func.argumentSize * (arguments - 1))
        val index = inst.pointer(argStart, func.argumentSize)
        if (index >= inputCount)
          can_unnop(arguments - 1)
        else
          false
      } else true

      can_unnop(func.arguments)
    })

    if(nopped && can_unnop) {
      copy( data = data.drop(inputCount).map( Program.adjustArguments(_, a => a - inputCount ) ), length = length - inputCount ).unNopInputs
    } else {
      this
    }
  }

  /**
    * Inserts Nop nodes for each output at the end of the program wiring them into the existing outputs
    *
    * @param functions list of functions to map the opcodes to
    * @return
    */
  def nopOutputs( implicit functions: Seq[Function[_]] ): Program = {
    @tailrec def outputNops( outputs: Int, acc: List[Instruction] ): Seq[Instruction] = if(outputs > 0) {
      outputNops( outputs - 1, Program.getNop( inputCount + length - outputCount + outputs - 1 ) :: acc )
    } else acc

    this.copy( data = data ++ outputNops( outputCount, Nil ), length = length + outputCount )
  }

  /**
    * If all three outputs are nops. Removes them from the data path by replacing them with their source nodes.
    * Source nodes stay in place but will be removed by a shrink
    * @param functions list of functions to map the opcodes to
    * @return
    */
  @tailrec
  def unNopOutputs( implicit functions: Seq[Function[_]] ): Program = {
    val nopF = Program.getNopF
    if( data.takeRight( outputCount ).forall( inst => inst.function == nopF && inst.pointer( nopF.instructionSize, nopF.argumentSize ) >= inputCount) ) {
      val result = data.dropRight( outputCount ) ++ data.takeRight( outputCount ).map { inst =>
        data( inst.pointer( nopF.instructionSize, nopF.argumentSize ) - inputCount )
      }
      copy( data = result ).unNopOutputs
    } else {
      this
    }
  }

  /**
    * Inserts a Nop instruction at the indicated instruction shifting all subsequent indexes to point 1 index to the
    * right.
    *
    * @param index the instruction to add an Nop code after
    */
  def insertNop( index: Int )( implicit functions: Seq[Function[_]] ): Program = {
    require( index >= 0 )
    require( index <= length - outputCount, "cannot insert into output nodes" )

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

    copy( data = inserted, length = length + 1 )
  }

  /**
   * Ensures the program is atleast the requested length.
   * It does this by adding instructions before the existing program
   * @param size the desired minimum length of the program
   * @param functions Functions which map to the instruction opcodes
   * @return The new program
   */
  def grow( size: Int )( implicit functions: Seq[Function[_]] ): Program = {
    if( length >= size ) {
      this
    } else {
      val growth = size - length
      val indexSeq = (0 until inputCount) ++ ((inputCount + growth) until (length + inputCount + growth))
      assert(indexSeq.length == inputCount + length)

      val grown = Seq.fill(growth)(Instruction(0)) ++ data.map { inst => {
        Program.adjustArguments( inst, indexSeq(_) )
      }}

      assert(grown.lengthCompare(size) == 0)
      copy(data = grown, length = size)
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
      val b = inputCount + (length - outputCount) * multiplier
      val c = b + outputCount
      (0 until inputCount) ++
        (inputCount until b by multiplier) ++
        (b until c)
    }
    assert(indexes.lengthCompare(inputCount + length) == 0)

    @tailrec def generate(read: Int, write: Int, acc: List[Instruction]): List[Instruction] = if(read < length) {
      if(inputCount + write == indexes(inputCount + read)) {
        generate( read + 1, write + 1, Program.adjustArguments( data(read), indexes(_) ) :: acc )
      } else {
        generate( read, write + 1, Instruction(ThreadLocalRandom.current().nextInt()) :: acc )
      }
    } else acc.reverse

    // fix any invalid instructions before returning the program
    val generated = generate( 0, 0, Nil )
    Generator.repair( copy( data = generated, length = generated.length ) )
  }
}
