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
import scala.util.Random

/**
 * Created by Elliot on 08/09/2015.
 */
object DotGraph {

  /**
   * Converts a program into a String which is the dot (https://en.wikipedia.org/wiki/DOT_(graph_description_language)) file representation of the execution graph
   * @param program the program to convert
   * @param functions the functions to use
   * @return the dot file contents
   */
  def apply(program: Program)(implicit functions: Seq[Function[_]]): String = {

    val letters = Random.alphanumeric

    val buffer: StringBuffer = new StringBuffer()

    val names =
      letters
        .grouped(5)
        .map( "n" + _.mkString.toLowerCase )
        .take((program.inputCount + program.data.length + program.outputCount) * 2)
        .toSet
        .take(program.inputCount + program.data.length + program.outputCount)
        .toSeq

    try {

      val used = program.used

      buffer.append("digraph graphname {\r\n")
      buffer.append("rankdir=\"LR\";\r\n")
      buffer.append("subgraph cluster_0 {")
      (0 until program.inputCount).foreach( i => {
        val index = i
        val node = names(i)
        buffer.append(s" $node [label=" + "\"" + s"Input $index" + "\"" + "];\r\n")
      })
      buffer.append("}")
      program
        .data
        .zipWithIndex
        .map { case (inst, index) => (inst, index + program.inputCount) }
        .filter { case (inst, index) => used(index) }
        .foreach { case (inst, index) => {
          val node = names(index)
          val func = functions(inst.instruction(program.instructionSize))
          val label = func.getLabel(inst)
          buffer.append(s" $node [label=" + "\"" + label + "\"" + "];\r\n")
        }
      }
      buffer.append("subgraph cluster_1 {")
      (0 until program.outputCount).foreach( i => {
        val index = i
        val node = names(program.inputCount + program.data.length + index)
        val source = names(program.inputCount + program.data.length - program.outputCount + index)
        buffer.append(s" $node [label=" + "\"" + s"Output $index" + "\"" + "];\r\n")
      })
      buffer.append("}")
      (0 until program.outputCount).foreach( i => {
        val index = i
        val node = names(program.inputCount + program.data.length + index)
        val source = names(program.inputCount + program.data.length - program.outputCount + index)
        buffer.append(s"$source -> $node;\r\n")
      })
      program
        .data
        .zipWithIndex
        .map { case (inst, index) => (inst, index + program.inputCount) }
        .filter { case (inst, index) => used(index) }
        .reverse
        .foreach {
        case (inst, index) => {
          val instructionSize = program.instructionSize
          val func = inst.instruction( instructionSize )
          val argumentSize = func.argumentSize
          (func.arguments: @switch) match {
            case 0 =>
            case 1 =>
              val a = inst.pointer(instructionSize, argumentSize)
              buffer.append(s"${names(a)} -> ${names(index)};\r\n")
            case 2 =>
              val a = inst.pointer(instructionSize, argumentSize)
              val b = inst.pointer(instructionSize + argumentSize, argumentSize)
              buffer.append(s"${names(a)} -> ${names(index)};\r\n")
              buffer.append(s"${names(b)} -> ${names(index)};\r\n")
            case 3 =>
              val a = inst.pointer(instructionSize, argumentSize)
              val b = inst.pointer(instructionSize + argumentSize, argumentSize)
              val c = inst.pointer(instructionSize + argumentSize + argumentSize, argumentSize)
              buffer.append(s"${names(a)} -> ${names(index)};\r\n")
              buffer.append(s"${names(b)} -> ${names(index)};\r\n")
              buffer.append(s"${names(c)} -> ${names(index)};\r\n")
            case _ => throw new IllegalArgumentException
          }
        }
      }

      buffer.append("}")
      buffer.toString
      } catch {
      case _: Exception => ""
    }
  }
}
