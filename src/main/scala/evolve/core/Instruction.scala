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
 * 32 bit value containing the function index and argument indexes
 */
case class Instruction( value: Int ) extends AnyVal {

  // extracts the instruction
  def instruction(bits: Int): Int = {
    require(bits >= 0 && bits <= 32, "Instructions are only 32 bits")
    if (bits == 0) {
      0
    } else if( bits == 32 ) {
      value
    } else {
      value >>> (32 - bits)
    }
  }

  // sets the instruction
  def instruction( inst: Int, bits: Int ): Instruction = {
    require( bits >= 0 && bits <= 32, "Instructions are only 32 bits" )
    if( bits == 0 ) {
      this
    } else if( bits == 32 ) {
      Instruction(inst)
    } else {
      require(inst >= 0 && inst <= ( 0xffffffff >>> (32 - bits) ), s"the value($inst) should fit in its bits($bits)")
      Instruction(
        (value & (0xffffffff >>> bits)) | (inst << (32 - bits))
      )
    }
  }

  // extracts the const value
  def const(start: Int, length: Int): Int = {
    require(start >= 0 && length <= 32, "Instructions are only 32 bits")
    require(start + length <= 32)
    if (length == 0) {
      0
    } else if( length == 32 ) {
      value
    } else {
      val end = 32 - (start + length)
      ((value << start) >> start) >> end
    }
  }

  // sets a const value
  def const(const: Int, start: Int, length: Int): Instruction = {
    require(start >= 0 && start <= 32)
    require(length >= 0 && length <= 32)
    require(start + length <= 32)
    if(length == 0) {
      this
    } else if(length == 32) {
      Instruction(const)
    } else {
      require(const >= ( Int.MinValue >> (32 - length) ) && const <= ( Int.MaxValue >>> (32 - length) ), s"the value($const) should fit in its bits($length)")
      val end = 32 - (start + length)
      val mask = (((0xffffffff << start) >>> start) >>> end) << end
      Instruction(
        (value & ~mask) | (const << (32 - (start + length)))
      )
    }
  }

  // extracts a pointer value
  def pointer(start: Int, length: Int): Int = {
    require(start >= 0 && start <= 32)
    require(length >= 0 && length <= 32)
    require(start + length <= 32)
    if(length == 0) {
      0
    } else if(length == 32) {
      value
    } else {
      val end = 32 - (start + length)
      val mask = (((0xffffffff << start) >>> start) >>> end) << end
      (((value << start) >>> start) & mask) >>> end
    }
  }

  // set a pointer value
  def pointer(pointer: Int, start: Int, length: Int): Instruction = {
    require(start >= 0 && start <= 32)
    require(length >= 0 && length <= 32)
    require(start + length <= 32)
    if(length == 0) {
      this
    } else if(length == 32) {
      Instruction(pointer)
    } else {
      require(pointer >= 0 && pointer <= ( 0xffffffff >>> (32 - length) ), s"the value($pointer) should fit in its bits($length)")
      val end = 32 - (start + length)
      val mask = (((0xffffffff << start) >>> start) >>> end) << end
      Instruction(
        (value & ~mask) | (pointer << (32 - (start + length)))
      )
    }
  }
}