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
 * Memory is a write only expanding data structure
 * Currently implemented with a mutable array buffer backing
 */
final class Memory[A:Manifest] private (private [this] val memory: Array[A], private [this] val length: Int ) {

  def apply(index: Int): A = memory( index )

  def append(data: A): Memory[A] = {
    memory(length) = data
    new Memory(memory, length + 1)
  }

  def skip(amount: Int = 1): Memory[A] = {
    require( amount >= 0 )
    if(amount == 0)
      this
    else
      new Memory(memory, length + amount)
  }

  def result(count: Int): Seq[A] = memory.takeRight(count).toSeq

  def release(): Unit = {
  }

  override def toString: String = memory.toString
}

object Memory {

  def apply[A:Manifest](inputs: List[A], expected: Int): Memory[A] = {
    val array = new Array[A]( expected + inputs.length )
    System.arraycopy(inputs.toArray, 0, array, 0, inputs.length)
    new Memory[A]( array, inputs.length )
  }
}