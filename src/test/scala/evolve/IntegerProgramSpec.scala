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

package evolve


import evolve.core.{Instruction, Program}
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class IntegerProgramSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  import evolve.functions.IntegerFunctions._

  def ins( index: Int, a: Int, b: Int ): Instruction = {
    val instructionSize = functions(index).instructionSize
    val argumentSize = functions(index).argumentSize
    
    val ins = Instruction(0)
      .instruction( index, instructionSize )
      .pointer(a, instructionSize, argumentSize)
      .pointer(b, instructionSize + argumentSize, argumentSize)

    assert( ins.instruction( instructionSize ) === index )
    assert( ins.pointer( instructionSize, argumentSize ) === a )
    assert( ins.pointer( instructionSize + argumentSize, argumentSize ) === b )
    ins
  }

  def failSafe( f: => Int ): Int = {
    try {
      f
    } catch {
      case e: ArithmeticException => 0
    }
  }

  "the basic functions" should "execute" in {
    val instructionSize = Nop.instructionSize
    val nopL = Program( instructionSize, Seq( ins( functions.indexOf(Nop), 0, 1 ) ), 1, 1 )
    val add = Program( instructionSize, Seq( ins( functions.indexOf(Add), 0, 1 ) ), 2, 1 )
    val sub = Program( instructionSize, Seq( ins( functions.indexOf(Subtract), 0, 1 ) ), 2, 1 )
    val mul = Program( instructionSize, Seq( ins( functions.indexOf(Multiply), 0, 1 ) ), 2, 1 )
    val div = Program( instructionSize, Seq( ins( functions.indexOf(Divide), 0, 1 ) ), 2, 1 )
    val mod = Program( instructionSize, Seq( ins( functions.indexOf(Modulus), 0, 1 ) ), 2, 1 )
    val inc = Program( instructionSize, Seq( ins( functions.indexOf(Increment), 0, 0 ) ), 2, 1 )
    val dec = Program( instructionSize, Seq( ins( functions.indexOf(Decrement), 0, 0 ) ), 2, 1 )
    val and = Program( instructionSize, Seq( ins( functions.indexOf(And), 0, 1 ) ), 2, 1 )
    val or = Program( instructionSize, Seq( ins( functions.indexOf(Or), 0, 1 ) ), 2, 1 )
    val xor = Program( instructionSize, Seq( ins( functions.indexOf(XOr), 0, 1 ) ), 2, 1 )
    val not = Program( instructionSize, Seq( ins( functions.indexOf(Not), 0, 0 ) ), 1, 1 )
    val shl = Program( instructionSize, Seq( ins( functions.indexOf(ShiftLeft), 0, 1 ) ), 2, 1 )
    val shsr = Program( instructionSize, Seq( ins( functions.indexOf(ShiftSignedRight), 0, 1 ) ), 2, 1 )
    val shur = Program( instructionSize, Seq( ins( functions.indexOf(ShiftUnsignedRight), 0, 1 ) ), 2, 1 )
    val max = Program( instructionSize, Seq( ins( functions.indexOf(Max), 0, 1 ) ), 2, 1 )
    val min = Program( instructionSize, Seq( ins( functions.indexOf(Min), 0, 1 ) ), 2, 1 )

    forAll { (a: Int, b: Int) =>
      assert( nopL(List(a)).result(1).head === a )
      assert( add(List(a, b)).result(1).head === a + b )
      assert( sub(List(a, b)).result(1).head === a - b )
      assert( mul(List(a, b)).result(1).head === a * b )
      assert( div(List(a, b)).result(1).head === failSafe(a / b) )
      assert( mod(List(a, b)).result(1).head === failSafe(a % b) )
      assert( inc(List(a, b)).result(1).head === a+1 )
      assert( dec(List(a, b)).result(1).head === a-1 )
      assert( and(List(a, b)).result(1).head === (a&b) )
      assert( or(List(a, b)).result(1).head === (a|b) )
      assert( xor(List(a, b)).result(1).head === (a^b) )
      assert( not(List(a)).result(1).head === (~a) )
      assert( shl(List(a, b)).result(1).head === (a << b) )
      assert( shsr(List(a, b)).result(1).head === (a >> b) )
      assert( shur(List(a, b)).result(1).head === (a >>> b) )
      assert( max(List(a, b)).result(1).head === math.max(a, b) )
      assert( min(List(a, b)).result(1).head === math.min(a, b) )
    }
  }
}
