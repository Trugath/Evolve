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

package evolve

import evolve.core.{Instruction, Program}
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class DoubleProgramSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  import evolve.functions.DoubleFunctions._

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

  def failSafe( f: => Double ): Double = {
    try {
      f
    } catch {
      case _: ArithmeticException => 0
    }
  }

  "the basic functions" should "execute" in {
    val instructionSize = Nop.instructionSize
    val nopL = Program( instructionSize, Seq( ins( functions.indexOf(Nop), 0, 1 ) ), 1, 1, 1 )
    val add = Program( instructionSize, Seq( ins( functions.indexOf(Add), 0, 1 ) ), 2, 1, 1 )
    val sub = Program( instructionSize, Seq( ins( functions.indexOf(Subtract), 0, 1 ) ), 2, 1, 1 )
    val mul = Program( instructionSize, Seq( ins( functions.indexOf(Multiply), 0, 1 ) ), 2, 1, 1 )
    val div = Program( instructionSize, Seq( ins( functions.indexOf(Divide), 0, 1 ) ), 2, 1, 1 )
    val mod = Program( instructionSize, Seq( ins( functions.indexOf(Modulus), 0, 1 ) ), 2, 1, 1 )
    val inc = Program( instructionSize, Seq( ins( functions.indexOf(Increment), 0, 0 ) ), 2, 1, 1 )
    val dec = Program( instructionSize, Seq( ins( functions.indexOf(Decrement), 0, 0 ) ), 2, 1, 1 )
    val max = Program( instructionSize, Seq( ins( functions.indexOf(Max), 0, 1 ) ), 2, 1, 1 )
    val min = Program( instructionSize, Seq( ins( functions.indexOf(Min), 0, 1 ) ), 2, 1, 1 )
    val gtz = Program( instructionSize, Seq( ins( functions.indexOf(GreaterThanZero), 0, 1 ) ), 2, 1, 1 )
    val ltz = Program( instructionSize, Seq( ins( functions.indexOf(LessThanZero), 0, 1 ) ), 2, 1, 1 )
    val sig = Program( instructionSize, Seq( ins( functions.indexOf(Sigmoid), 0, 1 ) ), 1, 1, 1 )
    val natexp = Program( instructionSize, Seq( ins( functions.indexOf(NaturalExp), 0, 1 ) ), 1, 1, 1 )
    val natlog = Program( instructionSize, Seq( ins( functions.indexOf(NaturalLog), 0, 1 ) ), 1, 1, 1 )
    val signum = Program( instructionSize, Seq( ins( functions.indexOf(Signum), 0, 1 ) ), 1, 1, 1 )

    forAll { (a: Double, b: Double ) =>
      assert( nopL(List(a), List(0.0))._1.result(1).head === a )
      assert( add(List(a, b), List(0.0))._1.result(1).head === a + b )
      assert( sub(List(a, b), List(0.0))._1.result(1).head === a - b )
      assert( mul(List(a, b), List(0.0))._1.result(1).head === a * b )
      assert( div(List(a, b), List(0.0))._1.result(1).head === failSafe(a / b) )
      assert( mod(List(a, b), List(0.0))._1.result(1).head === failSafe(a % b) )
      assert( inc(List(a, b), List(0.0))._1.result(1).head === a+1 )
      assert( dec(List(a, b), List(0.0))._1.result(1).head === a-1 )
      assert( max(List(a, b), List(0.0))._1.result(1).head === math.max(a, b) )
      assert( min(List(a, b), List(0.0))._1.result(1).head === math.min(a, b) )
      assert( gtz(List(a, b), List(0.0))._1.result(1).head === (if( a > 0 ) b else 0.0) )
      assert( ltz(List(a, b), List(0.0))._1.result(1).head === (if( a < 0 ) b else 0.0) )
      assert( sig(List(a), List(0.0))._1.result(1).head === 1.0 / (1.0 + math.exp(-a)) )
      assert( natexp(List(a), List(0.0))._1.result(1).head === math.exp(a) )
      if( !math.log(a).isNaN ){
        assert( natlog(List(a), List(0.0))._1.result(1).head === math.log(a) )
      } else {
        assert( natlog(List(a), List(0.0))._1.result(1).head.isNaN )
      }
      assert( signum(List(a), List(0.0))._1.result(1).head === math.signum(a) )
    }
  }
}
