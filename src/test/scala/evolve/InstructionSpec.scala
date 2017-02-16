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

import evolve.core.{Function, Instruction}
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class InstructionSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  implicit val instructionRange: Gen[Instruction] = for {
    x <- Gen.choose[Int](Int.MinValue, Int.MaxValue)
  } yield Instruction(x)

  "The instruction methods of Instruction" should "get and set correctly" in {
    forAll (instructionRange, Gen.choose[Int](Int.MinValue, Int.MaxValue), Gen.choose[Int](0, 32)) { (inst: Instruction, value: Int, length: Int) =>

      val tvalue = if (length == 32)
        value
      else if (length == 0)
        0
      else
        value >>> (32 - length)

      assert( tvalue === inst.instruction( tvalue, length ).instruction( length ) )
    }
  }

  "The const methods of Instruction" should "get and set correctly" in {

    assert( Instruction(0).const(0, 1) == 0 )
    assert( Instruction(0x80000000).const(0, 1) == -1 )

    forAll (instructionRange, Gen.choose[Int](Int.MinValue, Int.MaxValue), Gen.choose[Int](0, 31), Gen.choose[Int](0, 32)) { (inst: Instruction, value: Int, start: Int, _length: Int) =>
      val length = math.min(_length, 32 - start)
      assert(start + length <= 32)
      val tvalue = if (length == 32)
        value
      else if (length == 0)
        0
      else
        value >> (start + (32 - (start + length)))

      assert(tvalue === inst.const(tvalue, start, length).const(start, length))
    }
  }

  "The pointer methods of Instruction" should "get and set correctly" in {
    forAll (instructionRange, Gen.choose[Int](Int.MinValue, Int.MaxValue), Gen.choose[Int](0, 31), Gen.choose[Int](0, 32)) { (inst: Instruction, value: Int, start: Int, _length: Int) =>
        val length = math.min(_length, 32 - start)
        assert(start + length <= 32)
        val tvalue = if (length == 32)
          value
        else if (length == 0)
          0
        else
          value >>> (start + (32 - (start + length)))

        assert(tvalue === inst.pointer(tvalue, start, length).pointer(start, length))
      }
    }

  "Cleaning an instruction" should "not change any of its functional parts" in {

    def checkFunctional( inst: Instruction )(implicit functions: Seq[Function[_]]): Boolean = {
      val cleaned = inst.clean
      val instructionSize = functions.head.instructionSize
      assert( inst.instruction(instructionSize) === cleaned.instruction(instructionSize) )

      val func = inst.function
      val argumentSize = func.argumentSize

      def checkArg( arg: Int ): Boolean = if( arg > 0) {
        val argStart = instructionSize + (func.argumentSize * (arg - 1))
        assert( inst.pointer( argStart, argumentSize ) === cleaned.pointer( argStart, argumentSize ) )
        true
      } else true

      checkArg( func.arguments )
    }

    forAll (instructionRange) { inst =>
    {
      checkFunctional( inst )
    }
    {
      checkFunctional( inst )
    }
    {
      import functions.IntegerFunctions._
      checkFunctional( inst )
    }
    }
  }

  it should "not clean stored constant values" in {

    def checkConstant( inst: Instruction )(implicit functions: Seq[Function[_]]): Boolean = {
      val func = inst.function

      val cleaned = inst.clean
      assert(cleaned.function === func)

      assert( inst.const(func.constantRegionStart, func.constantRegionSize) === cleaned.const(func.constantRegionStart, func.constantRegionSize))
      true
    }

    forAll (instructionRange) { inst =>
    {
      checkConstant( inst )
    }
    {
      checkConstant( inst )
    }
    {
      import functions.IntegerFunctions._
      checkConstant( inst )
    }
    }
  }
}
