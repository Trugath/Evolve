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

import evolve.core.Generator
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class ProgramSpec  extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  "Any shrunk program" should "be all used" in {
    forAll(Gen.choose[Int](1, 16), Gen.choose[Int](0, 16), Gen.choose[Int](1, 16)) {
      (size: Int, inputCount: Int, outputCount: Int) => whenever(inputCount + size >= outputCount) {
        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount).shrink
          assert( program.used.drop(inputCount).forall( _ == true ))
        }
        {
          import functions.DoubleFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount).shrink
          assert( program.used.drop(inputCount).forall( _ == true ))
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount).shrink
          assert( program.used.drop(inputCount).forall( _ == true ))
        }
        {
          import functions.StringIntFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount).shrink
          assert( program.used.drop(inputCount).forall( _ == true ))
        }
      }
    }
  }

  it should "function the same as before" in {
    forAll(Gen.choose[Int](1, 16)) {
      (size: Int) => {
        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, size, 1, 1)
          val shrunk = program.shrink
          forAll { a: Boolean =>
            assert( program( List(a) ).result(1) === shrunk( List(a) ).result(1) )
          }
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, 1, 1)
          val shrunk = program.shrink
          forAll { a: Int =>
            assert( program( List(a) ).result(1) === shrunk( List(a) ).result(1) )
          }
        }
      }
    }
  }

  "Any shrunk program grown then re-shrunk" should "match itself" in {
    forAll(Gen.choose[Int](1, 16), Gen.choose[Int](0, 16), Gen.choose[Int](1, 16)) {
      (size: Int, inputCount: Int, outputCount: Int) => whenever( inputCount + size >= outputCount ) {
        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount).shrink
          val grown = program.grow(size*2).shrink
          assert( program.used === grown.used )
          assert( program === grown )
        }
        {
          import functions.DoubleFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount).shrink
          val grown = program.grow(size*2).shrink
          assert( program.used === grown.used )
          assert( program === grown )
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount).shrink
          val grown = program.grow(size*2).shrink
          assert( program.used === grown.used )
          assert( program === grown )
        }
        {
          import functions.StringIntFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount).shrink
          val grown = program.grow(size*2).shrink
          assert( program.used === grown.used )
          assert( program === grown )
        }
      }
    }
  }
}