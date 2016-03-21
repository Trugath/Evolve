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

import evolve.core.{Instruction, Program, Generator}
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class ProgramSpec  extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  "Any grown program" should "function the same" in {
    forAll(Gen.choose[Int](1, 64), Gen.choose[Int](1, 64), Gen.choose[Int](Int.MinValue, Int.MaxValue)) {
      (startSize: Int, grownSize: Int, seed: Int) => whenever(seed != 0) {
        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, startSize, 1, 1, seed)
          val grown = program.grow(grownSize)
          forAll { a: Boolean =>
            assert( program( List(a) ).result(1) === grown( List(a) ).result(1) )
          }
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, startSize, 1, 1, seed)
          val grown = program.grow(grownSize)
          forAll { a: Int =>
            assert( program( List(a) ).result(1) === grown( List(a) ).result(1) )
          }
        }
      }
    }
  }

  "Any shrunk program" should "be all used" in {
    forAll(Gen.choose[Int](1, 64), Gen.choose[Int](0, 16), Gen.choose[Int](1, 16), Gen.choose[Int](Int.MinValue, Int.MaxValue)) {
      (size: Int, inputCount: Int, outputCount: Int, seed: Int) => whenever(size >= outputCount && seed != 0) {
        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          assert( program.used.drop(inputCount).forall( _ == true ))
        }
        {
          import functions.DoubleFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          assert( program.used.drop(inputCount).forall( _ == true ))
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          assert( program.used.drop(inputCount).forall( _ == true ))
        }
        {
          import functions.StringIntFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          assert( program.used.drop(inputCount).forall( _ == true ))
        }
      }
    }
  }

  it should "function the same as before" in {

    {
      import functions.BooleanFunctions._
      val a = Program(Nop.instructionSize, Seq(
        Instruction(0)
          .instruction(functions.indexOf(Nop), Nop.instructionSize)
          .pointer(0, Nop.instructionSize, Nop.argumentSize)
      ), 1, 1)
      assert( a.used.forall( a => a ) )
      assert( a === a.shrink )

      // generate a known program
      val b = Generator(Nop.instructionSize, 3, 1, 1, -1)
      assert( b.data.length === 3 )
      assert( b.data(2).instruction(Nop.instructionSize) === functions.indexOf(Not) )
      assert( b.data(2).pointer(And.instructionSize, And.argumentSize) === 1 )
      assert( b.data(2).pointer(And.instructionSize + And.argumentSize, And.argumentSize) === 0 )
      assert( b.data(0).instruction(Nop.instructionSize) === functions.indexOf(XOr) )
      assert( b.data(0).pointer(XOr.instructionSize, XOr.argumentSize) === 0 )
      assert( b.data(0).pointer(XOr.instructionSize + XOr.argumentSize, XOr.argumentSize) === 0 )
      assert( b.used === List(true, true, false, true) )

      // shrink
      val bShrunk = b.shrink
      assert( bShrunk.data.length === 2 )
      assert( bShrunk.data(1).instruction(Nop.instructionSize) === functions.indexOf(Not) )
      assert( bShrunk.data(1).pointer(And.instructionSize, And.argumentSize) === 1 )
      assert( bShrunk.data(1).pointer(And.instructionSize + And.argumentSize, And.argumentSize) === 0 )
      assert( bShrunk.data(0).instruction(Nop.instructionSize) === functions.indexOf(XOr) )
      assert( bShrunk.data(0).pointer(XOr.instructionSize, XOr.argumentSize) === 0 )
      assert( bShrunk.data(0).pointer(XOr.instructionSize + XOr.argumentSize, XOr.argumentSize) === 0 )
      assert( bShrunk.used === List(true, true, true) )

      forAll { a: Boolean =>
        assert( b( List(a) ).result(0) === bShrunk( List(a) ).result(0) )
      }
    }

    forAll(Gen.choose[Int](1, 64), Gen.choose[Int](Int.MinValue, Int.MaxValue)) {
      (size: Int, seed: Int) => whenever(seed != 0) {
        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, size, 1, 1, seed)
          val shrunk = program.shrink
          forAll { a: Boolean =>
            assert( program( List(a) ).result(1) === shrunk( List(a) ).result(1) )
          }
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, 1, 1, seed)
          val shrunk = program.shrink
          forAll { a: Int =>
            assert( program( List(a) ).result(1) === shrunk( List(a) ).result(1) )
          }
        }
      }
    }
  }

  "Any shrunk program grown then re-shrunk" should "match itself" in {
    forAll(Gen.choose[Int](1, 16), Gen.choose[Int](0, 16), Gen.choose[Int](1, 16), Gen.choose[Int](2, 16), Gen.choose[Int](Int.MinValue, Int.MaxValue)) {
      (size: Int, inputCount: Int, _outputCount: Int, multiplier: Int, seed: Int) => whenever( seed != 0 ) {
        val outputCount = math.min(size, _outputCount)

        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          val grown = program.grow(size*2)
          assert( program.used.count( a => a ) === grown.used.count( a => a ) )
          assert( program === grown.shrink )
          val spread = program.spread(multiplier).shrink
          assert( program.used.count( a => a ) === spread.used.count( a => a ) )
          assert( program === spread.shrink )
        }
        {
          import functions.DoubleFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          val grown = program.grow(size*2).shrink
          assert( program.used === grown.used )
          assert( program === grown )
          val spread = program.spread(multiplier).shrink
          assert( program.used === spread.used )
          assert( program === spread )
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          val grown = program.grow(size*2).shrink
          assert( program.used === grown.used )
          assert( program === grown )
          val spread = program.spread(multiplier).shrink
          assert( program.used === spread.used )
          assert( program === spread )
        }
        {
          import functions.StringIntFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          val grown = program.grow(size*2).shrink
          assert( program.used === grown.used )
          assert( program === grown )
          val spread = program.spread(multiplier).shrink
          assert( program.used === spread.used )
          assert( program === spread )
        }
      }
    }
  }
}
