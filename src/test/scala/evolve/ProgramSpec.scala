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

import evolve.core._
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
          assert( program.cost === grown.cost )
          assert( program === grown.shrink )
        }
        {
          import functions.DoubleFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          val grown = program.grow(size*2).shrink
          assert( program.used === grown.used )
          assert( program.cost === grown.cost )
          assert( program === grown )
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          val grown = program.grow(size*2).shrink
          assert( program.used === grown.used )
          assert( program.cost === grown.cost )
          assert( program === grown )
        }
      }
    }
  }

  "A Problematic program" should "spread and shrink correctly" in {
    import functions.DoubleFunctions._
    val program = Program(6,Vector(Instruction(134217728), Instruction(0), Instruction(67108864), Instruction(402669569), Instruction(335568898)),0,1)
    val spread = program.spread(10)
    val shrunk = spread.shrink
    assert( program.used === shrunk.used )
    assert( program.cost === shrunk.cost )
    assert( program === shrunk )
  }

  "Any shrunk program spread then re-shrunk" should "match itself" in {
    forAll(Gen.choose[Int](1, 16), Gen.choose[Int](0, 16), Gen.choose[Int](1, 16), Gen.choose[Int](2, 16), Gen.choose[Int](Int.MinValue, Int.MaxValue)) {
      (size: Int, inputCount: Int, _outputCount: Int, multiplier: Int, seed: Int) => whenever( seed != 0 ) {
        val outputCount = math.min(size, _outputCount)

        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          val spread = program.spread(multiplier)
          val shrunk = spread.shrink
          assert( program.used === shrunk.used )
          assert( program.cost === shrunk.cost )
          assert( program === shrunk )
        }
        {
          import functions.DoubleFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          val spread = program.spread(multiplier)
          val shrunk = spread.shrink
          assert( program.used === shrunk.used )
          assert( program.cost === shrunk.cost )
          assert( program === shrunk )
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          val spread = program.spread(multiplier)
          val shrunk = spread.shrink
          assert( program.used === shrunk.used )
          assert( program.cost === shrunk.cost )
          assert( program === shrunk )
        }
      }
    }
  }

  def nopProgram( length: Int )( implicit functions: Seq[Function[_]] ): Program = {
    val f = functions.head
    def gen( index: Int, acc: List[Instruction] ): List[Instruction] = {
      val inst = Instruction(0).pointer( index, f.instructionSize, f.argumentSize )
      if(index == 0) {
        inst :: acc
      } else {
        gen( index - 1, inst :: acc )
      }
    }

    val result = Program( f.instructionSize, gen( length - 1, Nil ), 1, 1 )
    assert( result.data.length === length )
    result
  }

  "A simple NOP program" should "have a pipeline length of one" in {
    import functions.BooleanFunctions._
    val program = nopProgram(1)
    assert( program( List(false) ).result(1) === List( false ))
    assert( program( List(true)  ).result(1) === List( true ) )
    assert( program.maxPipelineLength === 1 )
  }

  "A simple two NOP program" should "have a pipeline length of two" in {
    import functions.BooleanFunctions._
    val program = nopProgram(2)
    assert( program( List(false) ).result(1) === List( false ))
    assert( program( List(true)  ).result(1) === List( true ) )
    assert( program.maxPipelineLength === 2 )
  }

  "Any valid length NOP program" should "have a pipeline length equal its length" in {
    import functions.BooleanFunctions._
    forAll(Gen.choose[Int](1, 4096)) { length =>
      val program = nopProgram(length)
      assert( program( List(false) ).result(1) === List( false ))
      assert( program( List(true)  ).result(1) === List( true ) )
      assert( program.maxPipelineLength === length )
    }
  }

  "A known 4 length pipeline three bit adder" should "show the correct information" in {
    import functions.BooleanFunctions._
    val testCases = TestCases(List(
      TestCase(List(false, false, false), List(false, false)),
      TestCase(List(false, false, true), List(false, true)),
      TestCase(List(false, true, false), List(false, true)),
      TestCase(List(false, true, true), List(true, false)),
      TestCase(List(true, false, false), List(false, true)),
      TestCase(List(true, false, true), List(true, false)),
      TestCase(List(true, true, false), List(true, false)),
      TestCase(List(true, true, true), List(true, true))
    ))

    val program = Program(6,Seq(Instruction(335552514), Instruction(134225922), Instruction(402685952), Instruction(402669572), Instruction(201359365), Instruction(35544), Instruction(268437419), Instruction(402669573), Instruction(201351174), Instruction(201359367), Instruction(201334786), Instruction(268536609), Instruction(201416715), Instruction(6700), Instruction(268550145), Instruction(134283274), Instruction(268542972), Instruction(201334801), Instruction(134324231), Instruction(402759685)),3,2)
    assert( testCases.score(program) === 0 )
    assert( program.cost === 14 )
    assert( program.maxPipelineLength === 4 )
  }

  "A known 3 length pipeline three bit adder" should "show the correct information" in {
    import functions.BooleanFunctions._
    val testCases = TestCases(List(
      TestCase(List(false, false, false), List(false, false)),
      TestCase(List(false, false, true), List(false, true)),
      TestCase(List(false, true, false), List(false, true)),
      TestCase(List(false, true, true), List(true, false)),
      TestCase(List(true, false, false), List(false, true)),
      TestCase(List(true, false, true), List(true, false)),
      TestCase(List(true, true, false), List(true, false)),
      TestCase(List(true, true, true), List(true, true))
    ))

    val program = Program(6,Seq(Instruction(335560706), Instruction(469778432), Instruction(69206018), Instruction(201367557), Instruction(335577091), Instruction(201326593), Instruction(134217729), Instruction(335609859), Instruction(402653186), Instruction(402743306), Instruction(268486579), Instruction(201392132), Instruction(201400322), Instruction(37874), Instruction(201326601), Instruction(201392137), Instruction(402653193), Instruction(402792466), Instruction(201465866), Instruction(134275084), Instruction(402669579), Instruction(469934083), Instruction(90803981), Instruction(67223689), Instruction(402702356), Instruction(134340616), Instruction(402743297)),3,2)
    assert( testCases.score(program) === 0 )
    assert( program.cost === 14 )
    assert( program.maxPipelineLength === 3 )
  }
}
