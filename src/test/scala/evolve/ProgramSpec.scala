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
import evolve.util.ProgramUtil
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

  "A simple NOP program" should "have a pipeline length of one" in {
    import functions.BooleanFunctions._
    val program = ProgramUtil.nopProgramLong(1, 1, 1)
    assert( program( List(false) ).result(1) === List( false ))
    assert( program( List(true)  ).result(1) === List( true ) )
    assert( program.maxPipelineLength === 1 )
  }

  it should "not get smaller when denopped" in {
    import functions.BooleanFunctions._
    val program = ProgramUtil.nopProgramLong(1, 1, 1).denop
    assert( program( List(false) ).result(1) === List( false ))
    assert( program( List(true)  ).result(1) === List( true ) )
    assert( program.maxPipelineLength === 1 )
  }

  "A simple two NOP program" should "have a pipeline length of two" in {
    import functions.BooleanFunctions._
    val program = ProgramUtil.nopProgramLong(2, 1, 1)
    assert( program( List(false) ).result(1) === List( false ))
    assert( program( List(true)  ).result(1) === List( true ) )
    assert( program.maxPipelineLength === 2 )
  }

  it should "shrink into a single nop program when denopped" in {
    import functions.BooleanFunctions._
    val program = ProgramUtil.nopProgramLong(2, 1, 1).denop.shrink
    assert( program( List(false) ).result(1) === List( false ))
    assert( program( List(true)  ).result(1) === List( true ) )
    assert( program.maxPipelineLength === 1 )
  }

  "Any length 'ShortNOP' program" should "have a pipeline length of one" in {
    import functions.BooleanFunctions._
    forAll(Gen.choose[Int](1, 256)) { length =>
      val program = ProgramUtil.nopProgramShort(length, 1, 1)
      assert( program( List(false) ).result(1) === List( false ))
      assert( program( List(true)  ).result(1) === List( true ) )
      assert( program.maxPipelineLength === 1 )
    }
  }

  "Any length 'LongNOP' program" should "have a pipeline length equal its length" in {
    import functions.BooleanFunctions._
    forAll(Gen.choose[Int](1, 256)) { length =>
      val program = ProgramUtil.nopProgramLong(length, 1, 1)
      assert( program( List(false) ).result(1) === List( false ))
      assert( program( List(true)  ).result(1) === List( true ) )
      assert( program.maxPipelineLength === length )
    }
  }

  it should "shrink into a single nop program when denopped" in {
    import functions.BooleanFunctions._
    forAll(Gen.choose[Int](1, 256)) { length =>
      val program = ProgramUtil.nopProgramShort(length, 1, 1).denop
      assert( program( List(false) ).result(1) === List( false ))
      assert( program( List(true)  ).result(1) === List( true ) )
      assert( program.maxPipelineLength === 1 )
    }
  }

  "Nop inserted into a single nop program" should "produce the correct program" in {
    import functions.BooleanFunctions._
    val a = Program(6,List(Instruction(0)),1,1)
    val b = Program(6,List(Instruction(0), Instruction(8192)),1,1)
    assert( a.insertNop(0) === b )
  }

  "Two Nop inserted into a single nop program" should "produce the correct program" in {
    import functions.BooleanFunctions._
    val a = Program(6,List(Instruction(0)),1,1)
    val b = Program(6,List(Instruction(0), Instruction(8192), Instruction(16384)),1,1)
    assert( a.insertNop(0).insertNop(1) === b )
    assert( a.insertNop(0).insertNop(0) === b )
  }

  "A nop inserted in the data region in a clean nop program" should "create a program identical to a nop program of the new length" in {
    import functions.BooleanFunctions._
    forAll( Gen.choose(1, 256) ) { length =>
      val a = ProgramUtil.nopProgramLong(length, 1, 1).clean
      val b = ProgramUtil.nopProgramLong(length + 1, 1, 1).clean
      assert( a( List(false) ).result(1) === b( List(false) ).result(1) )
      assert( a( List(true)  ).result(1) === b( List(true) ).result(1) )

      forAll( Gen.choose(0, a.data.length - 1 ) ) { index =>
        assert( a.insertNop(index) === b )
      }
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

    val program = Program(6,List(Instruction(134225922), Instruction(402677760), Instruction(201351172), Instruction(201334786), Instruction(134266885), Instruction(402702340)),3,2)
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

    val program = Program(6,Seq(Instruction(201326593), Instruction(134217729), Instruction(402653186), Instruction(201359362), Instruction(134266883), Instruction(402694145)),3,2)
    assert( testCases.score(program) === 0 )
    assert( program.cost === 14 )
    assert( program.maxPipelineLength === 3 )
  }

  it should "be able to have Nops inserted at any point and still function" in {
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

    val program = Program(6,Seq(Instruction(201326593), Instruction(134217729), Instruction(402653186), Instruction(201359362), Instruction(134266883), Instruction(402694145)),3,2)
    forAll(Gen.choose(0, program.data.length-2)) { index => {
      val inserted = program.insertNop(index)
      assert( inserted.data.length === program.data.length + 1)
      assert( inserted.cost === 15 )
      assert( testCases.score(inserted) === 0 )
    }}
  }

  it should "be able to have Nops inserted at any point then be denopped and shrunk then return to original and still function" in {
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

    val program = Program(6,Seq(Instruction(201326593), Instruction(134217729), Instruction(402653186), Instruction(201359362), Instruction(134266883), Instruction(402694145)),3,2)
    forAll(Gen.choose(0, program.data.length-2)) { index => {
      val after = program.insertNop(index).denop.shrink
      assert( after.data.length === program.data.length)
      assert( after.cost === 14 )
      assert( testCases.score(after) === 0 )
    }}
  }

  it should "pipeline correctly" in {
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

    val a = Program(6,Seq(Instruction(201326593), Instruction(134217729), Instruction(402653186), Instruction(201359362), Instruction(134266883), Instruction(402694145)),3,2)
    val b = a.pipeline
    assert( b === Program(6,Seq(Instruction(201326593), Instruction(134217729), Instruction(402653186), Instruction(16384), Instruction(201359366), Instruction(24576), Instruction(134275080), Instruction(8192), Instruction(402694154), Instruction(90112), Instruction(134275080), Instruction(90112)),3,2) )
    assert( testCases.score( a ) === 0L )
    assert( testCases.score( b ) === 0L )
  }

  it should "return to original after pipelining if denopped and shrunk" in {
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

    val a = Program(6,Seq(Instruction(201326593), Instruction(134217729), Instruction(402653186), Instruction(201359362), Instruction(134266883), Instruction(402694145)),3,2)
    val b = a.pipeline.denop.shrink

    assert( a === b )
    assert( testCases.score( a ) === 0L )
    assert( testCases.score( b ) === 0L )
  }


  "A known pythagorean program" should "return to original after nopping if denopped" in {
    import evolve.functions.DoubleFunctions._

    object SquareRoot extends Function[Double] {
      override def arguments: Int = 1
      override def cost: Int = 10
      override def getLabel(inst: Instruction): String = "SquareRoot"
      override def apply(inst: Instruction, arguments: List[Double]): Double = {
        val a = arguments.head
        math.sqrt(a)
      }
    }

    implicit val functions = evolve.functions.DoubleFunctions.functions.take(7) :+ SquareRoot

    def answer( a: Double, b: Double ): Double = {
      math.sqrt(a*a+b*b)
    }

    val testCases = TestCases(
      (for{
        a <- 1.0 until 4.0  by 1.0
        b <- a   until 5.0 by 1.0
      } yield TestCase(List(a, b), List(answer(a, b)))).toList
    )

    val a = Program(6,List(Instruction(335552513), Instruction(335544320), Instruction(201342979), Instruction(469800332)),2,1)
    assert( testCases.score( a ) === 0L )

    val b = a.nopInputs.nopOutputs.unNopOutputs.unNopInputs.shrink
    assert( testCases.score( b ) === 0L )

    assert( a === b )
  }
}
