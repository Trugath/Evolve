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

import java.util.concurrent.Executors

import evolve.core.Evolver.EvolverStrategy
import evolve.core._
import evolve.util.ProgramUtil
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

import scala.concurrent.ExecutionContext

class ProgramSpec  extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  private [this] implicit val ec = ExecutionContext.fromExecutor( Executors.newFixedThreadPool( Runtime.getRuntime.availableProcessors() ) )

  "Any grown program" should "function the same" in {
    forAll(Gen.choose[Int](1, 64), Gen.choose[Int](1, 64), Gen.choose[Int](Int.MinValue, Int.MaxValue)) {
      (startSize: Int, grownSize: Int, seed: Int) => whenever(seed != 0) {
        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, startSize, 1, 1, seed)
          val grown = program.grow(grownSize)
          forAll { a: Boolean =>
            assert( program( List(a), List.fill(program.data.length)(false) )._1.result(1) === grown( List(a), List.fill(grown.data.length)(false) )._1.result(1) )
          }
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, startSize, 1, 1, seed)
          val grown = program.grow(grownSize)
          forAll { a: Int =>
            assert( program( List(a), List.fill(program.data.length)(0) )._1.result(1) === grown( List(a), List.fill(grown.data.length)(0) )._1.result(1) )
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
        assert( b( List(a), false )._1.result(0) === bShrunk( List(a), false )._1.result(0) )
      }
    }

    forAll(Gen.choose[Int](1, 64), Gen.choose[Int](Int.MinValue, Int.MaxValue)) {
      (size: Int, seed: Int) => whenever(seed != 0) {
        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, size, 1, 1, seed)
          val shrunk = program.shrink
          forAll { a: Boolean =>
            assert( program( List(a), false )._1.result(1) === shrunk( List(a), false )._1.result(1) )
          }
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, 1, 1, seed)
          val shrunk = program.shrink
          forAll { a: Int =>
            assert( program( List(a), 0 )._1.result(1) === shrunk( List(a), 0 )._1.result(1) )
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
    assert( program( List(false), List(false) )._1.result(1) === List( false ))
    assert( program( List(true), List(false)  )._1.result(1) === List( true ) )
    assert( program.maxPipelineLength === 1 )
  }

  it should "not get smaller when denopped" in {
    import functions.BooleanFunctions._
    val program = ProgramUtil.nopProgramLong(1, 1, 1).denop
    assert( program( List(false), false )._1.result(1) === List( false ))
    assert( program( List(true), false  )._1.result(1) === List( true ) )
    assert( program.maxPipelineLength === 1 )
  }

  "A simple two NOP program" should "have a pipeline length of two" in {
    import functions.BooleanFunctions._
    val program = ProgramUtil.nopProgramLong(2, 1, 1)
    assert( program( List(false), false )._1.result(1) === List( false ))
    assert( program( List(true), false  )._1.result(1) === List( true ) )
    assert( program.maxPipelineLength === 2 )
  }

  it should "shrink into a single nop program when denopped" in {
    import functions.BooleanFunctions._
    val program = ProgramUtil.nopProgramLong(2, 1, 1).denop.shrink
    assert( program( List(false), false )._1.result(1) === List( false ))
    assert( program( List(true), false  )._1.result(1) === List( true ) )
    assert( program.maxPipelineLength === 1 )
  }

  "Any length 'ShortNOP' program" should "have a pipeline length of one" in {
    import functions.BooleanFunctions._
    forAll(Gen.choose[Int](1, 256)) { length =>
      val program = ProgramUtil.nopProgramShort(length, 1, 1)
      assert( program( List(false), false )._1.result(1) === List( false ))
      assert( program( List(true), false  )._1.result(1) === List( true ) )
      assert( program.maxPipelineLength === 1 )
    }
  }

  "Any length 'LongNOP' program" should "have a pipeline length equal its length" in {
    import functions.BooleanFunctions._
    forAll(Gen.choose[Int](1, 256)) { length =>
      val program = ProgramUtil.nopProgramLong(length, 1, 1)
      assert( program( List(false), false )._1.result(1) === List( false ))
      assert( program( List(true), false )._1.result(1) === List( true ) )
      assert( program.maxPipelineLength === length )
    }
  }

  it should "shrink into a single nop program when denopped" in {
    import functions.BooleanFunctions._
    forAll(Gen.choose[Int](1, 256)) { length =>
      val program = ProgramUtil.nopProgramShort(length, 1, 1).denop
      assert( program( List(false), false )._1.result(1) === List( false ))
      assert( program( List(true), false  )._1.result(1) === List( true ) )
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
      assert( a( List(false), false )._1.result(1) === b( List(false), false )._1.result(1) )
      assert( a( List(true), false  )._1.result(1) === b( List(true), false )._1.result(1) )

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

  "A problematic 4 bit full adder" should "deduplicate and clean correctly" in {

    import functions.BooleanFunctions._

    implicit val evolverStrategy = EvolverStrategy(24, 0.0015, optimiseForPipeline = false)
    implicit val ec = ExecutionContext.fromExecutor( Executors.newFixedThreadPool( Runtime.getRuntime.availableProcessors() ) )

    def bitsToBools(value: Int, bits: Int): List[Boolean] = {
      require(value >= 0 && value <= math.pow(2, bits))
      (0 until bits)
        .map(i => ((0x1 << i) & value) != 0x0)
        .reverse
        .toList
    }

    val testCases = TestCases((for {
      l <- 0 until 16
      r <- 0 until 16
    } yield TestCase[Boolean](bitsToBools(l, 4) ::: bitsToBools(r, 4), bitsToBools(l + r, 5))).toList)

    val a = Program(6,Seq(Instruction(469811202), Instruction(68778787), Instruction(134159985), Instruction(469762048), Instruction(133937857), Instruction(201342978), Instruction(335601672), Instruction(134332422), Instruction(402694153), Instruction(402694145), Instruction(134316036), Instruction(335609861), Instruction(335618055), Instruction(132243848), Instruction(469794818), Instruction(134340616), Instruction(268596694), Instruction(102689986), Instruction(268582921), Instruction(335593490), Instruction(201383939), Instruction(84197863), Instruction(469950481), Instruction(402677781), Instruction(54561), Instruction(469843977), Instruction(134216720), Instruction(163918), Instruction(402792470), Instruction(335568910), Instruction(174743), Instruction(268686661), Instruction(470065153), Instruction(201564186), Instruction(201654302), Instruction(470073359), Instruction(101455), Instruction(268653532), Instruction(134496289), Instruction(469934122), Instruction(335593502), Instruction(402898983), Instruction(402759710), Instruction(469762074), Instruction(335642647), Instruction(402710559), Instruction(335700000), Instruction(402776116), Instruction(201637917), Instruction(470196227), Instruction(201457716), Instruction(402653225), Instruction(470032421), Instruction(335970321), Instruction(32798), Instruction(335912990), Instruction(469819413), Instruction(335896630), Instruction(134643732), Instruction(134438915), Instruction(469852174), Instruction(172074), Instruction(268688997), Instruction(335618065), Instruction(470204480), Instruction(201703483), Instruction(268812476), Instruction(146756), Instruction(70620657), Instruction(335904819), Instruction(201695251), Instruction(201482282), Instruction(470106170), Instruction(134193197), Instruction(484552), Instruction(201998416), Instruction(134307865), Instruction(201924616), Instruction(335749122), Instruction(201326669), Instruction(470376494), Instruction(201777208), Instruction(268873432), Instruction(469844049), Instruction(268959781), Instruction(335913043), Instruction(268756594), Instruction(269008399), Instruction(101171233), Instruction(269096352), Instruction(133292034), Instruction(469835861), Instruction(131108941), Instruction(269036356), Instruction(402792480), Instruction(470556723), Instruction(470286342), Instruction(336248856), Instruction(335634466), Instruction(403324940), Instruction(269086813), Instruction(403431475), Instruction(269090853), Instruction(73697812), Instruction(202129427), Instruction(91962624), Instruction(402800743), Instruction(202244103), Instruction(201826336), Instruction(403464192), Instruction(201949214), Instruction(402972768), Instruction(336388124), Instruction(201859131), Instruction(202047508), Instruction(268773705), Instruction(268612379), Instruction(135151719), Instruction(134422605), Instruction(336543755), Instruction(134742119), Instruction(121732455), Instruction(261563), Instruction(201752576), Instruction(336232462), Instruction(470417486), Instruction(134512721), Instruction(134570061), Instruction(67773450), Instruction(201498684), Instruction(269301552), Instruction(470302854), Instruction(202195020), Instruction(336183376), Instruction(1066935), Instruction(201637980), Instruction(403415137), Instruction(403152952), Instruction(85544713), Instruction(269534619), Instruction(1044489), Instruction(469999670), Instruction(202268748), Instruction(268873114), Instruction(134938746), Instruction(336068759), Instruction(269426735), Instruction(134946949), Instruction(469860500), Instruction(403906568), Instruction(434312), Instruction(680887), Instruction(269541501), Instruction(134930515), Instruction(470098034), Instruction(470253598), Instruction(134381725), Instruction(741016)),8,5)
    val b = a.deduplicate
    val c = a.clean
    val d = b.clean
    val e = c.deduplicate

    assert( testCases.score( a ) === 0L )
    assert( testCases.score( b ) === 0L )
    assert( testCases.score( c ) === 0L )
    assert( testCases.score( d ) === 0L )
    assert( testCases.score( e ) === 0L )
  }


  "A known pythagorean program" should "return to original after nopping if denopped" in {
    import evolve.functions.DoubleFunctions._

    object SquareRoot extends Function[Double] {
      override val arguments: Int = 1
      override val cost: Int = 10
      override def getLabel(inst: Instruction): String = "SquareRoot"
      override def apply(inst: Instruction, arguments: List[Double]): Double = {
        val a = arguments.head
        math.sqrt(a)
      }
    }

    implicit val functions = evolve.functions.DoubleFunctions.functions.take(7) :+ SquareRoot

    val testCases = TestCases(
      List(
        TestCase(List(3.0, 4.0), List(5.0)),
        TestCase(List(5.0, 12.0), List(13.0)),
        TestCase(List(8.0, 15.0), List(17.0)),
        TestCase(List(7.0, 24.0), List(25.0)),
        TestCase(List(20.0, 21.0), List(29.0)),
        TestCase(List(12.0, 35.0), List(37.0)),
        TestCase(List(9.0, 40.0), List(41.0)),
        TestCase(List(28.0, 45.0), List(53.0)),
        TestCase(List(11.0, 60.0), List(61.0)),
        TestCase(List(16.0, 63.0), List(65.0)),
        TestCase(List(48.0, 55.0), List(73.0)),
        TestCase(List(13.0, 84.0), List(85.0))
      )
    )

    val a = Program(6,List(Instruction(335552513), Instruction(335544320), Instruction(201342979), Instruction(469794816)),2,1)
    assert( testCases.score( a ) === 0L )

    val b = a.nopInputs.nopOutputs.unNopOutputs.unNopInputs.shrink
    assert( testCases.score( b ) === 0L )
    assert( a === b )

    val c = a.nopInputs.nopOutputs.denop.shrink
    assert( testCases.score( c ) === 0L )
    assert( a === c )
  }
}
