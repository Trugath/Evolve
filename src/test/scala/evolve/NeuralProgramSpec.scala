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

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import java.util.Random
import java.util.concurrent.{Executors, ThreadLocalRandom}

import evolve.core.Evolver.EvolverStrategy
import evolve.core._
import evolve.util.EvolveUtil
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

class NeuralProgramSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  import evolve.functions.NeuralFunctions._

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
    val natlog = Program( instructionSize, Seq( ins( functions.indexOf(NaturalLog), 0, 1 ) ), 1, 1, 1)
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

  "The Delay function" should "function as expected" in {
    val instructionSize = Nop.instructionSize
    val delay = Program( instructionSize, Seq( ins( functions.indexOf(Delay), 0, 1 ) ), 1, 1, 1 )
    forAll { (a: Double, b: Double) =>
      val result = delay(List(a), List(b))
      assert( result._1.result(1).head === b )
      assert( result._2.head === a )
    }
  }

  it should "work as part of a larger program" in {
    val instructionSize = Nop.instructionSize
    val delay = Program( instructionSize, Seq( ins( functions.indexOf(Delay), 0, 0), ins( functions.indexOf(Delay), 1, 0 ), ins( functions.indexOf(Delay), 2, 0 ) ), 1, 1, 3 ).spread()

    forAll { a: Double =>
      val step1 = delay(List(a), List(0.0, 0.0, 0.0, 0.0, 0.0))
      assert( step1._1.result(1).head === 0.0 )
      assert( step1._2 === List(a, 0.0, 0.0, 0.0, 0.0) )

      val step2 = delay(List(0.0), List(a, 0.0, 0.0, 0.0, 0.0))
      assert( step2._1.result(1).head === 0.0 )
      assert( step2._2 === List(0.0, 0.0, a, 0.0, 0.0) )

      val step3 = delay(List(0.0), List(0.0, 0.0, a, 0.0, 0.0))
      assert( step3._1.result(1).head === 0.0 )
      assert( step3._2 === List(0.0, 0.0, 0.0, 0.0, a) )

      val step4 = delay(List(0.0), List(0.0, 0.0, 0.0, 0.0, a))
      assert( step4._1.result(1).head === a )
      assert( step4._2 === List(0.0, 0.0, 0.0, 0.0, 0.0) )
    }
  }

  "The Increasing function" should "function as expected" in {
    val instructionSize = Nop.instructionSize
    val inc = Program( instructionSize, Seq( ins( functions.indexOf(Increasing), 0, 1 ) ), 1, 1, 1 )
    forAll { (a: Double, b: Double) =>
      val result = inc(List(a), List(b))
      assert( result._2.head === a )

      if( a > b ) {
        assert( result._1.result(1).head === 1.0 )
      } else {
        assert( result._1.result(1).head === 0.0 )
      }
    }
  }

  "The Weight function" should "function as expected" in {
    val instructionSize = Nop.instructionSize
    val zeroWeight = Program( instructionSize, Seq( ins( functions.indexOf(Weight), 0, 1 ) ), 1, 1, 1 )
    assert( zeroWeight(List(0.0), 0.0)._1.result(1).head === 0.0 )
    assert( zeroWeight(List(1.0), 0.0)._1.result(1).head === 0.0 )

    val oneWeight = Program( instructionSize, Seq( ins( functions.indexOf(Weight), 0, 1 ).const(Weight.scale.toInt, Weight.constantRegionStart, Weight.constantRegionSize) ), 1, 1, 1 )
    assert( oneWeight(List(0.0), 0.0)._1.result(1).head === 0.0 )
    assert( oneWeight(List(1.0), 0.0)._1.result(1).head === 1.0 )

    forAll { a: Double =>
      val result = zeroWeight(List(a), 0.0)
      assert( result._1.result(1).head === 0.0 )
    }
  }

  "The Decreasing function" should "function as expected" in {
    val instructionSize = Nop.instructionSize
    val dec = Program( instructionSize, Seq( ins( functions.indexOf(Decreasing), 0, 1 ) ), 1, 1, 1 )
    forAll { (a: Double, b: Double) =>
      val result = dec(List(a), List(b))
      assert( result._2.head === a )

      if( a < b ) {
        assert( result._1.result(1).head === 1.0 )
      } else {
        assert( result._1.result(1).head === 0.0 )
      }
    }
  }

  "The Steady function" should "function as expected" in {
    val instructionSize = Nop.instructionSize
    val steady = Program( instructionSize, Seq( ins( functions.indexOf(Steady), 0, 1 ) ), 1, 1, 1 )
    forAll { (a: Double, b: Double) =>
      val result = steady(List(a), List(b))
      assert( result._2.head === a )

      if( a == b ) {
        assert( result._1.result(1).head === 1.0 )
      } else {
        assert( result._1.result(1).head === 0.0 )
      }
    }
  }

  "The CellAccumulator function" should "function as expected" in {
    val instructionSize = Nop.instructionSize
    val acc = Program( instructionSize, Seq( ins( functions.indexOf(CellAccumulator), 0, 1 ) ), 2, 1, 1 )
    forAll { (a: Double, b: Double, c: Double) =>
      val expected = a + b * c
      val result = acc(List(a, c), List(b))
      assert( result._1.result(1).head === expected )
      assert( result._2.head === expected )
    }
  }

  "The CellMemory function" should "function as expected" in {
    val instructionSize = Nop.instructionSize
    val acc = Program( instructionSize, Seq( ins( functions.indexOf(CellMemory), 0, 1 ) ), 2, 1, 1 )
    forAll { (a: Double, b: Double, c: Double) =>
      val result = acc(List(a, c), List(b))
      if( c > 0.5 ) {
        assert( result._1.result(1).head === a )
        assert( result._2.head === a )
      } else {
        assert( result._1.result(1).head === b )
        assert( result._2.head === b )
      }
    }
  }

  "A problematic Neural Function program" should "shrink and function identically" in {
    val instr = Seq(Instruction(1275068988), Instruction(142158444), Instruction(738213889), Instruction(1140852986), Instruction(1409320763), Instruction(90163105), Instruction(1610665120), Instruction(1409312678), Instruction(1208007467), Instruction(68100365), Instruction(100744703), Instruction(117921639), Instruction(469843975))
    val program = Program(6, instr,1,1, instr.length)
    val minified = program.shrink.clean

    forAll { a: Double =>
      assert( program(List(a), 0.0)._1.result(1) === minified(List(a), 0.0)._1.result(1))
    }
  }

  "A neural function program" should "be able to be evolved to output the last positive input" in {
    implicit val evolveStrategy: EvolverStrategy = EvolverStrategy(32, 0.0001, optimiseForPipeline = false)

    implicit val ec: ExecutionContextExecutor = ExecutionContext.fromExecutor( Executors.newFixedThreadPool( Runtime.getRuntime.availableProcessors() ) )

    def sequence( p: Program, input: List[Double] ): List[Double] = {
      input
        .grouped( p.inputCount )
        .foldLeft( (List.empty[Double], List.fill[Double](p.data.length)(0.0)) ) {
          case ((res, state), i) =>
            val (r, s) = p(i, state)
            (r.result(p.outputCount).toList ++ res, s)
        }
        ._1.reverse
    }

    def score( p: Program ): Double = {
      val input = ThreadLocalRandom.current().doubles(101).toArray.toList.map( _ - 0.5 )
      val filtered = input.foldLeft( List.empty[Double] ) { case (a, b) =>
          if(b > 0) {
            b :: a
          } else {
            a.headOption.getOrElse(0.0) :: a
          }
      }.reverse

      val result = sequence(p, input)
      val sum = (filtered zip result).map( a => (a._1 - a._2).abs ).sum
      if(sum.isNaN)
        Double.PositiveInfinity
      else
        sum
    }

    val result = EvolveUtil.fitness( Generator(Nop.instructionSize, 16, 1, 1), 0.0,1000000, score )
    assert( score( result ) === 0.0 )
  }
}
