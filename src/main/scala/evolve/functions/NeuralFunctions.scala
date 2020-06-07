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

package evolve.functions

import evolve.core.{Function, Instruction}
import evolve.functions.NeuralFunctions.ConstSmall.constantRegionSize

object NeuralFunctions {

  implicit val functions: Seq[Function[Double]] = Seq[Function[Double]](
    Nop,
    ConstLarge, ConstSmall,
    Add, Subtract, Multiply, Divide, Modulus, Increment, Decrement,
    Min, Max,
    GreaterThanZero, LessThanZero,
    Sigmoid, NaturalExp, NaturalLog, TanH,
    Signum,
    Delay, Increasing, Decreasing, Steady,
    CellAccumulator, Weight, CellMemory,
    GreaterThan, LessThan
  )

  implicit val scoreFunc: (Double, Double) => Long = (a, b) => {

    def nabs(i: Double): Double = if( i < 0 ) -i else i

    val result: Double = (a, b) match {
      case (left, right) if left.isNaN || right.isNaN => Int.MaxValue
      case (left, right)                              => nabs(left - right).abs
    }
    assert(result >= -0.00001)
    math.min(result * Int.MaxValue, Long.MaxValue.toDouble / 256.0).toLong
  }

  implicit def createConstant(implicit functions: Seq[Function[Double]]): Double => Instruction = { value: Double =>
    val constLargeScale = math.pow(2.0, math.min(constantRegionSize, 4))
    val constLargeValue: Int = math.min(math.max((value * constLargeScale).toInt, Int.MinValue >> (32 - ConstLarge.constantRegionSize)), Int.MaxValue >>> (32 - ConstLarge.constantRegionSize))
    val constLarge = Instruction(0)
      .instruction(functions.indexOf(ConstLarge), ConstLarge.instructionSize)
      .const(constLargeValue, ConstLarge.constantRegionStart, ConstLarge.constantRegionSize)
    val constLargeError: Double = (ConstLarge(constLarge, Nil) - value).abs

    val constSmallScale = math.pow(2.0, ConstSmall.constantRegionSize)
    val constSmallValue = math.min(math.max((value * constSmallScale).toInt, Int.MinValue >> (32 - ConstSmall.constantRegionSize)), Int.MaxValue >>> (32 - ConstSmall.constantRegionSize))
    val constSmall = Instruction(0)
      .instruction(functions.indexOf(ConstSmall), ConstSmall.instructionSize)
      .const(constSmallValue, ConstSmall.constantRegionStart, ConstSmall.constantRegionSize)
    val constSmallError: Double = (ConstSmall(constSmall, Nil) - value).abs

    if( constLargeError < constSmallError) {
      constLarge
    } else {
      constSmall
    }
  }

  object Nop extends Function[Double]  {
    override val arguments: Int = 1
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "Nop"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      arguments.head
    }
  }

  object ConstLarge extends Function[Double]  {
    override val arguments: Int = 0
    override val constantRegionSize: Int = 31 - constantRegionStart
    override val cost: Int = 2

    private [this] val scale = math.pow(2.0, math.min(constantRegionSize, 4))

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(constantRegionStart, constantRegionSize) / scale
      s"Const ($value)"
    }
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      inst.const(constantRegionStart, constantRegionSize) / scale
    }
  }

  object ConstSmall extends Function[Double]  {
    override val arguments: Int = 0
    override val constantRegionSize: Int = 31 - constantRegionStart
    override val cost: Int = 2

    private [this] val scale = math.pow(2.0, constantRegionSize)

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(constantRegionStart, constantRegionSize) / scale
      s"Const ($value)"
    }
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      inst.const(constantRegionStart, constantRegionSize) / scale
    }
  }

  object Add extends Function[Double]  {
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "Add"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      a + b
    }
  }

  object Subtract extends Function[Double]  {
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "Subtract"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      a - b
    }
  }

  object Multiply extends Function[Double]  {
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "Multiply"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      a * b
    }
  }

  object Divide extends Function[Double]  {
    override val cost: Int = 5
    override def getLabel(inst: Instruction): String = "Divide"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      try {
        a / b
      } catch {
        case _: ArithmeticException => 0.0
      }
    }
  }

  object Modulus extends Function[Double]  {
    override val cost: Int = 5
    override def getLabel(inst: Instruction): String = "Modulus"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      try {
        a % b
      } catch {
        case _: ArithmeticException => 0.0
      }
    }
  }

  object Increment extends Function[Double]  {
    override val arguments: Int = 1
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "Increment"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      arguments.head + 1.0
    }
  }

  object Decrement extends Function[Double]  {
    override val arguments: Int = 1
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "Decrement"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      arguments.head - 1.0
    }
  }

  object Max extends Function[Double] {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "Max"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      math.max(a, b)
    }
  }

  object Min extends Function[Double] {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "Min"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      math.min(a, b)
    }
  }

  object GreaterThanZero extends Function[Double] {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "GreaterThanZero"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      if( a > 0.0 ) {
        b
      } else {
        0.0
      }
    }
  }

  object LessThanZero extends Function[Double] {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "LessThanZero"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      if( a < 0.0 ) {
        b
      } else {
        0.0
      }
    }
  }


  object Sigmoid extends Function[Double] {
    override val arguments: Int = 1
    override val cost: Int = 5
    override def getLabel(inst: Instruction): String = "Sigmoid"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      1.0 / (1.0 + math.exp(-a))
    }
  }

  object NaturalExp extends Function[Double] {
    override val arguments: Int = 1
    override val cost: Int = 5
    override def getLabel(inst: Instruction): String = "NaturalExp"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      math.exp(a)
    }
  }

  object NaturalLog extends Function[Double] {
    override val arguments: Int = 1
    override val cost: Int = 5
    override def getLabel(inst: Instruction): String = "NaturalLog"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      math.log(a)
    }
  }

  object TanH extends Function[Double] {
    override val arguments: Int = 1
    override val cost: Int = 5
    override def getLabel(inst: Instruction): String = "TanH"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      math.tanh(a)
    }
  }

  object Signum extends Function[Double] {
    override val arguments: Int = 1
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "Signum"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      math.signum(a)
    }
  }

  object Delay extends Function[Double] {
    override val arguments: Int = 1
    override val cost: Int = 2
    override val usesState = true
    override def getLabel(inst: Instruction): String = "Delay"
    override def apply(inst: Instruction, state: Double, arguments: List[Double]): (Double, Double) = {
      (arguments.head, state)
    }
  }

  object Increasing extends Function[Double] {
    override val arguments: Int = 1
    override val cost: Int = 2
    override val usesState = true
    override def getLabel(inst: Instruction): String = "Increasing"
    override def apply(inst: Instruction, state: Double, arguments: List[Double]): (Double, Double) = {
      (arguments.head, if( state < arguments.head ) 1.0 else 0.0 )
    }
  }

  object Decreasing extends Function[Double] {
    override val arguments: Int = 1
    override val cost: Int = 2
    override val usesState = true
    override def getLabel(inst: Instruction): String = "Decreasing"
    override def apply(inst: Instruction, state: Double, arguments: List[Double]): (Double, Double) = {
      (arguments.head, if( state > arguments.head ) 1.0 else 0.0 )
    }
  }


  object Steady extends Function[Double] {
    override val arguments: Int = 1
    override val cost: Int = 2
    override val usesState = true
    override def getLabel(inst: Instruction): String = "Steady"
    override def apply(inst: Instruction, state: Double, arguments: List[Double]): (Double, Double) = {
      (arguments.head, if( state == arguments.head ) 1.0 else 0.0 )
    }
  }

  object CellAccumulator extends Function[Double] {
    override val arguments: Int = 2
    override val cost: Int = 2
    override val usesState = true
    override def getLabel(inst: Instruction): String = "CellAccumulator"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, state: Double, arguments: List[Double]): (Double, Double) = {
      val i = arguments.head
      val f = arguments(1)

      val c = i + state * f
      (c, c)
    }
  }

  object Weight extends Function[Double] {
    override val arguments: Int = 1
    override val constantRegionStart: Int = instructionSize + argumentSize
    override val constantRegionSize: Int = 31 - constantRegionStart
    override val cost: Int = 2

    val scale: Double = math.pow(2.0, constantRegionSize - 1) - 1

    override def getLabel(inst: Instruction): String = {
      val value = inst.const(constantRegionStart, constantRegionSize) / scale
      s"Weight ($value)"
    }
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      arguments.head * (inst.const(constantRegionStart, constantRegionSize).toDouble / scale)
    }
  }

  object CellMemory extends Function[Double] {
    override val arguments: Int = 2
    override val cost: Int = 2
    override val usesState = true
    override def getLabel(inst: Instruction): String = "CellMemory"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, state: Double, arguments: List[Double]): (Double, Double) = {
      val i = arguments.head
      val f = arguments(1)

      if( f > 0.5 ) {
        (i, i)
      } else {
        (state, state)
      }
    }
  }

  object GreaterThan extends Function[Double]  {
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = ">"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      if(a > b) 1.0 else 0.0
    }
  }

  object LessThan extends Function[Double]  {
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "<"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      if(a < b) 1.0 else 0.0
    }
  }
}
