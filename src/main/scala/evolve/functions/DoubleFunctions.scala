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

object DoubleFunctions {

  implicit val functions: Seq[Function[Double]] = Seq[Function[Double]](
    Nop,
    ConstLarge, ConstSmall,
    Add, Subtract, Multiply, Divide, Modulus, Increment, Decrement,
    Min, Max,
    GreaterThanZero, LessThanZero,
    Sigmoid, NaturalExp, NaturalLog,
    Signum
  )

  implicit val scoreFunc: (Double, Double) => Double = (a, b) => {

    def nabs(i: Double): Double = if( i < 0 ) -i else i

    val result: Double = (a, b) match {
      case (left, right) if left.isNaN || right.isNaN => Int.MaxValue
      case (left, right)                              => nabs(left - right).abs
    }
    assert(result >= -0.00001)
    result
  }

  implicit val createConstant: Double => Instruction = { (value: Double) =>
    val constLargeValue: Int = math.min(math.max(value.toInt, Int.MinValue >> (32 - ConstLarge.constantRegionSize)), Int.MaxValue >>> (32 - ConstLarge.constantRegionSize))
    val constLarge = Instruction(0).const(constLargeValue, ConstLarge.constantRegionStart, ConstLarge.constantRegionSize)
    val constLargeError: Double = (ConstLarge(constLarge, Nil) - value).abs

    val scale = math.pow(2.0, ConstSmall.constantRegionSize)
    val constSmallValue = math.min(math.max((value * scale).toInt, Int.MinValue >> (32 - ConstLarge.constantRegionSize)), Int.MaxValue >>> (32 - ConstLarge.constantRegionSize))
    val constSmall = Instruction(0).const(constSmallValue, ConstSmall.constantRegionStart, ConstSmall.constantRegionSize)
    val constSmallError: Double = (ConstSmall(constSmall, Nil) - value).abs

    if (constLargeError < constSmallError) {
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
    override val constantRegionSize: Int = 32 - constantRegionStart
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = {
      val value = inst.const(constantRegionStart, constantRegionSize)
      s"Const ($value)"
    }
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      inst.const(constantRegionStart, constantRegionSize)
    }

}
  object ConstSmall extends Function[Double]  {
    override val arguments: Int = 0
    override val constantRegionSize: Int = 32 - constantRegionStart
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
    override val cost: Int = 4
    override def getLabel(inst: Instruction): String = "Add"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      a + b
    }
  }

  object Subtract extends Function[Double]  {
    override val cost: Int = 4
    override def getLabel(inst: Instruction): String = "Subtract"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      a - b
    }
  }

  object Multiply extends Function[Double]  {
    override val cost: Int = 5
    override def getLabel(inst: Instruction): String = "Multiply"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      val b = arguments(1)
      a * b
    }
  }

  object Divide extends Function[Double]  {
    override val cost: Int = 10
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
    override val cost: Int = 10
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

  object Signum extends Function[Double] {
    override val arguments: Int = 1
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "Signum"
    override def apply(inst: Instruction, arguments: List[Double]): Double = {
      val a = arguments.head
      math.signum(a)
    }
  }
}
