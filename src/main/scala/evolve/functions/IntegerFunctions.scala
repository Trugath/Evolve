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

object IntegerFunctions {

  implicit val functions: Seq[Function[Int]] = Seq[Function[Int]](
    Nop,
    Const,
    Add, Subtract, Multiply, Divide, Modulus, Increment, Decrement,
    And, Or, XOr, Not,
    ShiftLeft, ShiftSignedRight, ShiftUnsignedRight,
    Min, Max
  )

  implicit val scoreFunc: (Int, Int) => Long = (a, b) => {

    def nabs(i: Long): Long = if( i < 0 ) -i else i

    val result = nabs(a - b)
    assert(result >= 0)
    result * 10
  }

  implicit val createConstant: Int => Instruction = { value: Int =>
    Instruction(0).const(value & ( Int.MinValue >> (32 - Const.constantRegionSize) ), Const.constantRegionStart, Const.constantRegionSize)
  }

  object Nop extends Function[Int]  {
    override val arguments: Int = 1
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "Nop"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      arguments.head
    }
  }

  object Const extends Function[Int]  {
    override val arguments: Int = 0
    override val constantRegionSize: Int = 32 - constantRegionStart
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = {
      val value = inst.const(constantRegionStart, constantRegionSize)
      s"Const ($value)"
    }
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      inst.const(constantRegionStart, constantRegionSize)
    }
  }

  object Add extends Function[Int]  {
    override val cost: Int = 4
    override def getLabel(inst: Instruction): String = "Add"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      a + b
    }
  }

  object Subtract extends Function[Int]  {
    override val cost: Int = 4
    override def getLabel(inst: Instruction): String = "Subtract"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      a - b
    }
  }

  object Multiply extends Function[Int]  {
    override val cost: Int = 5
    override def getLabel(inst: Instruction): String = "Multiply"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      a * b
    }
  }

  object Divide extends Function[Int]  {
    override val cost: Int = 10
    override def getLabel(inst: Instruction): String = "Divide"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      try {
        a / b
      } catch {
        case _: ArithmeticException => 0
      }
    }
  }

  object Modulus extends Function[Int]  {
    override val cost: Int = 10
    override def getLabel(inst: Instruction): String = "Modulus"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      try {
        a % b
      } catch {
        case _: ArithmeticException => 0
      }
    }
  }

  object Increment extends Function[Int]  {
    override val arguments: Int = 1
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "Increment"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      arguments.head + 1
    }
  }

  object Decrement extends Function[Int]  {
    override val arguments: Int = 1
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "Decrement"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      arguments.head - 1
    }
  }

  object And extends Function[Int]  {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "&"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      a&b
    }
  }

  object Or extends Function[Int]  {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "|"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      a|b
    }
  }

  object XOr extends Function[Int]  {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "^"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      a^b
    }
  }

  object Not extends Function[Int]  {
    override val arguments: Int = 1
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "~"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      ~arguments.head
    }
  }

  object ShiftLeft extends Function[Int]  {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "<<"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      a << b
    }
  }

  object ShiftUnsignedRight extends Function[Int]  {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = ">>>"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      a >>> b
    }
  }

  object ShiftSignedRight extends Function[Int]  {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = ">>"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      a >> b
    }
  }


  object Max extends Function[Int] {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "Max"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      math.max(a, b)
    }
  }

  object Min extends Function[Int] {
    override val cost: Int = 3
    override def getLabel(inst: Instruction): String = "Min"
    override def apply(inst: Instruction, arguments: List[Int]): Int = {
      val a = arguments.head
      val b = arguments(1)
      math.min(a, b)
    }
  }
}
