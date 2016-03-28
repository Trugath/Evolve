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

package evolve.functions

import evolve.core.{Function, Instruction, Memory}

object IntegerFunctions {

  implicit val functions = Seq[Function[Int]](
    Nop,
    Const,
    Add, Subtract, Multiply, Divide, Modulus, Increment, Decrement,
    And, Or, XOr, Not,
    ShiftLeft, ShiftSignedRight, ShiftUnsignedRight,
    Min, Max
  )

  implicit def scoreFunc: (Option[Int], Option[Int]) => Long = (a, b) => {

    def nabs(i: Long): Long = if( i < 0 ) -i else i

    val result = (a, b) match {
      case (Some(left), Some(right)) => nabs(left - right)
      case (Some(left), None) => left.abs
      case (None, Some(right)) => right.abs
      case (None, None) => 0
    }
    assert(result >= 0)
    result * 10
  }

  object Nop extends Function[Int]  {
    override def arguments: Int = 1
    override def cost: Int = 2
    override def getLabel(inst: Instruction): String = "Nop"
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a)
    }
  }

  object Const extends Function[Int]  {
    override def arguments: Int = 0
    override def cost: Int = 2
    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 32 - instructionSize)
      s"Const ($value)"
    }
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      memory.append( inst.const(instructionSize, 32 - instructionSize) )
    }
  }

  object Add extends Function[Int]  {
    override def cost: Int = 4
    override def getLabel(inst: Instruction): String = "Add"
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a + b)
    }
  }

  object Subtract extends Function[Int]  {
    override def cost: Int = 4
    override def getLabel(inst: Instruction): String = "Subtract"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a - b)
    }
  }

  object Multiply extends Function[Int]  {
    override def cost: Int = 5
    override def getLabel(inst: Instruction): String = "Multiply"
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a * b)
    }
  }

  object Divide extends Function[Int]  {
    override def cost: Int = 10
    override def getLabel(inst: Instruction): String = "Divide"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      try {
        memory.append(a / b)
      } catch {
        case e: ArithmeticException => memory.append(0)
      }
    }
  }

  object Modulus extends Function[Int]  {
    override def cost: Int = 10
    override def getLabel(inst: Instruction): String = "Modulus"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      try {
        memory.append(a % b)
      } catch {
        case e: ArithmeticException => memory.append(0)
      }
    }
  }

  object Increment extends Function[Int]  {
    override def arguments: Int = 1
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "Increment"
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a+1)
    }
  }

  object Decrement extends Function[Int]  {
    override def arguments: Int = 1
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "Decrement"
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a-1)
    }
  }

  object And extends Function[Int]  {
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "&"
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a&b)
    }
  }

  object Or extends Function[Int]  {
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "|"
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a|b)
    }
  }

  object XOr extends Function[Int]  {
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "^"
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a^b)
    }
  }

  object Not extends Function[Int]  {
    override def arguments: Int = 1
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "~"
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(~a)
    }
  }

  object ShiftLeft extends Function[Int]  {
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "<<"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a << b)
    }
  }

  object ShiftUnsignedRight extends Function[Int]  {
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = ">>>"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a >>> b)
    }
  }

  object ShiftSignedRight extends Function[Int]  {
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = ">>"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a >> b)
    }
  }


  object Max extends Function[Int] {
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "Max"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(math.max(a, b))
    }
  }

  object Min extends Function[Int] {
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "Min"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[Int]): Memory[Int] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(math.min(a, b))
    }
  }
}
