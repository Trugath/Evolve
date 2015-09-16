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

object DoubleFunctions {

  implicit val functions = Seq[Function[Double]](
    Nop,
    ConstLarge, ConstSmall,
    Add, Subtract, Multiply, Divide, Modulus, Increment, Decrement
  )

  implicit def scoreFunc: (Option[Double], Option[Double]) => Long = (a, b) => {

    def nabs(i: Double): Double = if( i < 0 ) -i else i

    val result: Double = (a, b) match {
      case (Some(left), Some(right)) if left.isNaN || right.isNaN => Int.MaxValue
      case (Some(left), Some(right)) => nabs(left - right).abs
      case (Some(left), _) => left.abs
      case (_, Some(right)) => right.abs
      case (_, _) => 0
    }
    assert(result >= -0.00001)
    math.min(result * Int.MaxValue, Long.MaxValue / 256L).toLong
  }

  object Nop extends Function[Double]  {
    override def arguments: Int = 1
    override def cost: Int = 2
    override def getLabel(inst: Instruction): String = "NopLeft"
    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a)
    }
  }

  object ConstLarge extends Function[Double]  {
    override def arguments: Int = 0
    override def cost: Int = 2
    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 32 - instructionSize)
      s"Const ($value)"
    }
    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      memory.append( inst.const(instructionSize, 32 - instructionSize) )
    }
  }

  object ConstSmall extends Function[Double]  {
    private val scale = math.pow(2.0, 32 - instructionSize)
    override def arguments: Int = 0
    override def cost: Int = 2
    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 32 - instructionSize) / scale
      s"Const ($value)"
    }
    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      memory.append( inst.const(instructionSize, 32 - instructionSize) / scale )
    }
  }

  object Add extends Function[Double]  {
    override def cost: Int = 4
    override def getLabel(inst: Instruction): String = "Add"
    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a + b)
    }
  }

  object Subtract extends Function[Double]  {
    override def cost: Int = 4
    override def getLabel(inst: Instruction): String = "Subtract"
    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a - b)
    }
  }

  object Multiply extends Function[Double]  {
    override def cost: Int = 5
    override def getLabel(inst: Instruction): String = "Multiply"
    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      memory.append(a * b)
    }
  }

  object Divide extends Function[Double]  {
    override def cost: Int = 10
    override def getLabel(inst: Instruction): String = "Divide"
    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      try {
        memory.append(a / b)
      } catch {
        case e: ArithmeticException => memory.append(0)
      }
    }
  }

  object Modulus extends Function[Double]  {
    override def cost: Int = 10
    override def getLabel(inst: Instruction): String = "Modulus"
    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      val b = memory(inst.pointer(instructionSize + argumentSize, argumentSize))
      try {
        memory.append(a % b)
      } catch {
        case e: ArithmeticException => memory.append(0)
      }
    }
  }

  object Increment extends Function[Double]  {
    override def arguments: Int = 1
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "Increment"
    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a+1)
    }
  }

  object Decrement extends Function[Double]  {
    override def arguments: Int = 1
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "Decrement"
    override def apply(inst: Instruction, memory: Memory[Double]): Memory[Double] = {
      val a = memory(inst.pointer(instructionSize, argumentSize))
      memory.append(a-1)
    }
  }
}
