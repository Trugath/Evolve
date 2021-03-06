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
object BooleanFunctions {

  implicit val functions: Seq[Function[Boolean]] = Seq[Function[Boolean]](
    Nop, Const,
    And, Or, Not,
    Implication, XOr, Equal
  )

  implicit val scoreFunc: (Boolean, Boolean) => Double = (a, b) => {
    val result = if (a == b) 0.0 else 10.0
    assert(result >= 0)
    result * result
  }

  implicit val createConstant: Boolean => Instruction = { value: Boolean =>
    Instruction(0).const( if(value) -1 else 0, Const.constantRegionStart, Const.constantRegionSize)
  }

  object Nop extends Function[Boolean]  {
    override val arguments: Int = 1
    override val cost: Int = 1
    override def getLabel(inst: Instruction): String = "Nop"

    override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
      arguments.head
    }
  }

  object Const extends Function[Boolean]  {
    override val arguments: Int = 0
    override val constantRegionSize = 1
    override val cost: Int = 1
    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 1) == -1
      s"Const ($value)"
    }
    override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
      inst.const(constantRegionStart, constantRegionSize) == -1
    }
  }

  object And extends Function[Boolean]  {
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "&"
    override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
      val a = arguments.head
      val b = arguments(1)
      a&b
    }
  }

  object Or extends Function[Boolean]  {
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "|"
    override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
      val a = arguments.head
      val b = arguments(1)
      a|b
    }
  }

  object Not extends Function[Boolean]  {
    override val arguments: Int = 1
    override val cost: Int = 2
    override def getLabel(inst: Instruction): String = "~"
    override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
      val a = arguments.head
      !a
    }
  }

  object Implication extends Function[Boolean]  {
    override val cost: Int = 7 // we want (!a | b) to be cheaper
    override def getLabel(inst: Instruction): String = "->"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
      val a = arguments.head
      val b = arguments(1)
      !a||b
    }
  }

  object XOr extends Function[Boolean]  {
    override val cost: Int = 3 // we want to be cheaper than the equivalent basic gate setup
    override def getLabel(inst: Instruction): String = "^"
    override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
      val a = arguments.head
      val b = arguments(1)
      a^b
    }
  }

  object Equal extends Function[Boolean]  {
    override val cost: Int = 10 // we want XOR -> NOT to be cheaper
    override def getLabel(inst: Instruction): String = "=="
    override def apply(inst: Instruction, arguments: List[Boolean]): Boolean = {
      val a = arguments.head
      val b = arguments(1)
      a==b
    }
  }
}
