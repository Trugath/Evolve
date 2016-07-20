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

object StringIntFunctions {

  private def distance(s1: String, s2: String): Int = {

    def minimum(i1: Int, i2: Int, i3: Int) = math.min(math.min(i1, i2), i3)

    val dist = Array.tabulate(s2.length + 1, s1.length + 1) { (j, i) => if (j == 0) i else if (i == 0) j else 0 }

    for (j <- 1 to s2.length; i <- 1 to s1.length)
      dist(j)(i) = if (s2(j - 1) == s1(i - 1)) dist(j - 1)(i - 1)
      else minimum(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)

    dist(s2.length)(s1.length)
  }

  implicit val functions = Seq[Function[(String, Int)]](
    Nop,

    ConstChar, ConstInt,

    // choice
    Min, Max, Token, CharAt, TakeString, TakeInt, OrElse,

    // string to int
    Length, Hash, Distance, Compare, Contains,

    // int to string
    Start, End,

    // string
    Concatenate,
    Head, Tail,
    Capitalise, UpperCase, LowerCase,
    Reverse,

    // int
    Add, Subtract, Multiply, Divide, Modulus, Increment, Decrement, Average
  )

  implicit def scoreFunc: (Option[(String, Int)], Option[(String, Int)]) => Long = (a, b) => {

    def nabs(i: Long): Long = if( i < 0 ) -i else i

    val result = (a, b) match {
      case (Some((leftS, leftI)), Some((rightS, rightI))) => nabs(distance(leftS, rightS) * 100) + nabs(leftS.compareTo(rightS)) + nabs(leftI - rightI)
      case (Some((leftS, leftI)), None) => nabs(distance(leftS, "")) + nabs(leftI)
      case (None, Some((rightS, rightI))) => nabs(distance(rightS, "")) + nabs(rightI)
      case (None, None) => 0
    }
    assert(result >= 0)
    result * 100
  }

  object Nop extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 1

    override def getLabel(inst: Instruction): String = "Nop"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a
    }
  }

  object ConstChar extends Function[(String, Int)]  {
    override def arguments: Int = 0
    override def cost: Int = 2
    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 32 - instructionSize).toChar.toString
      s"Const ($value)"
    }
    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      (inst.const(instructionSize, 32 - instructionSize).toChar.toString, 0)
    }
  }

  object ConstInt extends Function[(String, Int)]  {
    override def arguments: Int = 0
    override def cost: Int = 2
    override def getLabel(inst: Instruction): String = {
      val value = inst.const(instructionSize, 32 - instructionSize)
      s"Const ($value)"
    }
    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      ("", inst.const(instructionSize, 32 - instructionSize))
    }
  }

  object Min extends Function[(String, Int)] {

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Min"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      if(a._2 <= b._2)
        a
      else
        b
    }
  }

  object Max extends Function[(String, Int)] {

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Max"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      if(a._2 >= b._2)
        a
      else
        b
    }
  }

  object Token extends Function[(String, Int)] {

    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Token"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val tokens = a._1.split("\\s+")
      val token = math.max(-1, math.min(tokens.length - 1, a._2))
      if(token == -1)
        a.copy(_1 = "")
      else
        a.copy(_1 = tokens(token))
    }
  }

  object CharAt extends Function[(String, Int)] {

    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "CharAt"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val char = math.max(-1, math.min(a._1.length - 1, a._2))
      if(char == -1)
        a.copy(_1 = "")
      else
        a.copy(_1 = a._1.charAt(char).toString)
    }
  }

  object TakeString extends Function[(String, Int)] {

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "TakeString"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      a.copy(_1 = b._1)
    }
  }

  object TakeInt extends Function[(String, Int)] {

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "TakeInt"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      a.copy(_2 = b._2)
    }
  }

  object OrElse extends Function[(String, Int)] {

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "OrElse"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      if (a._1.length() > 0) a else b
    }
  }

  object Length extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Length"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy(_2 = a._1.length)
    }
  }

  object Hash extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Hash"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy(_2 = a._1.hashCode)
    }
  }

  object Distance extends Function[(String, Int)] {

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Distance"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      (a._1, distance(a._1, b._1))
    }
  }

  object Compare extends Function[(String, Int)] {

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Compare"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      (a._1, a._1.compareTo(b._1))
    }
  }

  object Contains extends Function[(String, Int)] {

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Contains"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      (a._1, if (a._1.contains(b._1)) 1 else 0)
    }
  }

  object Start extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Start"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy(_1 = a._1.splitAt(math.max(0, math.min(a._1.length, a._2)))._1)
    }
  }

  object End extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "End"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy(_1 = a._1.splitAt(math.max(0, math.min(a._1.length, a._2)))._2)
    }
  }

  object Concatenate extends Function[(String, Int)] {

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Concatenate"

    override def ordered: Boolean = true

    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      (a._1 + b._1, a._2)
    }
  }

  object Head extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Head"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy(_1 = a._1.headOption.getOrElse("").toString)
    }
  }

  object Tail extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Tail"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      try {
        a.copy(_1 = a._1.tail)
      } catch {
        case _: UnsupportedOperationException => a.copy(_1 = "")
      }
    }
  }

  object Capitalise extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Capitalise"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy(_1 = a._1.capitalize)
    }
  }

  object UpperCase extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "UpperCase"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy( _1 = a._1.toUpperCase )
    }
  }

  object LowerCase extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "LowerCase"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy( _1 = a._1.toLowerCase )
    }
  }

  object Reverse extends Function[(String, Int)] {
    override def arguments: Int = 1

    override def cost: Int = 10

    override def getLabel(inst: Instruction): String = "Reverse"


    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy( _1 = a._1.reverse )
    }
  }


  object Add extends Function[(String, Int)]  {
    override def cost: Int = 4
    override def getLabel(inst: Instruction): String = "Add"
    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      (a._1, a._2 + b._2)
    }
  }

  object Subtract extends Function[(String, Int)]  {
    override def cost: Int = 4
    override def getLabel(inst: Instruction): String = "Subtract"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      (a._1, a._2 - b._2)
    }
  }

  object Multiply extends Function[(String, Int)]  {
    override def cost: Int = 5
    override def getLabel(inst: Instruction): String = "Multiply"
    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      (a._1, a._2 * b._2)
    }
  }

  object Divide extends Function[(String, Int)]  {
    override def cost: Int = 10
    override def getLabel(inst: Instruction): String = "Divide"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      try {
        (a._1, a._2 / b._2)
      } catch {
        case e: ArithmeticException => ("", 0)
      }
    }
  }

  object Modulus extends Function[(String, Int)]  {
    override def cost: Int = 10
    override def getLabel(inst: Instruction): String = "Modulus"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      try {
        (a._1, a._2 % b._2)
      } catch {
        case e: ArithmeticException => ("", 0)
      }
    }
  }

  object Increment extends Function[(String, Int)]  {
    override def arguments: Int = 1
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "Increment"
    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy( _2 = a._2 + 1)
    }
  }

  object Decrement extends Function[(String, Int)]  {
    override def arguments: Int = 1
    override def cost: Int = 3
    override def getLabel(inst: Instruction): String = "Decrement"
    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      a.copy( _2 = a._2 - 1)
    }
  }

  object Average extends Function[(String, Int)]  {
    override def cost: Int = 10
    override def getLabel(inst: Instruction): String = "Average"
    override def ordered: Boolean = true
    override def apply(inst: Instruction, memory: Memory[(String, Int)]): (String, Int) = {
      val a = argument(inst, 0, memory)
      val b = argument(inst, 1, memory)
      a.copy( _2 = ((a._2.toLong + b._2.toLong) / 2).toInt  )
    }
  }
}
