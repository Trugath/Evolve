package evolve

import evolve.core.{Function, Instruction, Program}
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

class StringIntProgramSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {

  def ins( index: Int, a: Int, b: Int )( implicit functions: Seq[Function[_]]) : Instruction = {
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

  def failSafe( f: => Int ): Int = {
    try {
      f
    } catch {
      case e: ArithmeticException => 0
    }
  }

  "the extended functions" should "execute" in {

    import evolve.functions.StringIntFunctions._

    val instructionSize = Token.instructionSize
    val token = Program( instructionSize, Seq( ins( functions.indexOf(Token), 0, 1 ) ), 1, 1 )

    assert( token(List(("a b c d e", 0))).result(1).head === ("a", 0) )
    assert( token(List(("a b c d e", 1))).result(1).head === ("b", 1) )
    assert( token(List(("a b c d e", 2))).result(1).head === ("c", 2) )
    assert( token(List(("a b c d e", 3))).result(1).head === ("d", 3) )
    assert( token(List(("a b c d e", 4))).result(1).head === ("e", 4) )
  }
}
