package evolve.util

import evolve.core.{Function, Instruction, Program}

/**
  * Created by Elliot on 19/08/2016.
  */
object ProgramUtil {

  /**
    * Generates a number of NOP instructions each wired to the preceding instruction
    * @param length Total number of Nop instructions chained together
    * @param functions the functions to use for the Nop
    * @return The Nop sled as a sequence of instructions
    */
  def nopChain( length: Int )( implicit functions: Seq[Function[_]] ): Seq[Instruction] = {
    val f = functions.find( _.getLabel( Instruction(0) ) == "Nop" ).getOrElse( functions.head )
    val nop = Instruction(0).instruction( functions.indexOf(f), f.instructionSize )

    def gen( index: Int, acc: List[Instruction] ): List[Instruction] = {
      val inst =
        nop
          .pointer( index, f.instructionSize, f.argumentSize )
          .pointer( index, f.instructionSize + f.argumentSize, f.argumentSize )

      if(index == 0) {
        inst :: acc
      } else {
        gen( index - 1, inst :: acc )
      }
    }

    gen( length - 1, Nil )
  }

  /**
    * Generates a Nop program with the maximum length of Nop chain possible.
    * All outputs are wired to the first input through the nop chain.
    * @param length length of program required
    * @param inputs number of inputs for the program (>= 1)
    * @param outputs number of outputs for the program (>= 1)
    * @param functions the functions to use for the Nop
    * @return Newly created Nop program
    */
  def nopProgramLong(length: Int, inputs: Int, outputs: Int )(implicit functions: Seq[Function[_]] ): Program = {
    val f = functions.head
    Program( f.instructionSize, nopChain( length ), inputs, outputs )
  }

  /**
    * Generates a Nop program with the minimum length of Nop chain possible.
    * All outputs are directly wired to the first input using a nop.
    * @param length length of program required
    * @param inputs number of inputs for the program (>= 1)
    * @param outputs number of outputs for the program (>= 1)
    * @param functions the functions to use for the Nop
    * @return Newly created Nop program
    */
  def nopProgramShort(length: Int, inputs: Int, outputs: Int )(implicit functions: Seq[Function[_]] ): Program = {
    val f = functions.find( _.getLabel( Instruction(0) ) == "Nop" ).getOrElse( functions.head )
    val nop = Instruction(0).instruction( functions.indexOf(f), f.instructionSize )

    def gen( index: Int, acc: List[Instruction] ): List[Instruction] = {
      val inst =
        nop
          .pointer( 0, f.instructionSize, f.argumentSize )
          .pointer( 0, f.instructionSize + f.argumentSize, f.argumentSize )

      if(index == 0) {
        inst :: acc
      } else {
        gen( index - 1, inst :: acc )
      }
    }

    Program( f.instructionSize, gen( length - 1, Nil ), inputs, outputs )
  }
}
