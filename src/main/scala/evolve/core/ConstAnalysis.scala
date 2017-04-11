package evolve.core

import scala.annotation.tailrec

/**
  * Traces all constant pathways through the code and provides functions to modify the program based on this information
  */
object ConstAnalysis {

  def apply( program: Program )(implicit functions: Seq[Function[_]]): Seq[Boolean] = {
    import program._
    val const: Array[Boolean] = Array.fill(inputCount + data.length)(false)

    // mark known constants using the lack of inputs
    data
      .zipWithIndex
      .filter { case (inst, _) => inst.function.arguments == 0 && !inst.function.usesState }
      .foreach { case (_, index) => const(index+inputCount) = true }

    // work through program marking constants
    data
      .zipWithIndex
      .map( a => a.copy( _2 = a._2 + inputCount ))
      .foreach { case (inst, index) =>
        val func = inst.function

        @tailrec def inner( arg: Int ): Boolean = if( arg < func.arguments) {
          val pointer = inst.pointer( instructionSize + ( func.argumentSize * arg ), func.argumentSize )
          if(!const(pointer)) {
            false
          } else {
            inner( arg + 1 )
          }
        } else true
        const( index ) = !func.usesState && inner(0)
      }

    const.toSeq
  }

  def fillConstants[T]( program: Program )(implicit functions: Seq[Function[T]], const: (T) => Instruction ): Program = {

    import program._

    val constInstructions: Seq[Boolean] = ConstAnalysis( program )

    val working: Array[Instruction] = Array.ofDim(data.length)
    data.copyToArray(working)

    // extracts arguments from memory
    def arguments( func: Function[T], inst: Instruction ): List[T] = {

      @tailrec def extract( index: Int, acc: List[T] ): List[T] = if (index >= 0) {
        val source = inst.pointer(instructionSize + func.argumentSize * index, func.argumentSize)
        assert(constInstructions(source))

        val sourceInst = working(source - inputCount)
        val sourceFunc: Function[T] = sourceInst.function.asInstanceOf[Function[T]]
        assert(sourceFunc.arguments == 0)
        assert(!sourceFunc.usesState)

        val sourceValue = sourceFunc(sourceInst, Nil)
        extract( index - 1, sourceValue :: acc )
      } else acc

      func.arguments match {
        case 0 => Nil
        case _ => extract( func.arguments - 1, Nil )
      }
    }

    data
      .indices
      .filter( a => constInstructions( a + inputCount ) )
      .foreach { index =>
        val inst = working(index)
        val func = inst.function(functions).asInstanceOf[Function[T]]
        val value = func(inst, arguments(func, inst))
        val replacement = const(value)

        assert(replacement.function.arguments == 0)
        assert(!replacement.function.usesState)
        working(index) = replacement
      }

    assert( data
      .indices
      .forall { index =>
        val i2 = index + inputCount
        if( constInstructions(i2) ) {
          working(index).function.arguments == 0 && !working(index).function.usesState
        } else {
          working(index) == data(index)
        }
    } )

    Program( instructionSize, working.to[Seq], inputCount, outputCount)
  }
}
