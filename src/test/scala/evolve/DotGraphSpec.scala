package evolve

import evolve.core.{DotGraph, Generator, Instruction, Program}
import org.scalacheck.Gen
import org.scalatest.FlatSpec
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

/**
  * Created by ellio on 09/08/2016.
  */
class DotGraphSpec extends FlatSpec with PropertyChecks with GeneratorDrivenPropertyChecks {
  "A known program" should "convert into a DOT representation" in {
    import evolve.functions.BooleanFunctions._
    val p1 = Program( Nop.instructionSize, Seq( Instruction(0) ), 1, 1, 1 )
    assert( p1( List( true ), List(false) )._1.result(1) === List( true ) )
    assert( p1( List( false ), List(false) )._1.result(1) === List( false ) )

    val graph = DotGraph( p1 )
    assert( graph.startsWith("digraph graphname {\r\n rankdir=\"LR\";\r\n\r\n // Inputs\r\n subgraph cluster_0 {\r\n" ))
    assert( graph.contains( "Nop" ))
    assert( graph.contains( "\"Input 0\"") )
    assert( graph.contains( "\"Output 0\"") )
  }

  "Any program" should "roduce a DOT string" in {
    forAll(Gen.choose[Int](1, 64), Gen.choose[Int](0, 16), Gen.choose[Int](1, 16), Gen.choose[Int](Int.MinValue, Int.MaxValue)) {
      (size: Int, inputCount: Int, outputCount: Int, seed: Int) => whenever(size >= outputCount && seed != 0) {
        {
          import functions.BooleanFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          assert( DotGraph(program).length > 0 )
        }
        {
          import functions.DoubleFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          assert( program.used.drop(inputCount).forall( _ == true ))
          assert( DotGraph(program).length > 0 )
        }
        {
          import functions.IntegerFunctions._
          val program = Generator(Nop.instructionSize, size, inputCount, outputCount, seed).shrink
          assert( program.used.drop(inputCount).forall( _ == true ))
          assert( DotGraph(program).length > 0 )
        }
      }
    }
  }
}
