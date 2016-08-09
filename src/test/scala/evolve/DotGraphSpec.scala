package evolve

import evolve.core.{DotGraph, Instruction, Program}
import org.scalatest.FlatSpec

/**
  * Created by ellio on 09/08/2016.
  */
class DotGraphSpec extends FlatSpec {
  "A known program" should "convert into a DOT representation" in {
    import evolve.functions.BooleanFunctions._
    val p1 = Program( Nop.instructionSize, Seq( Instruction(0) ), 1, 1 )
    assert( p1( List( true ) ).result(1) === List( true ) )
    assert( p1( List( false ) ).result(1) === List( false ) )

    val graph = DotGraph( p1 )
    assert( graph.startsWith("""digraph graphname {
                               |rankdir="LR";
                               |subgraph cluster_0 {""".stripMargin ))
    assert( graph.contains( "Nop" ))
    assert( graph.contains( "\"Input 0\"") )
    assert( graph.contains( "\"Output 0\"") )
  }
}
