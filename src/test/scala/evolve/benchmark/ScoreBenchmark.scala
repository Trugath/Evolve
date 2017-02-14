package evolve.benchmark

import java.util.concurrent.Executors

import evolve.core._
import org.scalameter.api.{Gen, _}

import scala.concurrent.ExecutionContext

/**
  * Created by Elliot on 13/02/2017.
  */
object ScoreBenchmark extends Bench.LocalTime {

  import evolve.functions.BooleanFunctions._

  private [this] implicit val ec = ExecutionContext.fromExecutor( Executors.newFixedThreadPool( Runtime.getRuntime.availableProcessors() * 2 ) )

  private [this] def bitsToBools(value: Int, bits: Int): List[Boolean] = {
    require(value >= 0 && value <= math.pow(2, bits))
    (0 until bits)
      .map( i => ((0x1 << i) & value) != 0x0 )
      .reverse
      .toList
  }

  private [this] val testCases = TestCases((for{
    l <- 0 until 16
    r <- 0 until 16
  } yield TestCase[Boolean](bitsToBools(l, 4) ::: bitsToBools(r, 4), bitsToBools(l + r, 5))).toList )

  performance of "Scoring eight input, five output, boolean programs" config (
    exec.minWarmupRuns -> 1,
    exec.maxWarmupRuns -> 4
  ) in {
    using(Gen.range("length")(32, 4096, 32)) in { size =>
      val program = Generator(6, size, 8, 5)(evolve.functions.BooleanFunctions.functions)
      measure method "score" in {
        testCases.score( program )
      }
    }
  }
}
