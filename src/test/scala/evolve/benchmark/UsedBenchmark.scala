package evolve.benchmark

import evolve.core._
import org.scalameter.api.{Gen, _}

/**
  * Created by Elliot on 13/02/2017.
  */
object UsedBenchmark extends Bench.LocalTime {

  import evolve.functions.BooleanFunctions._

   performance of "Usage checking of programs" in {
    measure method "used" in {
      using(Gen.range("length")(32, 4096, 32)) in { size =>
        val program = Generator(6, size, 3, 2)
      }
    }
  }
}
