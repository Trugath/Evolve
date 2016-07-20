# Evolve
Genetic Algorithm Library [![Build Status](https://travis-ci.org/Trugath/Evolve.svg?branch=2_12)](https://travis-ci.org/Trugath/Evolve)[![Coverage Status](https://coveralls.io/repos/github/Trugath/Evolve/badge.svg?branch=2_12)](https://coveralls.io/github/Trugath/Evolve?branch=2_12)

This is a core library for use in generating Cartesian Genetic Programs.
The runtime execution requires no external libraries. External libraries are used only in testing.

The evolved CGP Programs are defined using a vector of integers. As such the Cartesian program is a single dimensional array instead of the more traditional two dimensional array.

There are four packages of example functions shipped but the library is designed for the user to provide its own functions.

In the examples folder there are some example applications to evolve solutions to problems.
The best one to start with is the ThreeBitAdder as it usually evolves a solution in under 100 generations. 
The example optimises the solution quite a bit (so it looks sane) before exporting a DOT file so you can visualise the program.

![Three Bit Adder](https://github.com/Trugath/Evolve/blob/master/images/ThreeBitAdder.png)

## Usage

The library requires a Seq of evolve.core.Function objects in implicit scope. 
```scala
  implicit val functions = Seq[Function[Boolean]](
    Nop, Const,
    And, Or, Not,
    Implication, XOr, Equal
  )
```
  
```scala
  import evolve.functions.BooleanFunctions._
```

This allows most functionality to work. To run the evolver you need an `EvolverStrategy` in implicit scope. This defines how many children and the mutation rate used.
```scala
  implicit val evolveStrategy = EvolverStrategy(128, 0.005)
```

You need a starter program in order to start evolution. You could define it manually or use the `Generator`

```scala
val start = Generator(Nop.instructionSize, size = 32, inputCount = 3, outputCount = 2)
```

To evolve the program you need test cases to score the program against.

```scala
    val testCases = TestCases(List(
      TestCase(List(false, false, false), List(false, false)),
      TestCase(List(false, false, true), List(false, true)),
      TestCase(List(false, true, false), List(false, true)),
      TestCase(List(false, true, true), List(true, false)),
      TestCase(List(true, false, false), List(false, true)),
      TestCase(List(true, false, true), List(true, false)),
      TestCase(List(true, true, false), List(true, false)),
      TestCase(List(true, true, true), List(true, true))
    ))
```

You can now write your own evolution system 
```scala
    @tailrec def function(program: Program): Program = {
      Evolver(program, testCases, optimise = false) match {
        case Some(evolved) =>
          val score = testCases.score(evolved)
          if (score == 0) {
            println( s"Solution found" )
            evolved
          } else {
            function(evolved)
          }

        case None =>
          function(program)
      }
    }

    val solution = function(start)
```

or use one of the provided functions
```scala
  val solution = EvolveUtil.counted(start, 10000, optimise = false, testCases)
```

You can output the solution to a DOT graph
```scala
Files.write(Paths.get("solution.dot"), DotGraph(optimised).getBytes(StandardCharsets.UTF_8) )
```
