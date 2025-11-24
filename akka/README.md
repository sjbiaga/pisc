Pi-calculus in SCala aka PISC ala RISC (experimental)
=====================================================

The π-calculus inner process nest inside outer processes, as nested methods.
These define the behavior of `Akka` actors corresponding to processes, but
their parameters also make outer names available to inner scope.

Prefixes are mapped one to one on `Scala` for-comprehensions
"inside" the Scala's `Future[_]` monad.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `Akka` and/or `Future`.


Program
-------


Optimizer
---------


Runtime
-------


Apps (examples)
---------------

Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `akka` contains five files: `dump.scala`, `loop.scala`, `stats.scala`, `spi.scala`,
and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/spi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ spi -akka ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ spio -akka ex

To get the intermediary `in/ex.scala.in` file, execute the `spin` command in the `sbt` shell:

    sbt:Stochastic π-Calculus[experimental]2Scala> spin -kk ex

where `example/pisc/ex.pisc` contains the stochastic π-calculus source (equations binding
agents to process expressions).

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
