Stochastic Pi-calculus in SCala aka sPISC ala RISC (experimental)
=================================================================


Program
-------


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `cef` contains five files: `dump.scala`, `loop.scala`, `stats.scala`, `spi.scala`,
and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/spi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ spi -cef ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ spio -cef ex

To get the intermediary `in/ex.scala.in` file, execute the `spin` command in the `sbt` shell:

    sbt:Stochastic π-Calculus[experimental]2Scala> spin -cef ex

where `example/pisc/ex.pisc` contains the stochastic π-calculus source (equations binding
agents to process expressions).

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
