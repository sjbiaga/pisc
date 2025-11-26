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

There are two phases to the optimizer. Phase 1 is due to the fact that the generator
issues an extra method for each agent invocation: in some cases - when the invocation
is not preceded by prefixes - the call to this extra method can be replaced with the
invocation (the direct call to the agent), and the method removed. Phase 2 succeeds
phase 1, and is based on the fact that the code is generated uniformly whether or not
an expression is part of a summation; indifferently, the receive blocks `fold` an
optional atomic boolean: this must not be the case except when the methods are actually
invoked as part of a summation. Particularly for "cases sum", these methods which are
invoked as part of a summation do not themselves `fold` an atomic boolean, they simply
perform (nested) case analysis and only on the exact (mis)match further invoke the actual
method which must `fold` an atomic boolean: therefore, this situation is handled separately,
and strictly those methods further invoked are optimized in phase 2, not the proxy methods
which perform case analysis.


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
