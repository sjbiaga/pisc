Stochastic Pi-calculus in SCala aka PISC ala RISC (experimental)
================================================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the FS2's `Stream[_, _]` monad.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `FS2` `Stream[_, _]`
and Cats Effect `IO[_]`.

Composition: parallel modelled with - `Stream.eval(List(...).map(_.compile.drain).parSequenceVoid)`.

Summation: probabilistic choice modelled with - `Stream.eval(List(...).map(_.compile.drain).parSequenceVoid)`.

[Guarded] Replication: modelled with infinite streams.


Program
-------

The crucial observation is that a replication output guard gives rise to an _infinite_
"stream" of values. A second observation, just as important, is that, in this new
perspective, a sequence of prefixes is just a sequence of `flatMap`s of streams: for
example, the expression `!.a(b). !.b<c>.` can be viewed simply as a `flatMap` of two
infinite streams, rather than a prefix and the spawning of a fiber upon communication
on the prefix.


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `fs2` contains five files: `dump.scala`, `loop.scala`, `stats.scala`, `spi.scala`,
and `IO.main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/spi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ spi -fs2 ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ spio -fs2 ex

To get the intermediary `in/ex.scala.in` file, execute the `spin` command in the `sbt` shell:

    sbt:Stochastic π-Calculus[experimental]2Scala> spin -fs2 ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
