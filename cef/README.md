BioAmbients in SCala aka BASC ala RISC (experimental)
=====================================================

BioAmbients maps one to one on `Scala` for-comprehensions
"inside" the Cats Effect's `IO[_]` monad.

The bioambients branch adds capabilities in comparison with
the [stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic).
This branch uses _cancellation_ to discard actions/capabilities.
Another [branch](https://github.com/sjbiaga/pisc/tree/bioambients-flatMap)
heavily uses `flatMap`'s and comparison with `null` to discard actions/capabilities.

After code generation, the bioambients "processes" could be
programmatically typed as `Scala` code using `CE` `IO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Channels for names work as [CE tutorial](https://typelevel.org/cats-effect/docs/tutorial)'s
producer/consumer but no queue, only `takers` and `offerers`, which can be at most one.

Composition: parallel modelled with - `List(...).parSequence`.

Summation: *probabilistic* choice modelled with - `parSequence`.

[Guarded] Replication: modelled with - `parSequence` and `lazy val` [or `def`].


Program
-------

A new name - will be available in the Scala scope:

    for
      x <- ν
      .
      .
      .
    yield
      ()

The inaction - `IO.unit`.

Agent identifiers (literals) start with uppercase, while
channel names start with lowercase.


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `cef` contains five files: `dump.scala`, `loop.scala`, `stats.scala`, `ba.scala`,
and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/ba.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ ba -cef ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ baio -cef ex

To get the intermediary `in/ex.scala.in` file, execute the `bain` command in the `sbt` shell:

    sbt:BioAmbients[experimental]2Scala> bain -cef ex

where `example/pisc/ex.pisc` contains the bioambients source (equations binding
agents to process expressions).

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
