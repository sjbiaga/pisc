Pi-calculus in SCala aka PISC ala RISC
======================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the Cats' `OptionT[F, _]` monad transformer.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Composition: parallel modelled with - `List(...).parSequenceVoid`.

Summation: non-deterministic choice modelled with - `parSequenceVoid` and `OptionT.when(Semaphore.tryAcquire)(...)`.

[Guarded] Replication: modelled with - `parSequenceVoid` and `lazy val` [or `def`].


Program
-------

Agent identifiers (literals) start with uppercase, while
channel names start with lowercase.


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `ce` contains three files: `pi.scala`, `pim.scala`, and `IO.main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/pi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ pi -cot ex.scala

or - if stopping output prefix replication -, add an underscore:

    ./examples $ pi_ -cot ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ pio -cot ex

To get the intermediary `in/ex.scala.in` file, execute the `pin` command in the `sbt` shell:

    sbt:π-Calculus2Scala> pin -cot ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ pi -cot --interactive ex1.scala ex2.scala

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
