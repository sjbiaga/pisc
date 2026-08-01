BioAmbients Pi-calculus in SCala aka BASC ala RISC
==================================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the ZIO's `ZStream[_, _, _]` monad.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `ZIO` `ZStream[_, _, _]`
and `Task[_]`.

Composition: parallel modelled with - `ZStream.fromZIO(ZIO.collectAllParDiscard(List(...).map(_.runDrain)))`.

Summation: probabilistic choice modelled with - `ZStream.fromZIO(ZIO.collectAllParDiscard(List(...).map(_.runDrain)))`.

[Guarded] Replication: modelled with infinite streams.


Program
-------

The crucial observation is that a replication output guard gives rise to an _infinite_
"stream" of values. A second observation, just as important, is that, in this new
perspective, a sequence of prefixes is just a sequence of `flatMap`s of streams: for
example, the expression `!.a(b). !.b<c>.` can be viewed simply as a `flatMap` of two
infinite streams, rather than a prefix and the spawning of a fiber upon communication
on the prefix.

The check for ambients' condition required by either a pair of communication or capability
prefixes is _asynchronous_. This means that the checks for several such pairs happen in
parallel, in different background fibers, and these are all in contention simultaneously,
which would not have been the case had the detection of the list of the pairs blocked instead
with each pair.

Hence, the map of enabled actions/capabilities may be confronted successively, without blocking
each time, and this may engender more background fibers which might unlock possible livelocks on
ambients' conditions.

However, in the case of linear replication, when an action/capability always resides in the map
and is enabled depending on a boolean flag, care must be taken to turn that flag off before the
server fiber finishes, because otherwise the server loop might detect faster the same enabled
action/capability more than once.

If everything goes well, the background fiber finishes and still the client fiber may be faster,
and re-enable the same action/capability by turning that flag on just _before_ it is turned off by
the server fiber.

Thus, there must be a strict order, that the turning of the flag off occurs before the background
fiber finishes (which the client fiber awaits on by `join`ing it). A cyclic barrier does the job.


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `zs` contains four files: `dump.scala`, `loop.scala`, `stats.scala`, and `ba.scala`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/ba.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ ba -zs ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ baio -zs ex

To get the intermediary `in/ex.scala.in` file, execute the `bain` command in the `sbt` shell:

    sbt:BioAmbients2Scala> bain -zs ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
