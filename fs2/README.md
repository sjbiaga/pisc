BioAmbients Pi-calculus in SCala aka PISC ala RISC (experimental)
=================================================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the FS2's `Stream[_, _]` monad.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `FS2` `Stream[_, _]`
and Cats Effect `IO[_]`.

Names act as [topic](https://fs2.io/#/concurrency-primitives?id=topic)s, with
possibly multiple [publishers and multiple subscribers](https://fs2.io/#/concurrency-primitives?id=single-publisher-multiple-subscriber).

Composition: parallel modelled with - `Stream.exec(List(...).map(_.compile.drain).parSequence.void)`.

Summation: probabilistic choice modelled with - `Stream.exec(List(...).map(_.compile.drain).parSequence.void)`.

[Guarded] Replication: modelled with infinite streams.


Program
-------

The crucial observation is that a replication output guard gives rise to an _infinite_
"stream" of values. A second observation, just as important, is that, in this new
perspective, a sequence of prefixes is just a sequence of `flatMap`s of streams: for
example, the expression `!.a(b). !.b<c>.` can be viewed simply as a `flatMap` of two
infinite streams, rather than a prefix and the spawning of a fiber upon communication
on the prefix.

A third observation is that it suffices that subscribers and publishers run in parallel
in order for topics to publish events to subscribers. But this is exactly what parallel
composition means in π-calculus. So, for example the expression `(!.a(b).) | (!.a<c>.)`
will lead to the infinite publisher stream `!.a<c>.` emit in parallel with the infinite
subscriber stream `!.a(b).`

Fourth, all that needs taken care of is that there should be no output on a
"channel" (which herein is a `Topic`) yet, lest there is a subscription. Hence, fifth,
there is no difference between a subscription of a replication input guard and of
an input prefix. There is, however a difference between a replication input guard like
`!.a(b).` and a replication with an input prefix like `! a(b).`: the former corresponds
to just one subscription, while the latter - to many; also in the case of a recursive
agent like `P(a) = a(b). P(a)`.

Awaiting (again) for a subscription is achieved using a `Queue[_, Unit]`. An infinite
input stream must therefore ensure fairness by enqueuing after each received element.
Output prefixes that await subscription are always thus started.


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

To get and run the examples, one can `source` the functions from `bin/ba.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ ba -fs2 ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ baio -fs2 ex

To get the intermediary `in/ex.scala.in` file, execute the `spin` command in the `sbt` shell:

    sbt:BioAmbients π-Calculus[experimental]2Scala> bain -fs2 ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
