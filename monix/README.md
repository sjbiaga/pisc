Pi-calculus in SCala aka PISC ala RISC (experimental)
=====================================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the Monix's `Iterant[_, _]` monad.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `Monix` `Iterant[_, _]`
and either Cats Effect `IO[_]` _or_ Monix `Task[_]`.

Names act as [concurrent channel](https://monix.io/api/current/monix/catnap/ConcurrentChannel.html)s, with
possibly multiple producers and multiple consumers (`MPMC`).

Composition: parallel modelled with - `Iterant.liftF(Observable.mapParallelUnorderedF(Int.MaxValue)(_.completedL).completedF[F])`.

Summation: non-deterministic choice modelled with - `Iterant.liftF(self.mapParallelUnorderedF(Int.MaxValue)(Iterant.liftF(semaphore.tryAcquire).ifM(_, Iterant.empty).completedL).completedF[F])`.

[Guarded] Replication: modelled with infinite streams.


Program
-------

The crucial observation is that a replication output guard gives rise to an _infinite_
"stream" of values. A second observation, just as important, is that, in this new
perspective, a sequence of prefixes is just a sequence of `flatMap`s of streams: for
example, the expression `!.a(b). !.b<c>.` can be viewed simply as a `flatMap` of two
infinite streams, rather than a prefix and the spawning of a fiber upon communication
on the prefix.

A third observation is that it suffices that consumers and producers run in parallel
in order for channels to push events to consumers. But this is exactly what parallel
composition means in π-calculus. So, for example the expression `(!.a(b).) | (!.a<c>.)`
will lead to the infinite producer stream `!.a<c>.` emit in parallel with the infinite
consumer stream `!.a(b).`

Fourth, all that needs taken care of is that there should be no output on a
"channel" (which herein is a `ConcurrentChannel`) yet, lest there is a subscription. Hence, fifth,
there is no difference between a subscription of a replication input guard and of
an input prefix. There is, however a difference between a replication input guard like
`!.a(b).` and a replication with an input prefix like `! a(b).`: the former corresponds
to just one subscription, while the latter - to many; also in the case of a recursive
agent like `P(a) = a(b). P(a)`.

Awaiting (again) for a subscription is achieved using a `ConcurrentQueue[_, Unit]`. An infinite
input stream must therefore ensure fairness by enqueuing after each received element.
Output prefixes that await subscription are always thus started.


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `monix` contains four files: `pi.scala`, `pi_.scala`, `IO.main.scala.in`, and `Task.main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/pi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ pi -monix ex.scala

or - if stopping output prefix replication -, add an underscore:

    ./examples $ pi_ -monix ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run either:

    ./examples $ pio -monix ex

_or_:

    ./examples $ pio -monix -Fmonix.eval.Task ex

To get the intermediary `in/ex.scala.in` file, execute the `pin` command in the `sbt` shell:

    sbt:π-Calculus[experimental]2Scala> pin -monix ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ pi -monix --interactive ex1.scala ex2.scala

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
