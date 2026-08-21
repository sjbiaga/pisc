BioAmbients in SCala aka BASC ala RISC
======================================

BioAmbients maps one to one on `Scala` for-comprehensions
"inside" the ZIO's `UIO[_]` monad.

The bioambients branch adds capabilities in comparison with
the [stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic).
This branch uses _cancellation_ to discard actions/capabilities.
Another [branch](https://github.com/sjbiaga/pisc/tree/bioambients-flatMap)
heavily uses `flatMap`'s and comparison with `null` to discard actions/capabilities.

After code generation, the bioambients "processes" could be
programmatically typed as `Scala` code using `ZIO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Composition: parallel modelled with - `ZIO.collectAllParDiscard(List(...))`.

Summation: *probabilistic* choice modelled with - `ZIO.collectAllParDiscard(List(...))`.

[Guarded] Replication: modelled with - `collectAllParDiscard` and `lazy val` [or `def`].


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

The inaction - `ZIO.unit`.

A long prefix path - "`v(x).x!{5}.x?{y}.τ@(1).x@∞?{z}.z!{y}.`":

    for
      x      <- ν
      _      <- x(⊤(1L), BigDecimal(5))("2b9b3d1a-9b17-4c3f-b126-268ec639a8a7")
      (y, _) <- x(⊤(1L))("eaab7d89-cf7e-4286-95aa-35adb187df55")
      _      <- τ(`ℝ⁺`(BigDecimal(1))("e34022d6-89f5-4148-92ba-f471db56749b"))
      (z, _) <- x(∞(1L))("8ce85b1d-d213-442d-8520-68f0f1db25af")
      _      <- z(⊤(1L), y)("d998269b-9edf-4129-9921-ab8647f3d6d1")
      .
      .
      .
    yield
      ()

Note that `UUID` second argument is absent.

A [mis]match `[x = y] P` translates as:

    for
      .
      .
      .
      _ <- if !(x == y) then ZIO.unit else
           for
             . // P
             .
             .
           yield
             ()
    yield
      ()

An `if then else` translates `if x = y then P else Q` as:

    for
      .
      .
      .
      _ <- ( if (x == y)
             then
               for // P
                 .
                 .
                 .
               yield
                 ()
             else
               for // Q
                 .
                 .
                 .
               yield
                 ()
           )
    yield
      ()

Each replication operator uses a unique variable pattern
named "`_<uuid>`" to translate lazily `! . π . P` as:

    for
      _<uuid> <- ZIO.succeed {
        lazy val _<uuid>: String => UIO[Any] = { implicit ^ =>
          List(
            .  // P
            .
            .
          ,
            for
              π
              _ <- _<uuid>(`π-uuid`)
            yield
              ()
          ).πcollectAllPar
        }
        <uuid>
      }
      π
      _ <- _<uuid>(`π-uuid`)
    yield
      ()

where "`uuid`" is some generated `java.util.UUID`.

Agent identifiers (literals) start with uppercase, while
channel names start with lowercase.

The check for ambients' condition required by either a pair of communication or capability
prefixes is _asynchronous_. This means that the checks for several such pairs happen in
parallel, in different background fibers, and these are all in contention simultaneously,
which would not have been the case had the detection of the list of the pairs blocked instead
with each pair.

Hence, the map of enabled actions/capabilities may be confronted successively, without blocking
each time, and this may engender more background fibers which might unlock possible livelocks on
ambients' conditions.


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `zio` contains six files: `dump.scala`, `loop.scala`, `stats.scala`,
`traces.scala`, `ba.scala`, and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/ba.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ ba -zio ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ baio -zio ex

To get the intermediary `in/ex.scala.in` file, execute the `bain` command in the `sbt` shell:

    sbt:BioAmbients2Scala> bain -zio ex

where `example/pisc/ex.pisc` contains the bioambients source (equations binding
agents to process expressions).

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
