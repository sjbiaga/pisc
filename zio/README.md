Polyadic Pi-calculus in SCala aka PISC ala RISC
===============================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the ZIO's `UIO[_]` monad.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `ZIO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Composition: parallel modelled with - `ZIO.collectAllParDiscard(List(...))`.

Summation: non-deterministic choice modelled with - a `semaphore: Semaphore[UIO]` and `List(...).map(_.whenZIO(semaphore.tryAcquire)).πcollectAllPar`.

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

A long prefix path - "`ν(x).x<5>.x(y).τ.x(z).z<y>.`":

    for
      x <- ν
      _ <- x(BigDecimal(5))
      y <- x()
      _ <- τ
      z <- x()
      _ <- z(y)
      .
      .
      .
    yield
      ()

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
named `_<uuid>` to translate lazily `! P` as:

    for
      _<uuid> <- ZIO.succeed {
        lazy val _<uuid>: UIO[Any] =
          List(
            .  // P
            .
            .
          ,
            for
              _ <- ZIO.unit
              _ <- _<uuid>
            yield
              ()
          )
          .πcollectAllPar
        _<uuid>
      }
      _ <- _<uuid>
    yield
      ()

where `uuid` is some generated `java.util.UUID`.

Agent identifiers (literals) start with uppercase, while
channel names start with lowercase.


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `zio` contains three files: `ppi.scala`, `ppi_.scala`, and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/ppi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ ppi -zio ex.scala

or - if stopping output prefix replication -, add an underscore:

    ./examples $ ppi_ -zio ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ ppio -zio ex

To get the intermediary `in/ex.scala.in` file, execute the `ppin` command in the `sbt` shell:

    sbt:Polyadic π-Calculus2Scala> ppin -zio ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ ppi -zio --interactive ex1.scala ex2.scala

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
