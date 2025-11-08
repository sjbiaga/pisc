Pi-calculus in SCala aka PISC ala RISC (experimental)
=====================================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the Cats Effect's `IO[_]` monad.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `CE` `IO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Channels for names work as [CE tutorial](https://typelevel.org/cats-effect/docs/tutorial)'s
producer/consumer but no queue, only `takers` and `offerers`.

Composition: parallel modelled with - `List(...).parSequence.void`.

Summation: non-deterministic choice modelled with - `parSequence` and `Semaphore.tryAcquire.ifM`.

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

One can intercalate "`println`"s:

    for
      x <- ν
      _ <- IO.println(s"new x=$x")
      _ <- x(5)
      _ <- IO.println("output x(5)")
      y <- x()
      _ <- IO.println("input x(y)")
      _ <- τ
      _ <- IO.println("silent transition")
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
      _ <- if !(x == y) then IO.unit else
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
      _<uuid> <- IO {
        lazy val _<uuid>: IO[Any] =
          NonEmptyList
            .fromListUnsafe(
              List(
                .  // P
                .
                .
              ,
                for
                  _ <- IO.unit
                  _ <- _<uuid>
                yield
                  ()
              )
            )
            .parSequence
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

The root project folder `ce` contains three files: `pi.scala`, `pi_.scala`, and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/pi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ pi -ce ex.scala

or - if stopping output prefix replication -, add an underscore:

    ./examples $ pi_ -ce ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ pio -ce ex

To get the intermediary `in/ex.scala.in` file, execute the `run` command in the `sbt` shell:

    sbt:π-Calculus[experimental]2Scala> run ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ pi -ce --interactive ex1.scala ex2.scala

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
