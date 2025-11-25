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

One may intercalate "`println`"s:

    for
      x      <- ν
      _      <- IO.println(s"new x=$x")
      t      <- x(⊤(1), BigDecimal(5))("2b9b3d1a-9b17-4c3f-b126-268ec639a8a7")
      _      <- IO.println(s"passive output duration = $t")
      (y, _) <- x(⊤(1L))("eaab7d89-cf7e-4286-95aa-35adb187df55")
      _      <- IO.println("input x(y)")
      t      <- τ(`ℝ⁺`(BigDecimal(1))("e34022d6-89f5-4148-92ba-f471db56749b"))
      _      <- IO.println(s"silent transition duration = $t")
      (z, t) <- x(∞(1L))("8ce85b1d-d213-442d-8520-68f0f1db25af")
      _      <- IO.println(s"immediate input duration = $t")
      _      <- z(⊤(1L), y)("d998269b-9edf-4129-9921-ab8647f3d6d1")
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
named "`_<uuid>`" to translate lazily `! . π . P` as:

    for
      _<uuid> <- IO {
        lazy val _<uuid>: String => IO[Any] = { implicit ^ =>
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
		  ).parSequence
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


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `ce` contains five files: `dump.scala`, `loop.scala`, `stats.scala`, `ba.scala`,
and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/ba.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ ba -ce ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ baio -ce ex

To get the intermediary `in/ex.scala.in` file, execute the `bain` command in the `sbt` shell:

    sbt:BioAmbients[experimental]2Scala> bain -ce ex

where `example/pisc/ex.pisc` contains the bioambients source (equations binding
agents to process expressions).

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
