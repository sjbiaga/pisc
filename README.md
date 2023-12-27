Π-calculus in SCala aka PISC ala RISC
=====================================

The Π-calculus maps one to one on Scala for-comprehensions
"inside" the Cats Effect's `IO[Unit]` monad.

After code generation, the Π-calculus "processes" could be
programmatically typed as Scala code using CE `IO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Channels for names (`UUID`s) work as CE tutorial's
producer/consumer but no queue, only `takers` and `offerers`.

Composition: parallel modelled with - `parMapN`.
Summation: non-deterministic choice modelled with - `IO.race`.

Calculus
--------

Program
-------

A new name - will be available in the Scala scope:

    for
      _ <- IO.unit
      x <- `v`
      .
      .
      .
    yield
      ()

The inaction - just the `Unit` value () after yield:

    for
      _ <- IO.unit
      .
      .
      .
    yield
      ()

(That's why `for` always starts with `_ <- IO.unit`.)

A long prefix path - "`vx.x<5>.x(y).𝜏.x(z).z<y>.`":

    for
      _ <- IO.unit
      x <- `v`
      _ <- x("30408bc7-3d40-415d-9a3b-3ea2c6be7f94" -> BigDecimal(5))
      y <- x()
      _ <- `𝜏`
      z <- x()
      _ <- z(y)
      .
      .
      .
    yield
      ()

One can intercalate "`println`"s:

    for
      _ <- IO.unit
      x <- `v`
      _ <- IO.println(s"new x=$x")
      _ <- x(5)
      _ <- IO.println("output x(5)")
      y <- x()
      _ <- IO.println("input x(y)")
      _ <- `𝜏`
      _ <- IO.println("silent transition")
      z <- x()
      _ <- z(y)
      .
      .
      .
    yield
      ()

A match `[x = y] P`

    for
      .
      .
      .
      _ <- if !(x._1 == y._1) then IO.unit else
           for
             .
             .
             .
           yield
             ()
      // nothing more
    yield
      ()

Agent identifiers (literals) start with uppercase, while
channel names start with lowercase.

Apps (examples)
---------------

The `examples` folder has three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder contains a file `main.scala.in`.
!!!Warning: do not delete it!!!
One can edit it, thought it's ready to generate a main `App`.

Let's go backwards. To run an example, `cd` to `examples` and execute:

    ./examples $ scala-cli run out/pi_example.scala --dependency org.typelevel::cats-effect:3.5.2 -S 3.4.0-RC1

To get the final source file `out/pi_example.scala`, concatenate two `.in` files:

    ./examples $ rm out/pi_example.scala; cat ../main.scala.in in/pi_example.scala.in > out/pi_example.scala

To get the intermediary `in/pi_example.scala.in` file, execute the `run` command in the `sbt` shell:

    sbt:psc> run pi_example

where `example/pisc/pi_example.pisc` contains the Π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/out/pi_example.scala` and add a top-level `package pi_example` line.
