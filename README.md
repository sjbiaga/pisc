Stochastic Π-calculus in SCala aka PISC ala RISC
================================================

The Π-calculus maps one to one on Scala for-comprehensions
"inside" the Cats Effect's `IO[Unit]` monad.

The stochastic branch adds rates to actions in comparison with
the [Π-calculus](https://github.com/sjbiaga/pisc/tree/main).

After code generation, the Π-calculus "processes" could be
programmatically typed as Scala code using CE `IO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Channels for names (`UUID`s) work as CE tutorial's
producer/consumer but no queue, only `takers` and `offerers`.

Composition: parallel modelled with - `parMapN`.
Summation: probabilistic choice modelled with - `parMapN`.

The source code is divided in two: the parser in `Calculus.scala` and the
`Scala` source code generator in `Program.scala`.


Calculus
--------

The Π-calculus process expressions are exactly as in the literature, with
both ASCII and UTF-8 characters, and slight variations. There is no `if then else`,
but there is "match" and "mismatch". Forcibly, a _restriction_ and a _(mis)match_ are
"considered" _prefixes_, besides input/output prefixes per se.

The BNF formal grammar is the following. Lexically, `ident` is a channel name - (an
identifier) starting with lowercase letter; capital `IDENT` is an agent identifier
starting with uppercase letter. Both may contain single and double quotes.

A source file with the "`.pisc`" extension consists of equations, binding an agent identifier
with an optional list of "formal" (bound names) parameters, to a process expression. Because
the use of parentheses in a _restriction_ would lead to ambiguities, it is forced to start
with the UTF-8 character "v". "𝟎" is _inaction_ or the _empty sum_ (with empty parallel).
"𝜏" is the _silent transition_.

Lines starting with a hash `#` character are (line) comments. Blank lines are ignored.

Summation (`CHOICE`) has lower precedence than composition (`PARALLEL`).

The output prefix uses angular parentheses and has the form `NAME<NAME>.`, while
the input prefix uses the round parentheses and has the form `NAME(NAME).`. A _`name`_
in parentheses can also be a (constant) `String` literal, a (boxed in a) `BigDecimal` number,
or any `Scala` expression as a Scala comment between `/*` and `*/`.

The _`rate`_ of an action ("𝜏" or prefix) can be optionally annotated with `@`
and an infinite ("∞"), a `Scala` identifier, a (boxed in a) `BigDecimal` number,
or any `Scala` expression as a Scala comment between `/*` and `*/`.

A match has the form `[NAME=NAME]` and a mismatch the same, but
using the `NOT EQUAL TO` unicode `≠` character.

The name before parentheses must be a channel name.

Note that input/outut prefixes and the silent transition are followed by a dot,
whereas restriction and (mis)match are not.

    EQUATION   ::= AGENT "=" CHOICE
    CHOICE     ::= "(" CHOICE ")" | PARALLEL { "+" PARALLEL }
    PARALLEL   ::= "(" PARALLEL ")" | SEQUENTIAL { "|" SEQUENTIAL }
    SEQUENTIAL ::= PREFIXES [ "𝟎" | "(" CHOICE ")" | AGENT ]
    PREFIXES   ::= PREFIX { PREFIX }
    PREFIX     ::= "𝜏" [ @ RATE ] "."
	             | "v" "(" NAME ")"
	             | NAME [ @ RATE ] "<" NAME ">" "."
	             | NAME [ @ RATE ] "(" NAME ")" "."
	             | "[" NAME "=" NAME "]"
	             | "[" NAME "≠" NAME "]"
    AGENT      ::= [ QUAL ] IDENTIFIER [ "(" NAME { "," NAME } ")" ]

Not part of the original Π-calculus, an agent (call) expression - unless
it is binding in an equation -, may be preceded by a sequence of characters wrapped
between curly braces: these will be joined using the dot "`.`" character, standing for
a qualified package identifier. Thus, agents in different translated "`.scala`" files
can be reused; the lexical category is `qual`.


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

A long prefix path - "`v(x).x<5>.x(y).𝜏.x(z).z<y>.`":

    for
      _ <- IO.unit
      x <- `v`
      _ <- x("" -> BigDecimal(5))
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

The root project folder contains two files: `pi.scala` and `main.scala.in`.
!!!Warning: do not delete them!!!
One can edit'em, though they're ready to generate a main `App`.

Let's go backwards. To run an example, `cd` to `examples` and execute:

    ./examples $ scala-cli run ../loop.scala ../stats.scala ../spi.scala out/pi_example.scala --dependency org.typelevel::cats-effect:3.5.2 -S 3.4.0-RC1

To get the final source file `out/pi_example.scala`, concatenate two `.in` files:

    ./examples $ rm out/pi_example.scala; cat ../main.scala.in in/pi_example.scala.in > out/pi_example.scala

To get the intermediary `in/pi_example.scala.in` file, execute the `run` command in the `sbt` shell:

    sbt:psc> run pi_example

where `example/pisc/pi_example.pisc` contains the Π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/out/pi_example.scala` and add a top-level `package pi_example` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ scala-cli run --interactive ../loop.scala ../stats.scala ../spi.scala out/pi1.scala out/pi2.scala --dependency org.typelevel::cats-effect:3.5.2 -S 3.4.0-RC1
