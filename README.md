Π-calculus in SCala aka PISC ala RISC
=====================================

The Π-calculus maps one to one on Scala for-comprehensions
"inside" the Cats Effect's `IO[Unit]` monad.

[Stochastic Π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic) is still experimental.

After code generation, the Π-calculus "processes" could be
programmatically typed as Scala code using CE `IO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Channels for names (`UUID`s) work as [CE tutorial](https://typelevel.org/cats-effect/docs/tutorial)'s
producer/consumer but no queue, only `takers` and `offerers`.

Composition: parallel modelled with - `parMapN`.
Summation: non-deterministic choice modelled with - `IO.race` and `Semaphore`.
Replication: modelled with - `parMapN` and `lazy val`.

The source code is divided in two: the parser in `Calculus.scala` and the
`Scala` source code generator in `Program.scala`.


Calculus
--------

The Π-calculus process expressions are exactly as in the literature, with
both ASCII and UTF-8 characters, and slight variations. There is "match" and
"mismatch", but also there is `if then else` or the sugared Elvis operator.
Forcibly, _restriction_ is "considered" a _prefix_, besides input/output
prefixes per se.

The BNF formal grammar is the following. Lexically, `ident` is a channel name - (an
identifier) starting with lowercase letter; capital `IDENT` is an agent identifier
starting with uppercase letter. Both may contain single and double quotes.

A source file with the "`.pisc`" extension consists of equations, binding an agent identifier
with an optional list of "formal" (bound names) parameters, to a process expression. Because
the use of parentheses in a _restriction_ would lead to ambiguities, it is forced to start
with the UTF-8 character "ν". "𝟎" is _inaction_ or the _empty sum_ (with empty parallel).
"τ" is the _silent transition_.

Lines starting with a hash `#` character are (line) comments. Blank lines are ignored.
Lines starting with an `@` character are intermixed as `Scala` code. Lines ending with
backslash continue the previous line (folding from the empty string).

Summation (`CHOICE`) has lower precedence than composition (`PARALLEL`).

The output prefix uses angular parentheses and has the form `NAME<NAME>.`, while
the input prefix uses the round parentheses and has the form `NAME(NAME).`. A _`name`_
in parentheses can also be a (constant) `String` literal, a (boxed in a) `BigDecimal` number,
or any `Scala` expression as a Scala comment between `/*` and `*/`.

A match has the form `[NAME=NAME]` and a mismatch the same, but
using the `NOT EQUAL TO` unicode `≠` character. `NAME=NAME` or `NAME≠NAME` is a
_test_, that can be used also as `if NAME(=|≠)NAME then CHOICE else CHOICE` or
as the syntactic sugar `NAME(=|≠)NAME ? CHOICE : CHOICE` Elvis ternary operator.

Stack safe is the _replication_ unary operator `! CHOICE`.

The name before parentheses (angular or round) must be a channel name.

Note that input/outut prefixes and the silent transition are followed by a dot,
whereas restriction is not; also, inaction, agent call, (mis)match, `if then else`
and replication are "leaves".

    EQUATION   ::= AGENT "=" CHOICE
    CHOICE     ::= "(" CHOICE ")" | PARALLEL { "+" PARALLEL }
    PARALLEL   ::= "(" PARALLEL ")" | SEQUENTIAL { "|" SEQUENTIAL }
    SEQUENTIAL ::= PREFIXES [ LEAF | "(" CHOICE ")" ]
    PREFIXES   ::= PREFIX { PREFIX }
    PREFIX     ::= Π "."
                 | "ν" "(" NAME ")"
    Π          ::= "τ"
                 | NAME "<" NAME ">"
                 | NAME "(" NAME ")"
    LEAF       ::= "𝟎"
                 | AGENT
                 | "[" NAME ("="|"≠") NAME "]" CHOICE
                 | "if" NAME ("="|"≠") NAME "then" CHOICE "else" CHOICE
                 | NAME ("="|"≠") NAME "?" CHOICE ":" CHOICE
                 | "!" CHOICE
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
      x <- ν
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

A long prefix path - "`ν(x).x<5>.x(y).τ.x(z).z<y>.`":

    for
      _ <- IO.unit
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
      _ <- IO.unit
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

Each replication operator uses the same variable pattern
named `pi` to translate lazily `! P` as:

    for
      .
      .
      .
      pi <- IO {
        lazy val `<uuid>`: IO[Unit] =
          for
            _ <- (
                   .  // P
                   .
                   .
                 ,
                   for
                     _ <- IO.unit
                     _ <- `<uuid>`
                   yield
                     ()
                 ).parMapN { (_, _) => }
          yield
            ()
        `<uuid>`
      }
      _ <- pi
    yield
      ()

where `<uuid>` is some generated `java.util.UUID`.

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

Let's go backwards. But first, let's assume there is a shell (`bash`) function "`pi`":

    function pi() {
        set "$@" ../pi.scala
        ~/.local/share/coursier/bin/scala-cli "$@"                                             \
                                              -S 3.4.0-RC1                                     \
                                              --dependency org.typelevel::cats-effect:3.5.2
    }

To run an example, `cd` to `examples` and execute:

    ./examples $ pi run ex.scala

To get the final source file `ex.scala`, run `scalafmt` on the `.out` files:

    ./examples $ rm out/ex.scala; cat out/ex.scala.out | scalafmt --stdin --stdout > ex.scala

To get the intermediary `out/ex.scala.out`, concatenate two `.in` files:

    ./examples $ rm out/ex.scala.out; { cat ../main.scala.in; cat in/ex.scala.in | sed -e 's/^/  /'; } > out/ex.scala.out

These two steps can be put in a shell (`bash`) function "`pio`"

    function pio() {
        while [ $# -gt 0 ]
        do
            rm out/"$1".scala.out; { cat ../main.scala.in; cat in/"$1".scala.in | sed -e 's/^/  /'; } > out/"$1".scala.out
            rm "$1".scala; cat out/"$1".scala.out |
            scalafmt --stdin --stdout |
            sed 's/for[ ][(][_][ ][<][-][ ]IO[.]unit[)][ ]yield[ ][(][)]/`𝟎`/g' > "$1".scala
            shift
        done
    }

To get the first `in/ex.scala.in` file, execute the `run` command in the `sbt` shell:

    sbt:pisc> run ex

where `example/pisc/ex.pisc` contains the Π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ pi run --interactive ex1.scala ex2.scala
