Pi-calculus in SCala aka PISC ala RISC
======================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the Cats Effect's `IO[Unit]` monad.

[Stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic) is in alpha stage.
[Polyadic π-calculus](https://github.com/sjbiaga/pisc/tree/polyadic) is also supported.
[Ambient Calculus](https://github.com/sjbiaga/pisc/tree/ambient) is nicely done, too. In a
similar way - somehow combining π-calculus with ambients - is implemented
[π-calculus with transactions](https://github.com/sjbiaga/pisc/tree/transactions).

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `CE` `IO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Channels for names work as [CE tutorial](https://typelevel.org/cats-effect/docs/tutorial)'s
producer/consumer but no queue, only `takers` and `offerers`.

Composition: parallel modelled with - `parMapN`.

Summation: non-deterministic choice modelled with - `parMapN` and `Semaphore.tryAcquire`.

[Guarded] Replication: modelled with - `parMapN` and `lazy val` [or `def`].

The source code is divided in two: the parser in `Calculus.scala` and the
`Scala` source code generator in `Program.scala`.


Calculus
--------

The π-calculus process expressions are exactly as in the literature, with
both ASCII and UTF-8 characters, and slight variations. There is "match" and
"mismatch", but also there is `if then else` or the sugared Elvis operator.
Forcibly, _restriction_ is "considered" a _prefix_, besides input/output
prefixes per se.

The BNF formal grammar is the following.

    EQUATION   ::= AGENT "=" CHOICE
    CHOICE     ::= PARALLEL { "+" PARALLEL }
    PARALLEL   ::= SEQUENTIAL { "|" SEQUENTIAL }
    SEQUENTIAL ::= PREFIXES [ LEAF | "(" CHOICE ")" ]
    PREFIXES   ::= { PREFIX }
    PREFIX     ::= μ "."
                 | "ν" "(" NAME ")"
    μ          ::= "τ" [ EXPRESSION ]
                 | NAME "<" [ NAME ] ">" [ EXPRESSION ]
                 | NAME "(" NAME ")" [ EXPRESSION ]
    LEAF       ::= "𝟎"
                 | AGENT
                 | "[" NAME ("="|"≠") NAME "]" CHOICE
                 | "if" NAME ("="|"≠") NAME "then" CHOICE "else" CHOICE
                 | NAME ("="|"≠") NAME "?" CHOICE ":" CHOICE
                 | "!" [ "." μ "." ] CHOICE
    AGENT      ::= [ QUAL ] IDENTIFIER [ "(" ")" | "(" NAME { "," NAME } ")" ]
    EXPRESSION ::= "/*" ... "*/"

Lexically, `ident` is a channel name - (an identifier) starting with lowercase letter;
capital `IDENT` is an agent identifier starting with uppercase letter. Both may contain
single and double quotes.

A source file with the "`.pisc`" extension consists of equations, binding an agent identifier
with an optional list of "formal" (bound names) parameters, to a process expression. Because
the use of parentheses in a _restriction_ would lead to ambiguities, it is forced to start
with the UTF-8 character "ν". "𝟎" is _inaction_ or the _empty sum_ (with empty parallel).
"τ" is the _silent transition_.

Lines starting with a hash `#` character are (line) comments. Blank lines are ignored.
Lines starting with an `@` character are intermixed as `Scala` code. Lines ending with
backslash continue on the next line.

Summation (`CHOICE`) has lower precedence than composition (`PARALLEL`).

The output prefix uses angular parentheses and has the form `NAME<NAME>.`, while
the input prefix uses the round parentheses and has the form `NAME(NAME).`. A _`name`_
in parentheses can also be a (constant) `String` literal, a (boxed in a) `BigDecimal` number,
or a [`Scalameta`](https://scalameta.org) term as a `Scala` comment between `/*` and `*/`.

A match has the form `[NAME=NAME]` and a mismatch the same, but
using the `NOT EQUAL TO` Unicode `≠` character. `NAME=NAME` or `NAME≠NAME` is a
_test_, that can be used also as `if NAME(=|≠)NAME then CHOICE else CHOICE` or
as the syntactic sugar `NAME(=|≠)NAME ? CHOICE : CHOICE` Elvis ternary operator.

Stack safe is the [guarded] _replication_ unary operator `! [ "." μ "." ] CHOICE`;
the guard `"." μ "."` is optional, and it starts with a `"."` so that it is
distinguished from other prefixes.

The name before parentheses (angular or round) must be a channel name.

For output, the name in angular parentheses is optional, if empty being `null`.
This may be used to cease guarded replication with _input_ prefix guard: i.e.,
if a `null` is received, the (stack-safe, recursive) replication stops.

Note that input/output prefixes and the silent transition are followed by a dot,
whereas restriction is not; also, inaction, agent call, (mis)match, `if then else`,
Elvis operator, and replication are "leaves".

Between "τ" and "." in a silent transition, there can be a `Scalameta` term for
which a `for` generator `_ <- IO { term }` is inserted _after_ the transition,
or any list of `Enumerator`s which are added _after_ the transition. Any symbol
that is found in these terms is considered a _free_ name.

Between output prefix closing angular parenthesis and ".", there can be a
`Scalameta` term as an `IO[Any]` piece of code, that is executed under supervision
(so that cancellation does not outlive the `cats.effect.std.Supervisor[IO]`),
just after ending the output but before returning from it.

Between input prefix closing round parenthesis and ".", there can be a
`Scalameta` function-term as a `T`-parameterized `T => IO[T]` piece of code,
that is applied the prior result from the input, obtaining the `IO[T]` that is
executed under supervision (so that cancellation does not outlive the
`cats.effect.std.Supervisor[IO]`) providing the actual result, just after ending
the input but before returning from it.

If `null` is received, that function will not run. Otherwise, if its result is
`null`, this may be used to cease guarded replication with _output_ prefix guard:
i.e., should just this one input prefix remain, the (stack-safe, recursive)
replication stops.

And if, for each guarded replication, care is taken of to *stop* each, then the
entire program exits; unless prevented by non-exiting - self or mutual - recursive
agent calls or unguarded replication.

Not part of the original π-calculus, an agent (call) expression - unless
it is binding in an equation -, may be preceded by a sequence of characters wrapped
between curly braces: these will be joined using the dot "`.`" character, standing for
a qualified package identifier. Thus, agents in different translated "`.scala`" files
can be reused; the lexical category is `qual`.

Unlike the rest of the agents, the `Main` agent has the command line arguments
spliced as `vararg` parameter(s).


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
        lazy val _<uuid>: IO[Unit] =
          (
            .  // P
            .
            .
          ,
            for
              _ <- IO.unit
              _ <- _<uuid>
            yield
              ()
          ).parMapN { (_, _) => }
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

The root project folder contains three files: `pi.scala`, `pi_.scala`, and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/pi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ pi ex.scala

or - if stopping output prefix replication -, add an underscore:

    ./examples $ pi_ ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ pio ex

To get the intermediary `in/ex.scala.in` file, execute the `run` command in the `sbt` shell:

    sbt:π-Calculus2Scala> run ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ pi --interactive ex1.scala ex2.scala

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.

DotArrow
--------

[`DotArrow`](https://github.com/sjbiaga/pisc-dotarrow) is the codename
for "_mobile code_". It is implemented in a very simplistic fashion.
