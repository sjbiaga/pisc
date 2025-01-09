Polyadic Pi-calculus in SCala aka PISC ala RISC
===============================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the Cats Effect's `IO[_]` monad.

Asynchronous [Polyadic π-calculus](https://github.com/sjbiaga/pisc/tree/polyadic-async) is a variant.
[Stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic) is in alpha stage.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `CE` `IO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Channels for names work as [CE tutorial](https://typelevel.org/cats-effect/docs/tutorial)'s
producer/consumer but no queue, only `takers` and `offerers`.

Composition: parallel modelled with - `NonEmptyList.fromListUnsafe(...).parTraverse(identity)`.

Summation: non-deterministic choice modelled with - `parTraverse` and `Semaphore.tryAcquire.ifM`.

[Guarded] Replication: modelled with - `parTraverse` and `lazy val` [or `def`].

The source code is divided in two: the parser in `Calculus.scala` and the
`Scala` source code generator in `Program.scala`.


Calculus
--------

The π-calculus process expressions are exactly as in the literature, with
both ASCII and UTF-8 characters, and slight variations. There is "match" and
"mismatch", but also there is `if then else` or the sugared Elvis operator.
Forcibly, _restriction_ is "considered" a _prefix_, besides input/output
prefixes per se.

The BNF formal grammar for processes is the following.

    LINE           ::= EQUATION | DEFINITION
    EQUATION       ::= INVOCATION "=" CHOICE
    DEFINITION     ::= "⟦<CODE>" [ TEMPLATE ] "<CODE>⟧" PARAMS [ POINTERS ] "=" CHOICE
    CHOICE         ::= PARALLEL { "+" PARALLEL }
    PARALLEL       ::= SEQUENTIAL { "|" SEQUENTIAL }
    SEQUENTIAL     ::= PREFIXES [ LEAF | "(" CHOICE ")" ]
    LEAF           ::= "[" NAME ("="|"≠") NAME "]" CHOICE
                     | "if" NAME ("="|"≠") NAME "then" CHOICE "else" CHOICE
                     | NAME ("="|"≠") NAME "?" CHOICE ":" CHOICE
                     | "!" [ "." μ "." ] CHOICE
                     | CAPITAL
                     | INVOCATION
                     | INSTANTIATION
    CAPITAL        ::= IDENTIFIER [ "(" [ NAMES ] ")" ] ( POINTERS | "{" "}" )
    INSTANTIATION  ::= "⟦<CODE>" INSTANCE "<CODE>⟧" [ POINTERS ]
    INVOCATION     ::= [ QUAL ] IDENTIFIER PARAMS
    PARAMS         ::= [ "(" NAMES ")" ]
    POINTERS       ::= "{" NAMES "}"
    NAMES          ::= NAME { "," NAME }

The BNF formal grammar for prefixes is the following.

    PREFIXES       ::= { PREFIX }
    PREFIX         ::= "ν" "(" NAMES ")"
                     | μ "."
    μ              ::= "τ" [ EXPRESSION ]
                     | NAME "<" NAMES ">" [ EXPRESSION ]
                     | NAME ARITY "<" ">" [ EXPRESSION ]
                     | NAME "(" NAMES ")" [ EXPRESSION ]
    ARITY          ::= "#" NATURAL_NUMBER
    EXPRESSION     ::= "/*" ... "*/"

Lexically, `ident` is a channel name - (an identifier) starting with lowercase letter;
capital `IDENT` is an agent identifier starting with uppercase letter. Both may contain
single and double quotes.

A source file with the "`.pisc`" extension consists of equations, binding an agent identifier
with an optional list of "formal" (bound names) parameters, to a process expression. Because
the use of parentheses in a _restriction_ would lead to ambiguities, it is forced to start
with the UTF-8 character "ν". "()" is _inaction_ or the _empty sum_.
"τ" is the _silent transition_.

Lines starting with a hash `#` character are (line) comments. Blank lines are ignored.
Lines starting with an `@` character are intermixed as `Scala` code. Lines ending with
backslash continue on the next line.

Summation (`CHOICE`) has lower precedence than composition (`PARALLEL`).

The output prefix uses angular parentheses and has the form `NAME<NAMES>.`, while
the input prefix uses the round parentheses and has the form `NAME(NAMES).`. A _`name`_
in parentheses can also be a (constant) `String` literal, a (boxed in a) `BigDecimal` number,
or a [`Scalameta`](https://scalameta.org) term as a `Scala` comment between `/*` and `*/`.
The polyadic version allows multiple names separated by comma between parentheses.

A match has the form `[NAME=NAME]` and a mismatch the same, but
using the `NOT EQUAL TO` Unicode `≠` character. `NAME=NAME` or `NAME≠NAME` is a
_test_, that can be used also as `if NAME(=|≠)NAME then CHOICE else CHOICE` or
as the syntactic sugar `NAME(=|≠)NAME ? CHOICE : CHOICE` Elvis ternary operator.

Stack safe is the [guarded] _replication_ unary operator `! [ "." μ "." ] CHOICE`;
the guard `"." μ "."` is optional, and it starts with a `"."` so that it is
distinguished from other prefixes.

The name before parentheses (angular or round) must be a channel name.

For output, the names in angular parentheses are optional, if empty being `null`.
This may be used to cease guarded replication with _input_ prefix guard: i.e.,
if `null`s are received, the (stack-safe, recursive) replication stops.

For polyadic π-calculus, the absence of arguments mandates the channel arity
be specified (with "#").

Note that input/output prefixes and the silent transition are followed by a dot,
whereas restriction is not; also, inaction, invocation, (mis)match, `if then else`,
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
`Scalameta` function-term as a `Seq[Any] => IO[Seq[Any]]` piece of code,
that is applied the prior result from the input, obtaining the `IO[Seq[Any]]` that is
executed under supervision (so that cancellation does not outlive the
`cats.effect.std.Supervisor[IO]`) providing the actual result, just after ending
the input but before returning from it.

If `null`s are received, that function will not run. Otherwise, if its results are
`null`s, this may be used to cease guarded replication with _output_ prefix guard:
i.e., should just this one input prefix remain, the (stack-safe, recursive)
replication stops.

And if, for each guarded replication, care is taken of to *stop* each, then the
entire program exits; unless prevented by non-exiting - self or mutual - recursive
agent invocations or unguarded replication.

Not part of the original π-calculus, an (agent) invocation expression - unless
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

The inaction - `IO.Unit`.

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
            .parTraverse(identity)
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

To get and run the examples, one can `source` the functions from `bin/ppi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ ppi ex.scala

or - if stopping output prefix replication -, add an underscore:

    ./examples $ ppi_ ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ ppio ex

To get the intermediary `in/ex.scala.in` file, execute the `run` command in the `sbt` shell:

    sbt:Polyadic π-Calculus2Scala> run ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ ppi --interactive ex1.scala ex2.scala

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
