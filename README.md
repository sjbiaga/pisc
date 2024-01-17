Stochastic Pi-calculus in SCala aka PISC ala RISC
=================================================

The Π-calculus maps one to one on Scala for-comprehensions
"inside" the Cats Effect's `IO[Unit]` monad.

The stochastic branch adds rates to actions in comparison with
the [Π-calculus](https://github.com/sjbiaga/pisc/tree/main).

After code generation, the Π-calculus "processes" could be
programmatically typed as Scala code using CE `IO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Channels for names (`UUID`s) work as [CE tutorial](https://typelevel.org/cats-effect/docs/tutorial)'s
producer/consumer but no queue, only `takers` and `offerers`.

Composition: parallel modelled with - `parMapN`.
Summation: probabilistic choice modelled with - `parMapN`.
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
or a [`Scalameta`](https://scalameta.org) term as a `Scala` comment between `/*` and `*/`.

The _`rate`_ of an action ("τ" or prefix) can be optionally annotated with `@`
and an infinite ("∞"), a `Scala` identifier, a (boxed in a) `BigDecimal` number,
or any `Scala` expression as a Scala comment between `/*` and `*/`.

A match has the form `[NAME=NAME]` and a mismatch the same, but
using the `NOT EQUAL TO` unicode `≠` character. `NAME=NAME` or `NAME≠NAME` is a
_test_,that can be used also as `if NAME(=|≠)NAME then CHOICE else CHOICE` or
as the syntactic sugar `NAME(=|≠)NAME ? CHOICE : CHOICE` Elvis ternary operator.

Stack safe is the _replication_ unary operator `! Π . CHOICE`.

The name before parentheses (angular or round) must be a channel name.

Note that input/outut prefixes and the silent transition are followed by a dot,
whereas restriction is not; also, inaction, agent call, (mis)match, `if then else`
and replication are "leaves".

Between "τ" and "." in a silent transition, there can be a `Scalameta` term for
which a `for` generator `_ <- IO { term }` is inserted _after_ the transition,
or any list of `Enumerator`s which are added _after_ the transition. Any symbol
that is found in these terms is considered a _free_ name.

    EQUATION   ::= AGENT "=" CHOICE
    CHOICE     ::= "(" CHOICE ")" | PARALLEL { "+" PARALLEL }
    PARALLEL   ::= "(" PARALLEL ")" | SEQUENTIAL { "|" SEQUENTIAL }
    SEQUENTIAL ::= PREFIXES [ LEAF | "(" CHOICE ")" ]
    PREFIXES   ::= { PREFIX }
    PREFIX     ::= Π "."
                 | "ν" "(" NAME ")"
    Π          ::= "τ" [ @ RATE | EXPRESSION ]
                 | NAME [ @ RATE ] "<" NAME ">"
                 | NAME [ @ RATE ] "(" NAME ")"
    LEAF       ::= "𝟎"
                 | AGENT
                 | "[" NAME ("="|"≠") NAME "]" CHOICE
                 | "if" NAME ("="|"≠") NAME "then" CHOICE "else" CHOICE
                 | NAME ("="|"≠") NAME "?" CHOICE ":" CHOICE
                 | "!" Π "." CHOICE
    AGENT      ::= [ QUAL ] IDENTIFIER [ "(" ")" | "(" NAME { "," NAME } ")" ]
    EXPRESSION ::= "/*" ... "*/"

Not part of the original Π-calculus, an agent (call) expression - unless
it is binding in an equation -, may be preceded by a sequence of characters wrapped
between curly braces: these will be joined using the dot "`.`" character, standing for
a qualified package identifier. Thus, agents in different translated "`.scala`" files
can be reused; the lexical category is `qual`.


Stochastic
----------

The execution of a stochastic Π-calculus program is handled in the files:
`loop.scala`, `stats.scala` and `spi.scala`. The `Main` agent is called from
the final generated source file wherein `main.scala.in` was `cat`enated.

From `main.scala.in`, two fibres are launched in `background` and used as `Resource`s,
such that terminating the program cancels them. However, when a process is _discarded_,
`IO.never` is used for this situation, so the program won't exit any longer. This
is something to improve. `IO.canceled` cannot be used as it raises an exception.

Silent transitions, input and output prefixes ("actions") are associated with
an UUID (`Universally Unique IDentifier`) by inheriting the trait `Key` via the
trait `State`. The only other expression that is a `State` is... summation.

A `State` contains a `Set` of `enabled` actions `(UUIDs`). These are computed as follows.
Each silent transition, input or output prefix has (the key of) itself as the only enabled
action: these are the base cases. The enabled actions of a sequence of prefixes or
restriction, unless this sequence is empty, is that of the *first* action, if any.
If the sequence is empty or does not have actions (only restrictions), then the
enabled actions are either those of the "leaf" expression or of another processs
expression, that "`end`"s the sequence of prefixes. Both are optional, case in which
is taken to be the inaction `𝟎`, which has no enabled actions. Besides `𝟎`, other leaf
expressions are (mis)match, `if then else`, Elvis operator (all three being treated
the same), agent call and (guarded) replication.

The only case of a leaf that has enabled actions is the latter - a (guard)
non-positive polarity prefix. Otherwise, another ending process expression must
follow the sequence between a pair of parentheses. An exception is raised if
the sequence is empty and the `end`ing is none.

The set of enabled actions of a (parallel) composition is the union of each sequence's.
set of enabled actions. Finally, the set of enabled actions of a summation is the union
of each choice's set of enabled actions. Only a summation has a set of enabled actions,
and thus is a `State`. For the other two cases (sequence and composition) the set of
enabled actions is only propagated "upwards" by the parser.

The file `StochasticPi.scala` is used to create a _transition "system"_ between `State`s
for the purpose of enabling the actions in the successor state, immediately prior to
the "expiration" of the precursor enabled action. From action to consecutive action,
there is a transition, even if there are restrictions in between (picture these latter
translation in `Scala`). And finally, if there is a last action in a sequence, then
there is a transition from it to either an `end`ing summation or replication (guard);
otherwise there is no transition: but if this were the case - there is no last action -,
the set of enabled actions of a summation or a (guarded) replication, is directly and
wholly subsumed in/by another summation; and, of course, eventually, the enabled actions
of the summation (process) expression pertaining to an agent definitional equation are
immediately enabled upon its entry in any call.

The latter is the case also for the other three leaf types: (mis)match, `if then else`
and Elvis operator. For the case of guarded replication, if there is
a last action of a sequence of actions, there is a transition from that to the
guard action/prefix. But more importantly, upon the expiration of a last action from
a sequence of prefixes, if its `end` is a _summation_, the enabled actions of this
summation are enabled _before_ actions are next to be fired (in parallel).

Besides the _enabled_ actions, `StochasticPi.scala` is used to create also the
_discarded_ actions. Each choice corresponds to a set of enabled actions. The
set of enabled actions of the summation is but the union of the former. However,
even this is handled in the parser, a somewhat "duplicated" algorithm creates
the discarded actions.

Thus, assume a choice of the sumation and dub it - `it`; the union of the set of
_all_ enabled actions of the choices to the left - `left`; and, the union of the
set of _all_ enabled actions of the choices to the right - `right`. Then for
each key in `it`, the discarded actions is the union of `left` with `right`.
As an action is part of many summations on the way up to the top level,
each time there is more than one choice, to the same key there will be added
more "let/right" discarded actions/keys.

What is the benefit? At execution, when a key is part of the enabled actions,
performing the action corresponding to that key means two things. First, the
actions that are in parallel remain the same. Second, whenever the key is
part of a composition (maybe more than one sequence), than the fact that
this parallel composition is a choice in a summation, demands that all other
actions in the other `left` and `right` choices be discarded. So, there maybe
many summations in which the expired _key_ discards other keys.

The discarded and enabled actions are then embedded as an immutable map from
`String` to `Set[String]`in the generated output file. These `magical` maps
are declared as `π-trick` and `π-spell`.

The contention between the enabled actions occurs as follows. Each action in a
sequence is a `for` generator that calls a method in `spi.scala`; the key of
this actions is passed as second argument. The method offers the action `rate`
associated with the key, or enqueues the pair in the `/` `Queue`. That does not mean
the rate will be used, because the action may be _discarded_. A background fiber
blocks on `take`ing or dequeuing this `Queue`. It then updates the `%` map
of all enabled actions by merely setting the rate - which otherwise, at the
moment these actions were enobled, was set to `None` - which is why it is
crucial the [enabled] actions are enabled (rate set to `None`, enough to be
in the `%` map) when actions are fired in parallel. A second background fiber
is blocked on a `Semaphore` that the other bacground fiber releases, and
then (blocking) polls for a next "offer".

This second background fiber then takes a snapshot of the `%` map searching
for any enabled action that has been "reached", and thus has a associated a
rate rather than `None`. If this snapshot is empty, than it will block on the
semaphore shared with the first background fiber.

As soon as the snapshot is non-empty, the second background fiber computes
statistically - starting from the rate - the _delay_ ("delta") that corresponds
to the fastest action. It then uses the key get an associated `Deferred`, and
the delay to _complete_ a `Deferred`. One will match. This corresponds to the
current "winner".

Meanwhile, all methods called (in parallel, either summation or composition)
from a `for` generator, having `offered` the action rate, are (and must _all_
be) blocked on `Deferred.get`'s method. As soon as these `Deferred` are `complete`d
with `null`, each method will `flatMap` to `IO.never`.

Before actually performing the action (which occurs eventually), there is
some "book-keeping" to do. First, the second background fiber blocks on
a semaphore (initially in the `acquire`d state, and shared with all
the agents trough the `using` parameter list, as `/`, `%`, or `\`),
while the winning fiber does some updating to the `%` map and the `\` set,
before releasing the semaphore. Thus, the invariant that the semaphore
is in the `acquire`d state with each start of a contention must be preserved.
There are two cases here:

1. The semaphore is `release`d _before_ the second background fiber
   `acquire`s it. Hence, that the winning fiber has finished
   updating. Thus, the second background fiber will _NOT_ remain
   blocked, and may proceed to "announcing" each discarded key
   that it has lost contention, `complete`ing `Deferrred`s with `null`.
   The semaphore is now in the `acquire`d case.

2. The semaphore is `release`d _after_ the second background fiber
   `acquire`s it. The second background fiber blocked politely,
   announced nicely, the `\` set of discarded keys emptied, but
   the semaphore is now in the `release`d case.

To maintain the invariant, trying to comply with 2.,
the semaphore must be `acquired`. But doing so, and trying
to comply with 1., will result in the second background fiber
to remain blocked. The way out of this is to use the method
`tryAcquire` on the semaphore to maintain the invariant in
either 1. or 2. case.


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

A long prefix path - "`v(x).x<5>.x(y).τ@(1).x@∞(z).z<y>.`":

    for
      _ <- IO.unit
      x <- ν
      _ <- x(BigDecimal(5))
      (y, _) <- x(null)
      _ <- τ(BigDecimal(1))
      (z, _) <- x(∞)
      _ <- z(y)
      .
      .
      .
    yield
      ()

Note that `UUID` second argument is absent.

One can intercalate "`println`"s:

    for
      _ <- IO.unit
      x <- ν
      _ <- IO.println(s"new x=$x")
      t <- x(BigDecimal(5))
      _ <- IO.println("passive output time = $t")
      (y, _) <- x(null)
      _ <- IO.println("input x(y)")
      t <- τ(BigDecimal(1))
      _ <- IO.println(s"silent transition time = $t")
      (z, t) <- x(∞)
      _ <- IO.println(s"immediate input time = $t")
      _ <- z(y, null)
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
named `pi` to translate lazily `! π. P` as:

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
                     π
                     _ <- `<uuid>`
                   yield
                     ()
                 ).parMapN { (_, _) => }
          yield
            ()
        `<uuid>`
      }
      π
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

The root project folder contains four files: `loop.scala`, `stats.scala`, `spi.scala` and `main.scala.in`.
!!!Warning: do not delete them!!!
One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/pi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ spi run ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ pio ex

To get the intermediary `in/ex.scala.in` file, execute the `run` command in the `sbt` shell:

    sbt:pisc> run ex

where `example/pisc/ex.pisc` contains the stochastic Π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/greeter.scala` and add a top-level `package pisc.greeter` line,
edit `examples/fibonacci.scala` and add a top-level `package pisc.fibonacci` line,

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ spi run --interactive fibonacci.scala greeter.scala
