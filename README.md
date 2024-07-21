Stochastic Pi-calculus in SCala aka PISC ala RISC
=================================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the Cats Effect's `IO[Unit]` monad.

The stochastic branch adds rates to actions in comparison with
the [π-calculus](https://github.com/sjbiaga/pisc/tree/main).

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `CE` `IO`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").

Channels for names work as [CE tutorial](https://typelevel.org/cats-effect/docs/tutorial)'s
producer/consumer but no queue, only `takers` and `offerers`, which can be at most one.

Composition: parallel modelled with - `parMapN`.

Summation: *probabilistic* choice modelled with - `parMapN`.

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

The BNF formal grammar for processes is the following.

    EQUATION   ::= AGENT "=" CHOICE
    CHOICE     ::= PARALLEL { "+" PARALLEL }
    PARALLEL   ::= SEQUENTIAL { "|" SEQUENTIAL }
    SEQUENTIAL ::= PREFIXES [ LEAF | "(" CHOICE ")" ]
    LEAF       ::= AGENT
                 | "[" NAME ("="|"≠") NAME "]" CHOICE
                 | "if" NAME ("="|"≠") NAME "then" CHOICE "else" CHOICE
                 | NAME ("="|"≠") NAME "?" CHOICE ":" CHOICE
                 | "!" [ "." μ "." ] CHOICE
    AGENT      ::= IDENTIFIER [ "(" ")" | "(" NAME { "," NAME } ")" ]
    EXPRESSION ::= "/*" ... "*/"

The BNF formal grammar for prefixes is the following.

    PREFIXES   ::= { PREFIX }
    PREFIX     ::= μ "."
                 | "ν" "(" NAME ")"
    μ          ::= "τ" [ @ RATE ] [ EXPRESSION ]
                 | NAME [ @ RATE ] "<" [ NAME ] ">" [ EXPRESSION ]
                 | NAME [ @ RATE ] "(" NAME ")" [ EXPRESSION ]

Lexically, `ident` is a channel name - (an identifier) starting with lowercase letter;
capital `IDENT` is an agent identifier starting with uppercase letter. Both may contain
single and double quotes.

A source file with the "`.pisc`" extension consists of equations, binding an agent identifier
with an optional list of "formal" (bound names) parameters, to a process expression. Because
the use of parentheses in a _restriction_ would lead to ambiguities, it is forced to start
with the UTF-8 character "ν". "()" is _inaction_ or the _empty sum_ (with empty parallel).
"τ" is the _silent transition_.

Lines starting with a hash `#` character are (line) comments. Blank lines are ignored.
Lines starting with an `@` character are intermixed as `Scala` code. Lines ending with
backslash continue on the next line.

Summation (`CHOICE`) has lower precedence than composition (`PARALLEL`).

The output prefix uses angular parentheses and has the form `NAME<NAME>.`, while
the input prefix uses the round parentheses and has the form `NAME(NAME).`. A _`name`_
in parentheses can also be a (constant) `String` literal, a (boxed in a) `BigDecimal` number,
or a [`Scalameta`](https://scalameta.org) term as a `Scala` comment between `/*` and `*/`.

The _`rate`_ of an action ("τ" or prefix) can be optionally annotated with `@`
and an infinite-symbol ("∞"), a "top"-symbol ("⊤"), a `Scala` identifier,
a (boxed in a) `BigDecimal` number, or any [`Scalameta`](https://scalameta.org) term
as a `Scala` comment between `/*` and `*/`.

A match has the form `[NAME=NAME]` and a mismatch the same, but
using the `NOT EQUAL TO` Unicode `≠` character. `NAME=NAME` or `NAME≠NAME` is a
_test_,that can be used also as `if NAME(=|≠)NAME then CHOICE else CHOICE` or
as the syntactic sugar `NAME(=|≠)NAME ? CHOICE : CHOICE` Elvis ternary operator.

Stack safe is the [guarded] _replication_ unary operator `! [ "." μ"." ] CHOICE`;
the guard `"." μ "."` is optional, and it starts with a `"."` so that it is
distinguished from other prefixes.

The name before parentheses (angular or round) must be a channel name.

For output, the name in angular parentheses is optional, if empty being `null`.
This may be used to cease guarded replication with input prefix guard: i.e., if a
`null` is received, the (stack-safe, recursive) replication stops.

Note that input/output prefixes and the silent transition are followed by a dot,
whereas restriction is not; also, inaction, invocation, (mis)match, `if then else`
and replication are "leaves".

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
invocations or unguarded replication.

Unlike the rest of the agents, the `Main` agent has the command line arguments
spliced as `vararg` parameter(s).


Stochastic
----------

The execution of a stochastic π-calculus program is handled in the files:
`loop.scala`, `stats.scala` and `spi.scala` or `spi_.scala`. The `Main` agent is
called from the final generated source file wherein `main.scala.in` was `cat`enated.

From `main.scala.in`, two fibers are launched in `background` and used as `Resource`s,
such that terminating the program cancels them. Even when a process is _discarded_,
a `Deferred[IO, Option[(Double, -)]]` completed with `None` is used for this
situation, hence the fiber does not block semantically (so the program would not
exit any longer).

Silent transitions, input and output prefixes ("actions") are associated with
an `UUID` (`Universally Unique IDentifier`) by inheriting the trait `Key` via the
trait `State`. The only other expression that is a `State` is... summation (or choice).

A `State` contains a `Set` of `enabled` actions `(UUIDs`). _Parsing_ the equations
binding agents to process expressions, results in finding the enabled actions
for all summations - including those corresponding to the bound agents. The `State`
"transition system" is such that each "precursor" _completed_ action (i.e., that
is not excluded or discarded) prior to its "expiration", immediately enables other
"successor" actions, including possibly, again itself.

Unlike π-calculus, where communication occurs unhindered whenever a channel inputs
and outputs, in stochastic π-calculus actions have rates, and depending on these
there is a "probabilistic" _election_ which determine which channels are selected for
input/output. Therefore, it is important that there should be a "moment" for this
election to consider _all_ enabled actions when selecting the pairs to communicate,
or otherwise - if some enabled actions are provisionally "pending" - none.

Thus, in advance, it is always known which (as well as how many of _the same_)
enabled actions are just pending before blocking for their completion;
the latter may result in either success, or in actions being discarded (as not
having been probabilistically chosen) - by completion with `None`.

Pending actions will soon reach blocking for their completion. As long as there
are pending actions enabled but not yet blocked for completion, _none_ of the
other enabled actions actually blocking, do participate in what corresponds to
probabilistic choice - the election mentioned above: there is patience for that
"moment" to arrive. Of course, it may be hard to catch (for unguarded replication).

Even if many of the same action (i.e., with the same key) are enabled, these are
distinguished upon completion by a unique `UUID`, either per agent invocation, or
per replication. And due to the atomicity of updates, the management of the data
structure for the enabled actions, is coherent among all actions thus "firing"
in parallel. This makes actions behave as multisets.

Given that a summation is a list of choices, a choice is a list of (parallel)
compositions, and a composition is a list of (sequential) prefixes ended with
either a leaf or summation, when parsing the equations for resolving the
enabled actions, an _invariant_ ensures that (a) for each action there is a
non-empty set of enabled actions, unless (b) this action is the last in a sequence
of prefixes ending with inaction.

When violated, the invariant is maintained by inserting immediate τ-actions
(referred to as _implicit_, in contrast with those that are explicitly occurring)
with weight equal to `Long.MaxValue` - where appropriate,

- for the case of guarded replication, the guard itself is the enabled action:
  it enables itself, and other actions (a), unless the replicated process is
  inaction (b);

- for unguarded replication, the invariant holds (a), unless the replicated
  process is inaction: in this case, the latter is rewritten as "τ@∞.()" (a);

- likewise for (mis)match: on either branch, the invariant holds (a), unless
  the ensuing process is inaction: in this case, the latter is rewritten as
  "τ@∞.()" (a);

- the sole case of a leaf that is not prefixed by an action remains (either
  inaction or) invocation: in this case, its prefixes are appended "τ@∞." (a).

Sticking to this scheme (of implicit τ-actions), after parsing the equations,
each composition or choice will have a non-empty set of enabled actions (a).
If an equation is self-recursive, among the enabled actions of the last (implicit
or explicit) action some self-recursive invocation is prefixed with, is this last
action itself.

In the single case of (mis)match, the enabled actions on both branches become the
enabled actions for this kind of process expression. However, given that while
the enabled actions on the active branch only will be reached, the (enabled)
actions on the passive branch must so be _excluded_: but this mutual exclusion must
occur on behalf of just _one_ of the enabled actions; otherwise, whereas it were
enabled exactly once, an excluded action could risk being "disabled" more than once.

Here, there are two subtleties. First, if the process on the active branch is a
summation, then there is not exactly one of the enabled actions on which behalf
the passive branch must be excluded, while finding them all becomes complicated.
Second, if there are nested (mis)matches, the one enabled action chosen to exclude
the passive nesting branch, might belong to the passive nested branch, in which case
the exclusion of the passive nesting branch never occurs. For these two situations,
the nested (mis)match is prefixed with "τ@∞." (a).

The file `StochasticPi.scala` is used to create a [non-]deterministic _"transition system"_
between `State`s for the purpose of enabling the actions in the successor state, immediately
prior to the "expiration" of the precursor enabled action. From action to successive action,
there is a transition, even if there are restrictions in between (picture these latter
translation in `Scala`). And finally, as there always is a last action in a sequence (either
explicit or implicit), there is a transition from it to either an `end`ing summation or
replication guard.

But more importantly, upon the expiration of the last action from a sequence of prefixes,
the enabled actions are enabled _before_ they are next to be "fired" (in parallel) - which
means blocking for their completion.

The transition system is non-deterministic, because for the case of guarded replication
there is also one transition from the guard to itself and to the replicated process.
This means that after expiration of the guard action, the enabled actions are again
the guard itself and the enabled actions of the process that fires up in parallel.

Besides the _enabled_ actions, `StochasticPi.scala` is used to create also the
_discarded_ actions. Each choice corresponds to a set of enabled actions. The
set of enabled actions of the summation is but the union of the former. However,
a somewhat "duplicated" algorithm creates the discarded actions: and, with this
occasion, alse the _excluded_ actions.

Thus, assume a choice of the summation; the union of the set of _all_ enabled
actions of the choices to the left; and, the union of the set of _all_ enabled
actions of the choices to the right. Then for each key in the summation, the
discarded actions is the union of "left" with "right".

As an action is part of many summations on the way up to the top level,
each time there is more than one choice, to the same key there will be added
more "left/right" discarded actions/keys.

What is the benefit? At execution, when a key is part of the enabled actions,
performing the action corresponding to that key means two things. First, the
actions that are in parallel remain the same. Second, whenever the key is
part of a composition (maybe more than one sequence), then the fact that
this parallel composition is a choice in a summation, demands that all other
actions in the other "left" and "right" choices be discarded. So, there may be
many summations in which the expired _key_ discards other keys.

The discarded, excluded, and enabled actions are then embedded as an immutable map
from `String` to `Set[String]`in the generated output file. These `magical` maps
are declared as `π-trick`, `π-elvis` and `π-spell`, respectively.

The contention between the enabled actions occurs as follows. First, it "disables"
the excluded actions associated, if any. Each action in a sequence is a `for`
generator that calls a method in `spi.scala` (or `spi_.scala`); the key of this
action is passed as second argument. The method is within a unique scope (`^`)
to distinguish between different actions that correspond to the same key (multisets).
It creates a `Deferred[IO, Option[(Double, -)]]` and offers these together with the
action `rate` associated with the key, or enqueues a quadruple in the `/` `Queue`.
That does not mean the rate will be used, because the action may be _discarded_.
Then, it blocks semantically on the `Deferred.get` method. If discarded, it will
be `canceled`, but the cancellation will not outlive the `Supervisor`'s lifecycle
which the fiber `use`s for this purpose in a parameter to a (nested) `parMapN`.

A background fiber blocks on (`take`ing or) dequeuing this `Queue`. It then updates
the `%` map of all enabled actions by merely mapping the string `^ + key` (where `^`
stands for the unique scope) to a triple `Ref` & `Deferred` & rate - prior, it
decreases the number associated with the `key` from the (dual) `%` map.

There are two kinds of keys:

- the size of an `UUID`, each action has hard-coded a unique key; in the dual `%`
  map, these correspond to numbers: a number increases (`π-enable` method in
  `spi.scala`) as more actions - with the same key, i.e., in parallel - are enabled,
  but decreases (`poll` method in `loop.scala`) as the actions are "fired": when
  the number/counter reaches zero, a key is removed from the `%` map (also, when
  keys are `π-discard`ed);

- double the size of the former, concatenating the `UUID` of the current scope
  (`implicit ^`) with actions' keys, and replacing the value in the dual `map` by
  mapping `^ + key` to the triple `Ref` & `Deferred` & rate, each "fired" action
  has also associated a runtime unique key; to the "same actions", there may
  correspond concomitantly a number _and_ a triple in the dual `%` map.

It is crucial the [enabled] actions are enabled (rate absent, but enough for
the loose key to be in the `%` map) already when actions are fired in parallel.
A second background fiber is blocked on a `Semaphore` that the other background
fiber releases, and then (blocking) polls for a next "offer".

This second background fiber then awaits for all enabled actions to be "reached"
or "fired", and thus have associated a rate rather than a number (the value type
of the `%` map is a `Scala 3` _union_ type). If the multisets are not empty,
then it will block on the semaphore shared with the first background fiber.

As soon as the multisets are empty, the second background fiber computes
[statistically](https://github.com/scalanlp/breeze) - starting from the
rate(s) and/or weight(s) - the _delay(s)_ (or duration(s) or "delta(s)") that
correspond(s) to the fastest action(s). In plural, because the programmer can
set a degree of parallelism - the number of cores - allowing for multiple
communications to occur, as long as these do not share the same channel and
do not discard each other - otherwise, until such a gap, only the first (satisfying
these two conditions) are returned: for each (pair), it then uses a key to get an
associated `Deferred`, and the delay to _complete_ a `Deferred`; it does this twice,
for actions of opposite polarities, unless it's just the case of a single "τ".
One more constraint is that the two actions (in a pair) do not discard each other.

Meanwhile, all methods called (in parallel, either summation or composition)
from a `for` generator, having `offered` the action rate, are (and must _all_
be) blocked on `Deferred.get`'s method. As soon as one (pair of) `Deferred` is
`complete`d, the rest - upon being discarded - will be semantically unblocked
and canceled. Upon `complete`ion of a pair, a `CyclicBarrier[IO](3)` is also
passed, such that all three fibers (the loop, the positive polarity action and
the negative polarity action) `await`, and only continue as soon as - after the
"communication" but not before its result -, the keys to be discarded are
discarded and the keys to be enabled are enabled.

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

A long prefix path - "`v(x).x<5>.x(y).τ@(1).x@∞(z).z<y>.`":

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
        lazy val _<uuid>: String => IO[Unit] = { implicit ^ =>
          (
            .  // P
            .
            .
          ,
            for
              π
              _ <- _<uuid>(`π-uuid`)
            yield
              ()
          ).parMapN { (_, _) => }
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

The root project folder contains five files: `loop.scala`, `stats.scala`, `spi.scala`,
`spi_.scala`, and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/spi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ spi ex.scala

or - if stopping output prefix replication -, add an underscore:

    ./examples $ spi_ ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ spio ex

To get the intermediary `in/ex.scala.in` file, execute the `run` command in the `sbt` shell:

    sbt:Stochastic π-Calculus2Scala> run ex

where `example/pisc/ex.pisc` contains the stochastic π-calculus source (equations binding
agents to process expressions).
