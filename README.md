BioAmbients in SCala aka BASC ala RISC (experimental)
=====================================================

The source code is divided in two: the parser in `Calculus.scala` and the
`Scala` source code emitter(s) in `Program.scala`.


Calculus
--------

The bioambients process expressions are exactly as in the literature, with
both ASCII and UTF-8 characters, and slight variations. There is "match" and
"mismatch", but also there is `if then else` or the sugared Elvis operator.
Forcibly, _restriction_ is "considered" a _prefix_, besides input/output
prefixes per se.

The BNF formal grammar for processes is the following.

    LINE           ::= EQUATION | DEFINITION | DIRECTIVE
    EQUATION       ::= INVOCATION "=" CHOICE
    DEFINITION     ::= "⟦<CODE>" [ TEMPLATE ] "<CODE>⟧" PARAMS [ POINTERS ] "=" CHOICE
    DIRECTIVE      ::= "⟦" KEY = ( VALUE | "(" VALUE { "," VALUE } ")" ) "⟧"
    CHOICE         ::= [ SCALE ] PARALLEL { "+" PARALLEL }
    PARALLEL       ::= [ SCALE ] SEQUENTIAL { "|" SEQUENTIAL }
    SEQUENTIAL     ::= PREFIXES [ LEAF | "(" CHOICE ")" ]
    LEAF           ::= "[" NAME ("="|"≠") NAME "]" CHOICE
                     | "if" NAME ("="|"≠") NAME "then" CHOICE "else" CHOICE
                     | NAME ("="|"≠") NAME "?" CHOICE ":" CHOICE
                     | "!" [ SCALE ] [ PACE ] [ "." μ "." ] CHOICE
                     | [ LABEL ] "[" CHOICE "]"
                     | CAPITAL
                     | INVOCATION
                     | INSTANTIATION
    CAPITAL        ::= IDENTIFIER [ "(" [ NAMES ] ")" ] ( POINTERS | "{" "}" )
    INSTANTIATION  ::= "⟦<CODE>" INSTANCE "<CODE>⟧" [ POINTERS ]
    INVOCATION     ::= IDENTIFIER PARAMS
    PARAMS         ::= [ "(" NAMES ")" ]
    POINTERS       ::= "{" NAMES "}"
    NAMES          ::= NAME { "," NAME }
    NAMESʹ         ::= [ NAME ] { "," [ NAME ] }

The BNF formal grammar for prefixes is the following.

    PREFIXES       ::= { PREFIX }
    PREFIX         ::= μ "."
                     | ζ "."
                     | "ν" "(" NAMES ")"
    μ              ::= "τ" [ @ RATE ] [ EXPRESSION ]
                     | [ $ ] NAME [ @ RATE ] "!" "{" ["ν"] NAME "}" [ EXPRESSION ]
                     | [ $ ] NAME [ @ RATE ] "?" "{" NAME "}" [ EXPRESSION ]
                     | NAME <CONS> "(" NAMESʹ ")" [ EXPRESSION ]
    ζ              ::= ( "enter" | "accept" | "exit" | "expel" | "merge+" | "merge-" ) NAME [ @ RATE ]
    $              ::= "local" | "s2s" | "p2c" | "c2p"
    SCALE          ::= NATURAL_NUMBER "*"
    PACE           ::= NATURAL_NUMBER [ "," TIME_UNIT ]
    EXPRESSION     ::= "/*" ... "*/"

Lexically, `ident` is a channel name - (an identifier) starting with lowercase letter;
capital `IDENT` is an agent identifier starting with uppercase letter. Both may contain
single and double quotes.

A source file with the "`.basc`" extension consists of equations, binding an agent identifier
with an optional list of "formal" (bound names) parameters, to a process expression. Because
the use of parentheses in a _restriction_ would lead to ambiguities, it is forced to start
with the UTF-8 character "ν". "()" is _inaction_ or the _empty sum_.
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

Unlike the rest of the agents, the `Main` agent has the command line arguments
spliced as `vararg` parameter(s).


BioAmbients
-----------

The execution of a bioambients program is handled in the files:
`loop.scala`, `stats.scala` and `ba.scala`. The `Main` agent is invoked from the
final generated source file wherein `main.scala.in` was `cat`enated.

From `main.scala.in`, two fibers are launched in `background` and used as `Resource`s,
such that terminating the program cancels them. Even when a process is _discarded_,
a `Deferred[IO, Option[(Double, -)]]` completed with `None` is used for this
situation, hence the fiber does not block semantically (so the program would not
exit any longer).

Silent transitions, input and output prefixes ("actions") are associated with
an `UUID` (`Universally Unique IDentifier`) by inheriting the trait `Act`. The
only other expression that has `enabled` actions is... summation (or choice),
by extending `Sum`.

A state contains a `Set` of `enabled` actions `(UUIDs`). _Parsing_ the equations
binding agents to process expressions, results in finding the enabled actions
for all summations - including those corresponding to the bound agents. States
constrain that each "precursor" _completed_ action (i.e., that is not excluded
or discarded) prior to its "expiration", immediately enables other "successor"
actions, including possibly, again itself.

Unlike π-calculus, where communication occurs unhindered whenever a channel inputs
and outputs, in bioambients actions and capabilities have rates, and depending on these
there is a "probabilistic" _election_ which determine which channels are elected for
communication/movement. Therefore, it is important that there should be a "moment" for this
election to consider _all_ enabled actions when selecting the pairs to communicate/move,
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
per replication - the scope. And due to the atomicity of updates, the management
of the data structure for the enabled actions, is coherent among all actions thus
"firing" in parallel. This makes actions behave as multisets.

The file `BioAmbients.scala` is used to create a _directed multigraph_ with nodes
the _union_ of action and sum _type_, which at runtime gives rise to a
_transition system_ with states - multisets of enabled actions -, for the purpose of
enabling the actions in the successor state, immediately prior to the "expiration"
of the precursor enabled action. From action to successive action or sum, there is
an edge, even if there are restrictions in between (picture these latter translation
in `Scala`).

Upon the expiration of the last action from a sequence of prefixes, the enabled
actions are enabled _before_ they are next to be "fired" (in parallel) - which
means blocking for their completion.

The directed multigraph has multiple edges, because for the case of guarded
replication there is also one edge from the guard to itself (a loop) and to the
replicated process. This means that after expiration of the guard action, the
enabled actions are again the guard itself and the enabled actions of the process
that fires up in parallel.

Besides the _enabled_ actions, `BioAmbients.scala` is used to create also the
_discarded_ actions. Each choice corresponds to a set of enabled actions or
capabilities. The set of enabled actions/capabilities of the summation is but
the union of the former. However, a somewhat "duplicated" algorithm creates the
discarded actions: and, with this occasion, alse the _excluded_ actions.

Thus, assume a choice of the summation; the union of the set of _all_ enabled
actions of the choices to the left; and, the union of the set of _all_ enabled
actions of the choices to the right. Then for each key in the summation, the
discarded actions is the union of "left" with "right".

As an action/capability is part of many summations on the way up to the top level,
each time there is more than one choice, to the same key there will be added
more "left/right" discarded actions/capabilities/keys.

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
generator that calls a method in `ba.scala`; the key of this
action is passed as second argument. The method is within a unique scope (`^`)
to distinguish between different actions that correspond to the same key (multisets).
It creates a `Deferred[IO, Option[(Double, -)]]` and offers these together with the
action `rate` associated with the key, or enqueues a quadruple in the `/` `Queue`.
That does not mean the rate will be used, because the action may be _discarded_.
Then, it blocks semantically on the `Deferred.get` method. If discarded, it will
be `canceled`, but the cancellation will not outlive the `Supervisor`'s lifecycle
which the fiber `use`s for this purpose in a parameter to a (nested) `parSequence`.

A background fiber blocks on (`take`ing or) dequeuing this `Queue`. It then updates
the `%` map of all enabled actions by merely mapping the string `^ + key` (where `^`
stands for the unique scope) to a triple `Ref` & `Deferred` & rate - prior, it
decreases the number associated with the `key` from the (dual) `%` map.

There are two kinds of keys:

- the size of an `UUID`, each action has hard-coded a unique key; in the dual `%`
  map, these correspond to numbers: a number increases (`π-enable` method in
  `ba.scala`) as more actions - with the same key, i.e., in parallel - are enabled,
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
A second background fiber is blocked on a different `Queue` that the other
background fiber releases, and then (blocking) polls for a next "offer".

This second background fiber then awaits for all enabled actions to be "reached"
or "fired", and thus have associated a rate rather than a number (the value type
of the `%` map is a `Scala 3` _union_ type). If the multisets are not empty,
then it will block on the `Queue` shared with the first background fiber.

As soon as the multisets are empty, the second background fiber computes
[statistically](https://github.com/scalanlp/breeze) - starting from the
rate(s) and/or weight(s) - the _delay(s)_ (or duration(s) or "delta(s)") that
correspond(s) to the fastest action(s). In plural, because the programmer can
set a degree of parallelism - the number of cores - allowing for multiple
communications to occur (quasi-simultaneously), as long as these do not discard each
other - only those satisfying this condition are returned: for each (pair), it
then uses a key to get an associated `Deferred`, and the delay to _complete_ a
`Deferred`; it does this twice, for actions of opposite polarities, unless it's
just the case of a single "τ". One obvious constraint is that the two actions
(in a pair) do not discard each other.

Meanwhile, all methods called (in parallel, either summation or composition)
from a `for` generator, having `offered` the action rate, are (and must _all_
be) blocked on `Deferred.get`'s method. As soon as one (pair of) `Deferred` is
`complete`d, the rest - upon being discarded - will be semantically unblocked
_and_ canceled. Upon `complete`ion of a pair, a `CyclicBarrier[IO](3)` is also
passed, such that all three fibers (a parallel fiber from the loop, the positive
polarity action and the negative polarity action) `await`, and only continue as
soon as - after the "communication" but not before its result -, the keys to be
discarded are discarded and the keys to be enabled are enabled.


Traces
------

Traces are enabled by the `"traces"` directive. If active, the keys of actions
are appended: the channel name, ("τ" for the silent action), the agent name from
the current equation, a label, the action's rate, the direction/capability, and
the filename where traces are directed. However, it is not until runtime that
these keys may be possibly used. The traces' output is a `.csv` file with the
following columns:

    number,start,end,name,polarity,key,replication,label,rate,delay,duration,agent,dir_cap,start_ambient,end_ambient,filename

The first column is the current _number_. The next two columns are two timestamps,
the `start` and `end` of the action in nanoseconds. The _channel name_ is the
fourth column (or "τ"). The polarity of the action (`false` for output, `true`
for input, empty for `τ`) is the fifth column. (The number and the `end`
timestamp are the same for any _two_ input/output actions that result in a
communication). The sixth stores the unique _key_. The seventh tells whether the
action is the guard of a _replication_. The eighth is a _label_ that is a string
of tags, which differentiate between the elements of summations and compositions.
The ninth is the _rate_. The tenth is the _delay_ of the action (silent or
communication). The eleventh may be `0.0` for immediate actions, `NaN` for
passive actions, or the same as the tenth for active actions. The twelfth is the
originating _agent name_. The thirteenth is either the _direction_ or the
_capability_. The fourteenth and the fifteenth are the start ambient's and the
end ambient's label. The final sixteenth column is a _filename_ (with possible
to be ignored commas) or empty.

There may be silent actions which are inserted after the other are labelled:
these are not traceable, but nor should they need be; if they are still needed,
they must be inserted explicitly.


Snapshots
---------

Snapshots are enabled by the `"snapshot"` directive. If set, the ambients'
hierarchy at the end of each communication, is flushed into an `.xml` file. The
name of this file is the current number - the same as the first column in
[traces](#traces) - dash the polarity - the same as the fifth column in
[traces](#traces): these two name components are enough to make the name unique,
as well as to be corroborated with the `.csv` entries. Unlike the `.csv` file
which is appended records to, the `.xml` file gets always overwritten.


Emitters
--------

- [Cats Effect](https://github.com/sjbiaga/pisc/tree/bioambients-experimental/ce/README.md)

- [Cats Effect (flatMap)](https://github.com/sjbiaga/pisc/tree/bioambients-experimental/cef/README.md)


Branches
--------

- [π-calculus](https://github.com/sjbiaga/pisc/tree/main)

- [π-calculus async](https://github.com/sjbiaga/pisc/tree/main-async)

- [π-calculus (experimental)](https://github.com/sjbiaga/pisc/tree/main-experimental)

- [Polyadic π-calculus](https://github.com/sjbiaga/pisc/tree/polyadic)

- [Polyadic π-calculus async](https://github.com/sjbiaga/pisc/tree/polyadic-async)

- [Ambient Calculus](https://github.com/sjbiaga/pisc/tree/ambient)

- [Ambient Calculus async](https://github.com/sjbiaga/pisc/tree/ambient-async)

- [Stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic) using supervisor/`IO.canceled`

- [Stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic-flatMap) with `flatMap`s/`null` comparison

- [Stochastic π-calculus (experimental)](https://github.com/sjbiaga/pisc/tree/stochastic-experimental)

- [BioAmbients](https://github.com/sjbiaga/pisc/tree/bioambients) using supervisor/`IO.canceled`

- [BioAmbients](https://github.com/sjbiaga/pisc/tree/bioambients-flatMap) with `flatMap`s/`null` comparison

- [BioAmbients (experimental)](https://github.com/sjbiaga/pisc/tree/bioambients-experimental)
