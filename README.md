Pi-calculus in SCala aka PISC ala RISC (experimental)
=====================================================

Asynchronous [π-calculus](https://github.com/sjbiaga/pisc/tree/main-async) is a variant.
[Stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic) is in alpha stage.
[Polyadic π-calculus](https://github.com/sjbiaga/pisc/tree/polyadic) is also supported.
Asynchronous [Polyadic π-calculus](https://github.com/sjbiaga/pisc/tree/polyadic-async) is also supported.
[Ambient Calculus](https://github.com/sjbiaga/pisc/tree/ambient) is nicely done, too.
[BioAmbients](https://github.com/sjbiaga/pisc/tree/bioambients) is another fruitful
combination of ambients with stochastic π-calculus.

The source code is divided in two: the parser in `Calculus.scala` and the
`Scala` source code emitter(s) in `Program.scala`.


Calculus
--------

The π-calculus process expressions are exactly as in the literature, with
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
                     | CAPITAL
                     | INVOCATION
                     | INSTANTIATION
    CAPITAL        ::= IDENTIFIER [ "(" [ NAMES ] ")" ] ( POINTERS | "{" "}" )
    INSTANTIATION  ::= "⟦<CODE>" INSTANCE "<CODE>⟧" [ POINTERS ]
    INVOCATION     ::= [ QUAL ] IDENTIFIER PARAMS
    PARAMS         ::= [ "(" NAMES ")" ]
    POINTERS       ::= "{" NAMES "}"
    NAMES          ::= NAME { "," NAME }
    NAMESʹ         ::= [ NAME ] { "," [ NAME ] }

The BNF formal grammar for prefixes is the following.

    PREFIXES       ::= { PREFIX }
    PREFIX         ::= μ "."
                     | "ν" "(" NAMES ")"
    μ              ::= "τ" [ EXPRESSION ]
                     | NAME "<" [ ["ν"] NAME ] ">" [ EXPRESSION ]
                     | NAME "(" NAME ")" [ EXPRESSION ]
                     | NAME <CONS> "(" NAMESʹ ")" [ EXPRESSION ]
    SCALE          ::= NATURAL_NUMBER "*"
    PACE           ::= NATURAL_NUMBER [ "," TIME_UNIT ]
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
agent invocations or unguarded replication.

Not part of the original π-calculus, an (agent) invocation expression - unless
it is binding in an equation -, may be preceded by a sequence of characters wrapped
between curly braces: these will be joined using the dot "`.`" character, standing for
a qualified package identifier. Thus, agents in different translated "`.scala`" files
can be reused; the lexical category is `qual`.

Unlike the rest of the agents, the `Main` agent has the command line arguments
spliced as `vararg` parameter(s).

If an equation's list of formal parameters ends in "*", then the last one will not
be available in the output, its presence serving only to avoid not scoping a free name.
It facilitates passing from Scala values in expressions to names in the calculus.


Emitters
--------

- [Cats Effect](https://github.com/sjbiaga/pisc/tree/main-experimental/ce/README.md)

- [Cats Actors](https://github.com/sjbiaga/pisc/tree/main-experimental/ca/README.md)

- [Akka](https://github.com/sjbiaga/pisc/tree/main-experimental/akka/README.md)

- [Pekko](https://github.com/sjbiaga/pisc/tree/main-experimental/pekko/README.md)


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

- [BioAmbients](https://github.com/sjbiaga/pisc/tree/bioambients) using supervisor/`IO.canceled`

- [BioAmbients](https://github.com/sjbiaga/pisc/tree/bioambients-flatMap) with `flatMap`s/`null` comparison
