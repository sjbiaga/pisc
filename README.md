Polyadic Pi-calculus in SCala aka PISC ala RISC (experimental)
==============================================================

[π-calculus](https://github.com/sjbiaga/pisc/tree/main-experimental) is a variant.
[Stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic-experimental) is in alpha stage.
[Ambient Calculus](https://github.com/sjbiaga/pisc/tree/ambient) is nicely done, too.
[BioAmbients](https://github.com/sjbiaga/pisc/tree/bioambients-experimental) is another fruitful
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
    PREFIX         ::= "ν" "(" NAMES ")"
                     | μ "."
    μ              ::= "τ" [ EXPRESSION ]
                     | NAME "<" [ "ν" ] NAMES ">" [ EXPRESSION ]
                     | NAME ARITY "<" ">" [ EXPRESSION ]
                     | NAME "(" NAMES ")" [ EXPRESSION ]
                     | NAME <CONS> "(" NAMESʹ ")" [ EXPRESSION ]
    ARITY          ::= "#" NATURAL_NUMBER
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


Emitters
--------

- [Cats Effect](https://github.com/sjbiaga/pisc/tree/polyadic-experimental/ce/README.md)

- [FS2](https://github.com/sjbiaga/pisc/tree/polyadic-experimental/fs2/README.md)

- [Akka](https://github.com/sjbiaga/pisc/tree/polyadic-experimental/akka/README.md)

- [Pekko](https://github.com/sjbiaga/pisc/tree/polyadic-experimental/pekko/README.md)

- [ZStream](https://github.com/sjbiaga/pisc/tree/polyadic-experimental/zs/README.md)


Branches
--------

- [π-calculus (experimental)](https://github.com/sjbiaga/pisc/tree/main-experimental)

- [Polyadic π-calculus (experimental)](https://github.com/sjbiaga/pisc/tree/polyadic-experimental)

- [Ambient Calculus](https://github.com/sjbiaga/pisc/tree/ambient)

- [Ambient Calculus async](https://github.com/sjbiaga/pisc/tree/ambient-async)

- [Stochastic π-calculus (experimental)](https://github.com/sjbiaga/pisc/tree/stochastic-experimental)

- [BioAmbients (experimental)](https://github.com/sjbiaga/pisc/tree/bioambients-experimental)
