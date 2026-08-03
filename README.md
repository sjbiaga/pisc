Calculus of Mobile Ambients in SCala
====================================

The `Ambient Calculus` maps one to one on `Scala` for-comprehensions
"inside" the Cats Effect's `IO[_]` monad.

Unlike [π-calculus](https://github.com/sjbiaga/pisc/tree/main) with its variants
[Stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic) and
[Polyadic π-calculus](https://github.com/sjbiaga/pisc/tree/polyadic), the
implementation of `Ambient Calculus` is much simpler, although the handling
of capability actions involving operations on ambient trees is more elaborated.
[BioAmbients](https://github.com/sjbiaga/pisc/tree/bioambients) is another fruitful
combination of ambients with stochastic π-calculus.

The source code is divided in two: the parser in `Calculus.scala` and the
`Scala` source code generator in `Program.scala`.


Calculus
--------

The `Ambient Calculus` process or capability expressions are exactly as in the
literature, with both ASCII and UTF-8 characters, and slight variations.
Forcibly, _restriction_ is "considered" a _prefix_, besides input/capability
actions per se; output action is a _leaf_, like ambient, [guarded] replication,
and objective move.

The BNF formal grammar for processes is the following.

    LINE           ::= EQUATION | DEFINITION | DIRECTIVE
    EQUATION       ::= INVOCATION "=" PARALLEL
    DEFINITION     ::= "⟦<CODE>" [ TEMPLATE ] "<CODE>⟧" PARAMS [ POINTERS ] "=" PARALLEL
    DIRECTIVE      ::= "⟦" KEY = ( VALUE | "(" VALUE { "," VALUE } ")" ) "⟧"
    PARALLEL       ::= [ SCALE ] SEQUENTIAL { "|" SEQUENTIAL }
    SEQUENTIAL     ::= PREFIXES [ LEAF | "(" PARALLEL ")" ]
    LEAF           ::= "!" [ SCALE ] [ PACE ] [ "." "(" NAME ")" "." ] PARALLEL
                     | NAME "[" PARALLEL "]"
                     | "<" CAPS ">" [ EXPRESSION ]
                     | "go" NAME "." PARALLEL
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
    PREFIX         ::= "τ" [ EXPRESSION ] "."
                     | "ν" "(" NAMES ")"
                     | CAPS "."
                     | "(" NAME ")" [ EXPRESSION ] "."
    SCALE          ::= NATURAL_NUMBER "*"
    PACE           ::= NATURAL_NUMBER [ "," TIME_UNIT ]
    EXPRESSION     ::= "/*" ... "*/"

The BNF formal grammar for capabilities is the following.

    CAPS           ::= CAPABILITY { "." CAPABILITY }
    CAPABILITY     ::= "ε"
                     | ( "in" | "out" | "open" ) NAME
                     | NAME

Lexically, `ident` is an ambient name - (an identifier) starting with lowercase letter;
it may contain single and double quotes.

A source file with the "`.masc`" extension consists of equations, binding an agent identifier
with an optional list of "formal" (bound names) parameters, to a process expression. Because
the use of parentheses in a _restriction_ would lead to ambiguities, it is forced to start
with the UTF-8 character "ν". "()" is _inaction_ or _void_ (empty parallel).
"τ" is the _silent transition_ - which does not exist in the original calculus.

Lines starting with a hash `#` character are (line) comments. Blank lines are ignored.
Lines starting with an `@` character are intermixed as `Scala` code. Lines ending with
backslash continue on the next line.

The output action uses angular parentheses and has the form `<CAPS>`, while
the input action uses the round parentheses and has the form `(NAME).`. A _`name`_
in parentheses can only be an ambient name or a capabilities path (which allows
also for variables).

Stack safe is the [guarded] _replication_ unary operator `! [ "." (NAME) "." ] PARALLEL`;
the guard `"." (NAME) "."` is optional, and it surrounded by `"."` so that it is
unambiguously parsed.

When an ambient is "launched" with the `NAME "[" PARALLEL "]"` syntax, a new `UUID`
will be associated with it, as the common value of all `IOLocal`s corresponding to
the fibers created in parallel by `parSequence`. The `NAME` must have been previously
introduced using "ν" - an ambient name.

Not part of the original `Ambient Calculus`, an agent (invocation) expression - unless
it is binding in an equation -, may be preceded by a sequence of characters wrapped
between curly braces: these will be joined using the dot "`.`" character, standing for
a qualified package identifier. Thus, agents in different translated "`.scala`" files
can be reused; the lexical category is `qual`.

Unlike the rest of the agents, the "`Main`" agent has the command line arguments
spliced as `vararg` parameter(s).

Between "τ" and "." in a silent transition, there can be a `Scalameta` term for
which a `for` generator `_ <- IO { term }` is inserted _after_ the transition,
or any list of `Enumerator`s which are added _after_ the transition. Any symbol
that is found in these terms is considered a _free_ name.


Emitters
--------

- [Cats Effect](https://github.com/sjbiaga/pisc/tree/ambient/ce/README.md)

- [ZIO](https://github.com/sjbiaga/pisc/tree/ambient/zio/README.md)


Branches
--------

- [π-calculus](https://github.com/sjbiaga/pisc/tree/main)

- [Polyadic π-calculus](https://github.com/sjbiaga/pisc/tree/polyadic)

- [Ambient Calculus](https://github.com/sjbiaga/pisc/tree/ambient)

- [Ambient Calculus async](https://github.com/sjbiaga/pisc/tree/ambient-async)

- [Ambient Calculus](https://github.com/sjbiaga/pisc/tree/ambient)

- [Stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic)

- [BioAmbients](https://github.com/sjbiaga/pisc/tree/bioambients)
