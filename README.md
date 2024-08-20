Calculus of Mobile Ambients in SCala
====================================

The `Ambient Calculus` maps one to one on `Scala` for-comprehensions
"inside" the Cats Effect's `IO[Unit]` monad.

Unlike [π-calculus](https://github.com/sjbiaga/pisc/tree/main) with its variants
[Stochastic π-calculus](https://github.com/sjbiaga/pisc/tree/stochastic) and
[Polyadic π-calculus](https://github.com/sjbiaga/pisc/tree/polyadic), the
implementation of `Ambient Calculus` is much simpler, although the handling
of capability actions involving operations on ambient trees is more elaborated.

After code generation, the `Ambient Calculus` "processes" could be
programmatically typed as `Scala` code using `CE` `IO`.

Communications in ambients work as [CE tutorial](https://typelevel.org/cats-effect/docs/tutorial)'s
producer/consumer but no queue, only `takers` and `offerers`.

Composition: parallel modelled with - `parMapN`.

Ambient: singly parallelized with - `parMapN` and `IOLocal`.

[Guarded] Replication: modelled with - `parMapN` and `lazy val` [or `def`].

The source code is divided in two: the parser in `Calculus.scala` and the
`Scala` source code generator in `Program.scala`.


Calculus
--------

The `Ambient Calculus` process or capability expressions are exactly as in the
literature, with both ASCII and UTF-8 characters, and slight variations.
Forcibly, _restriction_ is "considered" a _prefix_, besides input/capability
actions per se; output action is a _leaf_, like ambient and [guarded] replication.

The BNF formal grammar for processes is the following.

    EQUATION   ::= AGENT "=" PARALLEL
    PARALLEL   ::= SEQUENTIAL { "|" SEQUENTIAL }
    SEQUENTIAL ::= PREFIXES [ LEAF | "(" PARALLEL ")" ]
    LEAF       ::= AGENT
                 | "!" [ "." "(" NAME ")" "." ] PARALLEL
                 | NAME "[" PARALLEL "]"
                 | "<" CAPS ">" [ EXPRESSION ]
    AGENT      ::= [ QUAL ] IDENTIFIER [ "(" ")" | "(" NAMES ")" ]

The BNF formal grammar for prefixes is the following.

    PREFIXES   ::= { PREFIX }
    PREFIX     ::= "τ" [ EXPRESSION ] "."
                 | "ν" "(" NAMES ")"
                 | CAPS "."
                 | "(" NAME ")" [ EXPRESSION ] "."
    NAMES      ::= NAME { "," NAME }
    EXPRESSION ::= "/*" ... "*/"

The BNF formal grammar for capabilities is the following.

    CAPS       ::= CAPABILITY { "." CAPABILITY }
    CAPABILITY ::= "ε"
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
the fibers created in parallel by `parMapN`. The `NAME` must have been previously
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


Ambients
--------

Recall that using `parMapN` in `CE` resorts to fibers. There isn't really any
distinction between fibers ran in parallel, so not between ambients, were it not
for the possibility to have per-fiber copies of the same `IOLocal`. And so, in
fact, ambients are not differentiated at runtime (running in parallel) except
that the "_ether_" used for communication is resolved starting with an `IOLocal`.

An `IOLocal` is a parameter to each agent (so including "`Main`") invocation, and is
thus available to be altered when "launching" ambients in parallel. The value of
these `IOLocal`s are `UUID`s. Were it not for the capability to "`open`" ambients,
the ether would not have been mixed as it is when two ambients are "merged" into
one another.

With only the "`in`" and "`out`" capabilities, the _tree_ of ambients always has
the same nodes, although by moving these around, the tree topology is altered.
The ambients tree is a `Map` having as keys `Set`s of `UUID`s (the _nodes_,
initially singletons), and as values pairs with the first member a redundant
data structure representing the node in the tree, and second member  - the
ambient's ether. The former represents a node in the tree by specifying its
_root_, _children_ and _siblings_, so all the possible information about a node
is replicated in each node, making it thus redundant. Although longer to implement
and heavier to allocate, it makes it easier to maintain and lighter to use.

Surprisingly enough - but perhaps not unexpected -, the "`in`" capability has to
do with the siblings, the "`out`" capability - with the root, and the "`open`"
capability - with the children. However, there are common operators to handle
each - involving all siblings, root and children: `remove`, `insert`, `update` and
`merge`.

Let's just divulge prior why the nodes are `Set`s of `UUID`s. So the
`IOLocal` is associated with an `UUID`. When an ambient is "`open`"ed, there
exist already fibers running in parallel, each with its `IOLocal`: those
fibers corresponding to the opening ambient have a different `UUID` associated
than those fibers corresponding to the opened ambient. It is just not doable to
change the `UUID` for all these fibers to a new `UUID`; rather they are left to
correspond to their `UUID`, and the two `UUID`s are contained in a new `Set`.
But given that this is such from the start, there always occurs the union of two
disjoint `Set`s .

What happens given an `IOLocal` is that, to retrieve its node, the keys of the
tree `Map` are searched the `UUID` of the `IOLocal`, for membership to a key-`Set`.
This `Set` is a node, and in the `Map`ped value there can be found all the
information about an ambient tree, together with the ambient's name and ether.

- For an "`in`" capability, the destination ambient name where to move the current
ambient into, is identified among the _siblings_ of the current ambient's node, by
comparison using `eq` (thus, equality of `Java` references, which is fast). A busy
loop occurs, unless there is exactly one match, when (1) the current node is removed
from its root's children, and, for each sibling, from its siblings; too, (2) given
that now the new root is one of the current node's former sibling, for each of the
former's (new root's) child, the latter (current node) is added as sibling, and then (3)
the latter is added to the former's children, while also (4) having its root accordingly
reassigned (the new root) and its siblings reassigned the new root's (previous) children.

- For an "`out`" capability, the destination ambient name where to move the current
ambient into, is straightly identified as the _root_ of the current ambient's node, by
comparison using `eq` (thus, equality of `Java` references, which is fast). A busy
loop occurs, unless there is a match [which may be delayed until other capability
actions running in parallel suitably take place], when (1) the current node is removed
from its root's children, and, for each sibling, from its siblings; too, (2) given
that now the new root is the root of the current node's root, for each of the
former's (new root's) child, the latter (current node) is added as sibling, and then (3)
the latter is added to the former's children, while also (4) having its root accordingly
reassigned (the new root) and its siblings reassigned the new root's (previous) children.

- For an "`open`" capability, the destination ambient name which to merge the current
ambient with, is identified among the _children_ of the current ambient's node, by
comparison using `eq` (thus, equality of `Java` references, which is fast). A busy
loop occurs, unless there is exactly one match, when (1) the current node is removed
from its root's children, and, for each sibling, from its siblings; now, referring
to the current node as "root", and to the child as "node", if (2) there may be the
case of possible communication between the ether of root and the ether of node,
this is "flushed" in a crossing manner (i.e., output from root's ether with input
from node's ether, respectively, input from root's ether with output from node's
ether); further, (3) root and node may now be replaced in the `Map` with their
disjoint union, referred to as "join": also, (4) in place of root, the root's root's
children now have join updated, and [instead of root] each root's sibling's siblings
now have join updated; obviously, (5) join must preserve the same root and siblings
as root; too, (6) the children of node should become those of the join, and node
must not remain a child of join - equivalent, in fact, with merely concatenating
the children and siblings of node as join's new children (thus excluding node);
more, (6) the latter (new children) must be reassigned join as new root; lastly, (7)
merging the children and siblings of node as mutual siblings by concatenating
the siblings of node to each of node's child's siblings, and concatenating
the children of node to each of node's sibling's siblings.


Capabilities
------------

There is one and the same wrapper class for both ambient names and capabilities.

And there is one case class for capability _paths_, that models variables and
capabilities.

When a new (ambient) name is introduced, it is just an object of a wrapper class,
wrapping the sole "`()`" instance of `Unit`. However, each ambient name/object will
have a distinct `Java` reference used when matching ambient names against names
from capabilities. Of course, had the `equals` method been used, all ambient names
would be equal.

A capability is an instance of a case class (named `ζ`) with three parameters:

- the operation: either `in`, `out`, or `open` (`enum` type `ζ-Op`);
- an instance of the wrapper class; and
- an optional next field, of the same base type as `ζ`, which makes the case class
  actually a capability path (i.e., a linked list).

Letting the square brackets indicate that there is a wrapper class involved,
for a capability (path) of the form "`in m`", the instance of the case class is
"`ζ(Some(ζ-Op.in), [()], None)`", where `[()]` matches an object of the wrapper
class that has been introduced for name `m`.

A variable is an instance of the same `ζ` case class, but without an operation,
which thus makes the first parameter of `ζ` be an `Option`.

If this "variable" is passed as output via `<>`, it is done so as the object
"`ζ(None, [x], _)`" in the wrapper class, where `x` is again an object of either
the form:

- "`ζ(Some(_), y, _)`" - where `y` is an "ambient name", or

- "`ζ(None, z, _)`" - where `z` is a variable, and so on.

Note that the case "`ζ(Some(_), [ζ(None, _, _)], _)`", although possible, will raise
error at runtime, because only "`ζ(Some(_), [()], _)`" is a possible match for
ambient names. So, instead of "`ν(m) ( <m> | (n). in n. )`", one should write
"`ν(m) ( <in m> | (n). n. )`".

Such indefinite output and input of variables piling up, rather than being resolved
upon input, it is interpreted recursively only when a capability path containing
such a variable is encountered. Eventually, the bottom case revolves to a
capability (action).


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
ambient names start with lowercase.

Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       masc/
       in/
       out/

The root project folder contains two files: `ma.scala` and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/ma.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ ma ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ maio ex

To get the intermediary `in/ex.scala.in` file, execute the `run` command in the `sbt` shell:

    sbt:MobileAmbients2Scala> run ex

where `example/masc/ex.masc` contains the `Ambient Calculus` source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ am --interactive ex1.scala ex2.scala

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
