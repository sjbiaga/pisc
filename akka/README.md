Pi-calculus in SCala aka PISC ala RISC (experimental)
=====================================================

The π-calculus inner process nest inside outer processes, as nested methods.
These define the behavior of `Akka` actors corresponding to processes, but
their parameters also make outer names available to inner scope.

Prefixes are mapped one to one on `Scala` for-comprehensions
"inside" the Scala's `Future[_]` monad.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `Akka` and/or `Future`.


Program
-------

There are three extension methods in the program emitter, invoked from the
`Program.Main` class' `apply` function: `AST.generateʹ`, `AST.generate`, and
`Pre.emit`. The former two have as receiver an `AST` and are named after the
word "generate", because they also _generate_ code (like definitions), unlike
the latter with receiver a `Pre` (also an abstract syntax tree) which only _emits_
a list of `Scalameta` `Enumerator`s to be used in for-comprehensions.

The output - for every binding equation - consists in _nested_ definitions, each
with the return type `Behavior[Π]` where `akka.actor.typed.Behavior` is the FQN
for `Behavior` and the type `Π` is `Either[Option[AtomicBoolean], Behavior[Π]]`.

There are two reasons why `Π` is an `Either`. One, is that (nondeterministic) summation
is implemented using an `AtomicBoolean`: the actors which stand for each choice
are sent this `AtomicBoolean` as the message `Left(Some(ab))`. For example, the
code generated for the binding equation "`P(x) =`" (where the expression corresponding
to the agent `P` is inaction) is this:

    def P(x: `()`): Behavior[Π] = {
      Behaviors.receive { case (given ActorContext[Π], Left(it)) =>
        if (it.fold(true)(_.compareAndSet(false, true))) {
          Behaviors.empty
        } else {
          Behaviors.stopped
        }
      }
    }

Two, prefixes (including `ν` and polyadic unconsing) are emitted together as a
for-comprehension "inside" the Scala's `Future[_]` monad - which may also have
Scala enumerator expressions that compute asynchronously. This demands the use of
the `pipeToSelf` method on the `ActorContext`, but what is the result? Especially
if there are new names introduced by the for-comprehension, these must be available
to the leaf expression. Thus the result is the next behavior of the current actor,
received as message wrapped in `Right` - the result of `pipeToSelf` -, that has
captured all the new names when the prefixes end. For instance, `Q = ν(x) x(x). P(x)`
generates this code:

    01 def Q(): Behavior[Π] = {
    02   def _υ4υ(): Behavior[Π] = {
    03     def _υ1υ(x: `()`): Behavior[Π] = {
    04       ()
    05       Behaviors.receive {
    06         case (given ActorContext[Π], _) => {
    07           val _υ3υ = given_ActorContext_Π.spawnAnonymous(P(x))
    08           _υ3υ ! Left(None)
    09           Behaviors.empty
    10         }
    11       }
    12     }
    13     Behaviors.receive {
    14       case (given ActorContext[Π], Right(it)) =>
    15         given_ActorContext_Π.self ! Left(None)
    16         it
    17       case (given ActorContext[Π], _)         =>
    18         given ExecutionContext = given_ActorContext_Π.executionContext
    19         given_ActorContext_Π.pipeToSelf {
    20           val _υ2υ = given_ActorContext_Π.spawnAnonymous(ν())
    21           for {
    22             x <- Future.successful(_υ2υ)
    23             x <- x()
    24           } yield Right(_υ1υ(x))
    25         }(_.get)
    26         Behaviors.same
    27     }
    28   }
    29   Behaviors.receive { case (given ActorContext[Π], Left(it)) =>
    30     if (it.fold(true)(_.compareAndSet(false, true))) {
    31       val _υ5υ = given_ActorContext_Π.spawnAnonymous(_υ4υ())
    32       _υ5υ ! Left(None)
    33       Behaviors.empty
    34     } else {
    35       Behaviors.stopped
    36     }
    37   }
    38 }

The input prefix `x(x).` introduces a new name (`x`) identically as the channel's
name. The code emitted is `x <- x()`, which is legal (unlike the assignment
`x = x()`). There are two nested methods: `_υ4υ` and `_υ1υ`, whose names are uniquely
chosen but randomly elected although sequentially issued (notice that there occur
five unique names above: `_υ1υ`, `_υ2υ`, `_υ3υ`, `_υ4υ`, and `_υ5υ`, numbered
consecutively).

Also note the pattern of the (nested) methods: a nested method (otherwise, "()")
followed by a `Behaviors.receive` with a `PartialFunction` as argument. The always
_two_ statements for each method are wrapped each time in a block (of statements).
We will see below that there may, in some cases, be more than two statements.

When agent `Q` occurs in an expression, its "invocation" consists in sending a
message to an actor spawned with `Q`'s behavior: this can only be either `Left(None)`
or `Left(Some(ab))` for some `AtomicBoolean` acting as a semaphore to a summation.
If the condition `it.fold(true)(_.compareAndSet(false, true))` in `line #30` succeeds,
then an actor assigned to `_υ5υ` is spawned and sent the message `Left(None)`. Its
behavior is defined in `lines #13-#27`, which corresponds to the prefixes `ν(x) x(x).`
(the creation of a new name `x` followed by a input on channel `x`).

Note how the new name `x` corresponds to an actor (assigned to `_υ2υ`) spawned in
`line #20` with the behavior `ν()`: this is how channels are implemented at [runtime](#runtime),
as special actors with two queues, for output and input. In `line #22`, the new name
`x` is introduced as an alias for the freshly spawned actor `_υ2υ` (this is also done
using a generator rather than an assignment in the for-comprehension). In `line #23`
the introduction of `x` is pending on the input on channel `x` (both have the same name,
which is perfectly legal in Scala). When input is available, the for-comprehension will
yield `Right(_υ1υ(x))`: this is a message that the current actor will send to itself,
which wraps in a `Right` the `Behavior` returned from `_υ1υ(x)` - where (the last) `x`
is captured by `lines #04-#11`. Because of `line #26`, the current actor will promptly
receive this latter message in the _same_ `PartialFunction` from lines `#14-#26`. The
case matched though this time is in `lines #14-#16`: the current actor continues just
by sending a `Left(None)` message to itself, but it changes the behavior to `it`. Now,
receiving in `lines #06-#10`, the behavior of `P(x)` is spawned as actor `_υ3υ` and it
is "invoked" by sending it the message `Left(None)`, after which the current actor
"stops" with `Behavior.empty` in `line #09`.

Let us change a bit the agent `Q`'s definition to `Q' = ν(x) x(y). ( P(x) | P(y) )`; the
leaf is now a composition (in fact a summation with a single operand, the composition):

    def `Q'`(): Behavior[Π] = {
      def _υdυ(): Behavior[Π] = {
        def _υ1υ(x: `()`, y: `()`): Behavior[Π] = {
          def _υ9υ(): Behavior[Π] = {
            Behaviors.receive {
              case (given ActorContext[Π], _) => {
                val _υaυ = given_ActorContext_Π.spawnAnonymous(P(x))
                val _υbυ = given_ActorContext_Π.spawnAnonymous(P(y))
                πLs(_υaυ, _υbυ).πforeach()
                Behaviors.empty
              }
            }
          }
          Behaviors.receive {
            case (given ActorContext[Π], _) => {
              val _υcυ = given_ActorContext_Π.spawnAnonymous(_υ9υ())
              _υcυ ! Left(None)
              Behaviors.empty
            }
          }
        }
        Behaviors.receive {
          case (given ActorContext[Π], Right(it)) =>
            given_ActorContext_Π.self ! Left(None)
            it
          case (given ActorContext[Π], _)         =>
            given ExecutionContext = given_ActorContext_Π.executionContext
            given_ActorContext_Π.pipeToSelf {
              val _υ2υ = given_ActorContext_Π.spawnAnonymous(ν())
              for {
                x <- Future.successful(_υ2υ)
                y <- x()
              } yield Right(_υ1υ(x, y))
            }(_.get)
            Behaviors.same
        }
      }
      Behaviors.receive { case (given ActorContext[Π], Left(it)) =>
        if (it.fold(true)(_.compareAndSet(false, true))) {
          val _υeυ = given_ActorContext_Π.spawnAnonymous(_υdυ())
          _υeυ ! Left(None)
          Behaviors.empty
        } else {
          Behaviors.stopped
        }
      }
    }

Although the sequence of issued unique names was consecutive, there are now some
gaps: this is because the [optimizer](#optimizer) has managed to "merge" two actors corresponding
to the "chain calls" for `P(x)` and `P(y)`. This is possible because in the composition
`P(x) | P(y)`, each invocation is prefixed with no prefixes. The optimization replaces
the spawning of the actor which spawns `P(x)` with spawning `P(x)` directly instead,
removing the other. This is impossible in the previous case, because there are three
steps that are respected in `generate`:

1. `lines #29-#37` which correspond to invoking the agent `Q`;
1. `lines #13-#27` which correspond to the non-empty prefixes;
1. `lines #05-#11` which correspond to the leaf (invocation of `P(x)`).

Indeed, it is the combination between the existence or not of prefixes and the
(range of the) cases for leaves that defines the logic in the [`generate`](#generate) method.
This in turn is the basis for the [optimizer](#optimizer).

On the other hand, the [`generate`](#generate) method handles also "cases sum" using the
[`generateʹ`](#generateʹ) ("generate prime") method. The general form of "cases sum" is

    [a = b][m = n][u = v]⋯ + [c = d][p = q][w = x]⋯ + ... + [e = f][s = t][y = z]⋯

The idea here is that the tests for (mis)matches should not generate code other than Scala's
`if`s, that is, neither (behavior) methods nor (spawned) actors; only in the innermost
branch (`if` for match, `else` for mismatch) should there occur such spawning of an actor
given its `Behavior`.

Let us start with prefixes, and later continue with the (cases for) leaves.

### emit

The `emit` extension method.

### generate

The `generate` extension method has as return type a tuple `(Option[Defn.Def], Int)`:
the first is a start of nested definitions, unless empty, while the second is the
parallelism if the receiver is a replication that declares a scaling (a limit
to the current number of started replications).

There are seven cases of code generation:

1. inaction - in this case, the method returns `(None, -1)`, signifying there is
   nothing to generate

1. cases sum

1. unary (fallthrough) summation - if there is no scaling, one operand means just
   composition (if not even just a sequence)

1. summation -

1. unary (fallthrough) composition - unless there is a parallelism involved (meaning
   the operand is a replication - following possible prefixes - with a scaling),
   the code generated for the operand is returned

1. composition - the code is generated by first generating code for the components,
   some of which may be replications with scaling: for each of these, a
   semaphore is created and passed to the corresponding definition (the code generated
   for such component), which must thus already have a parameter - this is taken care
   of upon the return from the `generate` method for sequences. The statements for
   spawning the actors corresponding to the components are appended an invocation to
   `foreach` with receiver the list of the spawned actors (this will send a message
   `Left(None)` to each actor). A definition is generated, which nests the definitions
   for the components, appended with a "Behaviors.receive" block, wrapping the previous
   statements. If the `semaphore: Option[String]` parameter to `generate` is present,
   this asks for a "release" of the semaphore either in the `Behaviors.empty` (`if`)
   branch or in the solitary `Behaviors.stopped` (`else`) branch.

1. sequence - here is where prefixes are mixed with sums or leaves. A sequence consists
   in prefixes followed by either inaction, summation, or a leaf:

   - (mis)match,
   - (possibly guarded) replication,
   - (macro) instantiation, or
   - (agent) invocation.

   In this case, the method `scheme` will implement a scheme for generating code (for a
   leaf). The call diagram is quite contrived, because it involves a definition not yet
   existent, a forward reference to it, a callback yet to be invoked, and the result
   dependent on the case/scheme. And, after all this, another definition will actually
   nest all the generated code (initial statements and the last actual `Behavior`): it
   will be the result of `generate` for a sequence.

   First, from invoking [`emit`](#emit) for each prefix, we get (1) the "channel" actors
   that are spawned before the for-comprehension, and (2) the `code` that is a list of
   `Enumerator`s. If the latter is empty, it means there are no prefixes, and this
   distinction is important.

   Second, there are two (nested) methods that are generated: one corresponds to the
   summation or leaf at the end of the sequence (of prefixes), the other to the
   prefixes themselves. Special cases are if the former is inaction and if the latter
   are empty.

   If the former is inaction, then the first method just returns `Behaviors.stopped`.
   If the latter are empty, then the second method returns just a thunk invoking
   the first method. If there are prefixes, however, then the second method uses
   `pipeToSelf` to step through the prefixes, and then yield the thunk invoking
   the first method: this thunk - when the `Future` is ready - will be a message
   to the current actor itself, but also will represent the next behavior, because
   upon the receipt of the message-behavior, it sends itself a continuation message
   `Left(None)`, prior to becoming the behavior in this message (which is a `Right`).

   Thus, empty prefixes correspond to no in-between messages at all, while some
   prefixes will amount to exactly _two_ messages: one for the for-comprehension
   and another dummy one (a slight overhead in the design). Of course, the thunk
   carries a method name, but also parameters which are _all_ the names emitted
   in the for-comprehension, no duplication: this means that the _last_ in scope
   will shadow any previous name - which is as desired.

   Third, the control is yielded to [`scheme`](#scheme) which is passed a "forward
   reference" representing the thunk, i.e., to the very method that is being defined.
   This is only useful to replication leaves in order to replicate themselves.

   Fourth, a scheme is passed also a callback, upon the invocation of which _some_
   code is generated: the nested method corresponding to the thunk, in turn itself
   with nested statements (a list that is the first argument to the callback) and a
   body (depending on or being the second argument to the callback), finally the
   _body_ of the "nesting" method currently `generate`d: as mentioned, either just
   the thunk - for the empty prefixes, or - for some prefixes - a two-messages
   implementation leading to the thunk-behavior.

   Fifth, the rest of the code is generated: the "nesting" method, which is
   parameterless, unless the leaf is a replication that declares a scaling
   (or parallelism), when a parameter is added to the method: this will be
   a semaphore visible in both the currently `generate`d method and in all
   the nested methods.

### scheme

This method invokes `generate` recursively (which in turn may invoke `scheme`).
It has two parameters:

1. `behavior: Term.Apply` - this is a thunk to a method used solely by guarded replication
   leaves to repeat their behavior after the prefixes and just before the guard

1. `callback: (List[Stat], Term.Apply | List[Stat]) => Unit` this is a function which
   must be called once, that captures the body of the definition returned by `generate`
   in a local variable of `generate`. If the scheme has nested definitions, these must
   be passed as the first argument to the callback, while the second argument defines
   how the last statement (the parameter to a `Behaviors.receive` block) is generated;
   there can be three cases:

   1. The second argument specifies how to spawn an actor (a call to its method with return
      type `Behavior[Π]`); the parameter to the issued `Behaviors.receive` block are two
      statements that respectively spawn an actor and send it a `Left(None)` message,
      then becoming `Behaviors.empty`.

   1. The second argument is already a `Behaviors.receive` block - this one is used, none
      other is generated.

   1. The second argument is a list of statements (possibly just one), which are wrapped
      in a `Behaviors.receive` block, and includes the becoming behavior.

   For conformity, the first argument must not be `Nil` (use `Lit.Unit() :: Nil` instead).

There are nine cases - of which four are replication - that are "code generation schemes":

1. inaction - the `generate`d definition has no nested methods (but `Lit.Unit()` is used
   instead), and the second argument is in case 3: a single `Behaviors.stopped`.

1. summation - the defintion `generate`d from the summation is the only nested method,
   and so the second argument is in case 1: a thunk to the behavior returned by the
   nested method.

1. (mis)match - code is `generate`d for either summation of the two branches; for either
   branch, if the generated method exists (the branch is present and the summation is
   not inaction), the code for spawning an actor and sending it a `Left(None)` message,
   wrapped in a `Term.Block`, will be the term in the corresponding branch of a `Term.If`;
   this latter will be the sole statement in the second argument (to the callback), thus
   in case 3. The first argument will be at most two nested `generate`d methods, or else
   `Lit.Unit()`.

1. unguarded replication - in this simplest case of a replication leaf, the second
   argument - in case 3 - to the callback ends with `Behaviors.same`, after it sends
   the message `Left(None)` to itself in order to replicate. If there is a nested
   definition `generate`d, prior this is spawned and the resulting actor sent the
   message `Left(None)`.

1. guarded replication with bound output guard -

1. guarded replication with input guard
1. guarded replication with output guard

1. (macro) instantiation
1. invocation

#### inaction



### generateʹ


Optimizer
---------


Runtime
-------


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `akka` contains three files: `pi.scala`, `pi_.scala`, and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/pi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ pi -akka ex.scala

or - if stopping output prefix replication -, add an underscore:

    ./examples $ pi_ -akka ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ pio -akka ex

To get the intermediary `in/ex.scala.in` file, execute the `pin` command in the `sbt` shell:

    sbt:π-Calculus[experimental]2Scala> pin -kk ex

Or, if you suspect the [optimizer](#optimizer) is buggy, try disabling it:

    sbt:π-Calculus[experimental]2Scala> pin -kk -O0 ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ pi -akka --interactive ex1.scala ex2.scala

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
