Pi-calculus in SCala aka PISC ala RISC (experimental)
=====================================================

The π-calculus maps one to one on `Scala` for-comprehensions
"inside" the Scala's `Future[_]` monad.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `Future`.

The for-comprehensions vertically put the prefix (after "`for`")
and the composition/summation (before "`yield`").


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
receiving in `lines #06-10`, the behavior of `P(x)` is spawned as actor `_υ3υ` and it
is "invoked" by sending it the message `Left(None)`, after which the current actor
"stops" with `Behavior.empty` in `line #09`.

### emit

### generate



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

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ pi -akka --interactive ex1.scala ex2.scala

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
