Pi-calculus in SCala aka PISC ala RISC (experimental)
=====================================================

The π-calculus inner process nest inside outer processes, as nested methods.
These define the behavior of `Pekko` actors corresponding to processes, but
their parameters also make outer names available to inner scope.

Prefixes are mapped one to one on `Scala` for-comprehensions
"inside" the Scala's `Future[_]` monad.

After code generation, the π-calculus "processes" could be
programmatically typed as `Scala` code using `Pekko` and/or `Future`.


Program
-------

There are three extension methods in the program emitter, invoked from the
`Program.Main` class' `apply` function: `AST.generateʹ`, `AST.generate`, and
`Pre.emit`. The former two have as receiver an `AST` and are named after the
word "generate", because they also _generate_ code (like definitions), unlike
the latter with receiver a `Pre` (also an abstract syntax tree) which only _emits_
a list of `Scalameta` `Enumerator`s to be used in for-comprehensions.

The output - for every binding equation - consists in _nested_ definitions, each
with the return type `Behavior[Π]` where `org.apache.pekko.actor.typed.Behavior` is the FQN
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
captured all the new names when the prefixes end.

For instance, equation `Q = ν(x) x(x). P(x)` generates this code:


    01 def Q(): Behavior[Π] = {
    02   def _υ4υ(): Behavior[Π] = {
    03     def _υ2υ(x: `()`): Behavior[Π] = {
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
    20           val _υ1υ = given_ActorContext_Π.spawnAnonymous(ν())
    21           for {
    22             x <- Future.successful(_υ1υ)
    23             x <- x()
    24           } yield Right(_υ2υ(x))
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

Note how the new name `x` corresponds to an actor (assigned to `_υ1υ`) spawned in
`line #20` with the behavior `ν()`: this is how channels are implemented at [runtime](#runtime),
as special actors with two queues, for output and input. In `line #22`, the new name
`x` is introduced as an alias for the freshly spawned actor `_υ1υ` (this is also done
using a generator rather than an assignment in the for-comprehension). In `line #23`
the introduction of `x` is pending on the input on channel `x` (both have the same name,
which is perfectly legal in Scala). When input is available, the for-comprehension will
yield `Right(_υ2υ(x))`: this is a message that the current actor will send to itself,
which wraps in a `Right` the `Behavior` returned from `_υ2υ(x)` - where (the last) `x`
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
      def _υiυ(): Behavior[Π] = {
        def _υ7υ(x: `()`, y: `()`): Behavior[Π] = {
          def _υeυ(): Behavior[Π] = {
            Behaviors.receive {
              case (given ActorContext[Π], _) => {
                val _υfυ = given_ActorContext_Π.spawnAnonymous(P(x))
                val _υgυ = given_ActorContext_Π.spawnAnonymous(P(y))
                πLs(_υfυ, _υgυ).πforeach()
                Behaviors.empty
              }
            }
          }
          Behaviors.receive {
            case (given ActorContext[Π], _) => {
              val _υhυ = given_ActorContext_Π.spawnAnonymous(_υeυ())
              _υhυ ! Left(None)
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
              val _υ6υ = given_ActorContext_Π.spawnAnonymous(ν())
              for {
                x <- Future.successful(_υ6υ)
                y <- x()
              } yield Right(_υ7υ(x, y))
            }(_.get)
            Behaviors.same
        }
      }
      Behaviors.receive { case (given ActorContext[Π], Left(it)) =>
        if (it.fold(true)(_.compareAndSet(false, true))) {
          val _υjυ = given_ActorContext_Π.spawnAnonymous(_υiυ())
          _υjυ ! Left(None)
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

1. `behavior: Term.Name` - this is a (parameterless `def` which returns a) thunk: a closure
   to a method used solely by guarded replication leaves to repeat their behavior
   after the prefixes and just before the guard

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
   instead), and the second argument is in case 3: a single `Behaviors.stopped` statement.

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

   For example, for the agent `R = if True = False then P else Q`, the optimized
   program - up to renaming - is the following:

       def R(): Behavior[Π] = {
         def _υtυ(): Behavior[Π] = {
           def _υkυ(): Behavior[Π] = {
             Behaviors.receive {
               case (given ActorContext[Π], _) => {
                 if (true ==== false) {
                   val _υrυ = given_ActorContext_Π.spawnAnonymous(P())
                   _υrυ ! Left(None)
                   Behaviors.empty
                 } else {
                   val _υsυ = given_ActorContext_Π.spawnAnonymous(Q())
                   _υsυ ! Left(None)
                   Behaviors.empty
                 }
               }
             }
           }
           _υkυ()
         }
         Behaviors.receive { case (given ActorContext[Π], Left(it)) =>
           if (it.fold(true)(_.compareAndSet(false, true))) {
             val _υuυ = given_ActorContext_Π.spawnAnonymous(_υtυ())
             _υuυ ! Left(None)
             Behaviors.empty
           } else {
             Behaviors.stopped
           }
         }
       }

   If, for instance, we introduce some prefixes so that `R` becomes the agent
   `R'(x) = ν(y) x<y>. y<νz>. if True = False then P(y) else Q(z)`, we get the
   following optimized output:

       def `R'`(x: `()`): Behavior[Π] = {
         def _υGυ(): Behavior[Π] = {
           def _υxυ(y: `()`, z: `()`): Behavior[Π] = {
             Behaviors.receive {
               case (given ActorContext[Π], _) => {
                 if (true ==== false) {
                   val _υEυ = given_ActorContext_Π.spawnAnonymous(P(y))
                   _υEυ ! Left(None)    // line#08
                   Behaviors.empty
                 } else {
                   val _υFυ = given_ActorContext_Π.spawnAnonymous(Q(z))
                   _υFυ ! Left(None)    // line #12
                   Behaviors.empty
                 }
               }
             }
           }
           Behaviors.receive {
             case (given ActorContext[Π], Right(it)) =>
               given_ActorContext_Π.self ! Left(None)
               it                                // line #21
             case (given ActorContext[Π], _)         =>
               given ExecutionContext = given_ActorContext_Π.executionContext
               given_ActorContext_Π.pipeToSelf {
                 val _υwυ = given_ActorContext_Π.spawnAnonymous(ν())
                 val _υvυ = given_ActorContext_Π.spawnAnonymous(ν())
                 for {
                   y <- Future.successful(_υwυ)
                   _ <- x(y)
                   z <- Future.successful(_υvυ)
                   _ <- y(z)
                 } yield Right(_υxυ(y, z)) // line #32
               }(_.get)
               Behaviors.same
           }
         }
         Behaviors.receive { case (given ActorContext[Π], Left(it)) =>
           if (it.fold(true)(_.compareAndSet(false, true))) {
             val _υHυ = given_ActorContext_Π.spawnAnonymous(_υGυ())
             _υHυ ! Left(None)
             Behaviors.empty
           } else {
             Behaviors.stopped
           }
         }
       }

   Notice how the behavior _after_ the prefixes is captured in `line #32` (by the
   method, in `line #03`, whom all bound names - `y` and `z` - are passed to), becomes
   in `line #21`, and actually computes in the `if` which either spawns `P(y)` in
   `line #08` or else `Q(z)` in `line #12`.

1. unguarded replication - in this simplest case of a replication leaf, the second
   argument - in case 3 - to the callback ends with `Behaviors.same`, after it sends
   the message `Left(None)` to itself in order to replicate. If there is a nested
   definition `generate`d, prior this is spawned and the resulting actor sent the
   message `Left(None)`. If a parallelism is specified, the latter generation
   process is parameterized with a fresh semapohore: it will be released somewhere
   in the nested `generate`d definition, but it is acquired with each receipt of
   a (recurrent) "replication message".

   For example, the process `R = ! 2 * if True = False then P else Q` `generate`s
   the following optimized code:

       def R(): Behavior[Π] = {
         def _υWυ(): Behavior[Π] = {
           def _υVυ(_υJυ: πSem): Behavior[Π] = {
             def _υIυ(): Behavior[Π] = {
               def _υTυ(): Behavior[Π] = {
                 def _υKυ(): Behavior[Π] = {
                   Behaviors.receive {
                     case (given ActorContext[Π], _) => {
                       if (true ==== false) {
                         val _υRυ = given_ActorContext_Π.spawnAnonymous(P())
                         _υRυ ! Left(None)
                         _υJυ.release    // line #12
                         Behaviors.empty
                       } else {
                         val _υSυ = given_ActorContext_Π.spawnAnonymous(Q())
                         _υSυ ! Left(None)
                         _υJυ.release    // line #17
                         Behaviors.empty
                       }
                     }
                   }
                 }
                 _υKυ()
               }
               Behaviors.receive {
                 case (given ActorContext[Π], _) => {
                   _υJυ.acquire                      // line #27
                   val _υUυ = given_ActorContext_Π.spawnAnonymous(_υTυ())
                   _υUυ ! Left(None)
                   given_ActorContext_Π.self ! Left(None) // line #30
                   Behaviors.same
                 }
               }
             }
             _υIυ()
           }
           Behaviors.receive {
             case (given ActorContext[Π], _) => {
               val _υYυ = πSem(2)
               val _υXυ = given_ActorContext_Π.spawnAnonymous(_υVυ(_υYυ)) // line #40
               πLs(_υXυ).πforeach()
               Behaviors.empty
             }
           }
         }
         Behaviors.receive { case (given ActorContext[Π], Left(it)) =>
           if (it.fold(true)(_.compareAndSet(false, true))) {
             val _υZυ = given_ActorContext_Π.spawnAnonymous(_υWυ())
             _υZυ ! Left(None)
             Behaviors.empty
           } else {
             Behaviors.stopped
           }
         }
       }

   The replication in `R` occurs in `line #30`, just after acquiring the 2-permits
   semaphore in `line #27`, semaphore which is passed from `line #40` and released
   in either the `if` branch in `line #12` or in the `else` branch in `line #17`,
   as soon as and wherever the code issued by the replication is done.

1. guarded replication with (silent prefix or) output guard - the code for the guard
   is [`emit`](#emit)ted, and the result of this `Future` is sent as a message to self
   via `pipeToSelf`. But the (successful) value of this `Future` - as a behavior - is
   that from a call to a inner nested method that spawns the code `generate`d for the
   replicated expression: this is how we spawn this actor with each replication.
   Now, in order to replicate - besides spawning this actor -, we would `pipeToSelf`
   a new `Future` again - the very outer method to which we have a "forward reference"
   as the first argument to the `scheme`. So - in the inner nested method - it is used
   as the becoming behavior, just after sending to self a `Left(None)` dummy message.
   The second argument to the callback is in case 2: an existing `Behaviors.receive`
   block.

   For example, the equation `P(a) = !.a<a>.` `generate`s the following code:

       def P(a: `()`): Behavior[Π] = {
         def _υ05υ(): Behavior[Π] = {
           def outer_υ01υ(): Behavior[Π] = {
             def thunk_υ02υ = outer_υ01υ()
             ()
             def inner_υ04υ(): Behavior[Π] = {
               Behaviors.receive { case (given ActorContext[Π], _) =>
                 given_ActorContext_Π.self ! Left(None)
                 thunk_υ02υ // line #09
               }
             }
             Behaviors.receive {
               case (given ActorContext[Π], Right(it)) =>
                 given_ActorContext_Π.self ! Left(None)
                 it                   // line #15
               case (given ActorContext[Π], _)         =>
                 given ExecutionContext = given_ActorContext_Π.executionContext
                 given_ActorContext_Π.pipeToSelf(
                   for (_υ03υ <- a(a))
                     yield Right(
                       if (_υ03υ eq None)
                         Behaviors.stopped
                       else
                         inner_υ04υ() // line #24
                     )
                 )(_.get)
                 Behaviors.same
             }
           }
           outer_υ01υ() // line #30
         }
         Behaviors.receive { case (given ActorContext[Π], Left(it)) =>
           if (it.fold(true)(_.compareAndSet(false, true))) {
             val _υ06υ = given_ActorContext_Π.spawnAnonymous(_υ05υ())
             _υ06υ ! Left(None)
             Behaviors.empty
           } else {
             Behaviors.stopped
           }
         }
       }


   The agent `P(a)` starts directly (without prefixes) a replication, so in `line #30`
   the behavior is returned without ado. Observe that there are other two nested methods,
   an "outer" one and an "inner" one. The outer nested method corresponds to the output
   guard, and in `line #24` - and thereafter in `line #15` -, changes the behavior to the
   inner method, which, because of the inaction, merely reverts the behavior - in
   `line #09` - back to the outer method, ready for the guard again.

1. guarded replication with bound output guard - similar to 5.

   For example, the equation `Q = ν(a) !.a<νb>.` `generate`s the following code:

       def Q(): Behavior[Π] = {
         def _υ0dυ(): Behavior[Π] = {
           def outer_υ08υ(a: `()`): Behavior[Π] = { // line #03
             def thunk_υ09υ = outer_υ08υ(a)
             ()
             def inner_υ0cυ(b: `()`): Behavior[Π] = { // line #06
               Behaviors.receive { case (given ActorContext[Π], _) =>
                 given_ActorContext_Π.self ! Left(None)
                 thunk_υ09υ // line #09
               }
             }
             Behaviors.receive {
               case (given ActorContext[Π], Right(it)) =>
                 given_ActorContext_Π.self ! Left(None)
                 it // line #15
               case (given ActorContext[Π], _)         =>
                 given ExecutionContext = given_ActorContext_Π.executionContext
                 given_ActorContext_Π.pipeToSelf {
                   val _υ0aυ = given_ActorContext_Π.spawnAnonymous(ν())
                   for {
                     b     <- Future.successful(_υ0aυ)
                     _υ0bυ <- a(b)
                   } yield Right(
                     if (_υ0bυ eq None)
                       Behaviors.stopped
                     else
                       inner_υ0cυ(b) // line #27
                   )
                 }(_.get)
                 Behaviors.same
             }
           }
           Behaviors.receive { // line #33
             case (given ActorContext[Π], Right(it)) =>
               given_ActorContext_Π.self ! Left(None)
               it // line #36
             case (given ActorContext[Π], _)         =>
               given ExecutionContext = given_ActorContext_Π.executionContext
               given_ActorContext_Π.pipeToSelf {
                 val _υ07υ = given_ActorContext_Π.spawnAnonymous(ν())
                 for (a <- Future.successful(_υ07υ))
                   yield Right(outer_υ08υ(a)) // line #42
               }(_.get)
               Behaviors.same
           } // line #45
         }
         Behaviors.receive { case (given ActorContext[Π], Left(it)) =>
           if (it.fold(true)(_.compareAndSet(false, true))) {
             val _υ0eυ = given_ActorContext_Π.spawnAnonymous(_υ0dυ())
             _υ0eυ ! Left(None)
             Behaviors.empty
           } else {
             Behaviors.stopped
           }
         }
       }

   This time the agent `Q` starts with a prefix (a new name), and so `lines #33-#45`
   compose these prefixes into a `Future`, result of which is the behavior from
   `line #42` and then `line #36`: the "outer" method from `line #03`. This in turn
   composes a `Future` from the guard, result of which is the behavior from `line #27`
   and then `line #15`: the "inner" method from `line #06`. The latter, because of
   the inaction, then sends itself a repeating dummy mesage, and reverts the behavior - in
   `line #09` - back to the former, ready for the guard again.

   Had we used the expression `ν(b) !.a<νb>.` instead, although the `b` in the bound
   output guard clobbers the `b` from the new name prefix, it would still be all right
   because in `line #09` we would use a "thunk" (which would capture the parameter `b`
   from `line #03`) instead of the presumed `outer_υ08υ(b)` (when `b` would be
   the parameter from `line #06`, not `line #03`).

1. guarded replication with input guard - similar to 5.

   For example, the equation `R = ν(a) !.a(b).` `generate`s the following code:

       def R(): Behavior[Π] = {
         def _υ0jυ(): Behavior[Π] = {
           def outer_υ0iυ(a: `()`): Behavior[Π] = {
             def thunk_υ0gυ = outer_υ0iυ(a)
             ()
             def inner_υ0hυ(b: `()`): Behavior[Π] = {
               Behaviors.receive { case (given ActorContext[Π], _) =>
                 given_ActorContext_Π.self ! Left(None)
                 thunk_υ0gυ // line #09
               }
             }
             Behaviors.receive {
               case (given ActorContext[Π], Right(it)) =>
                 given_ActorContext_Π.self ! Left(None)
                 it
               case (given ActorContext[Π], _)         =>
                 given ExecutionContext = given_ActorContext_Π.executionContext
                 given_ActorContext_Π.pipeToSelf(
                   for (b <- a()) // line 19
                     yield Right(
                       if (!b) // line #21
                         Behaviors.stopped
                       else {
                         inner_υ0hυ(b)
                       }
                     )
                 )(_.get)
                 Behaviors.same
             }
           }
           Behaviors.receive {
             case (given ActorContext[Π], Right(it)) =>
               given_ActorContext_Π.self ! Left(None)
               it
             case (given ActorContext[Π], _)         =>
               given ExecutionContext = given_ActorContext_Π.executionContext
               given_ActorContext_Π.pipeToSelf {
                 val _υ0fυ = given_ActorContext_Π.spawnAnonymous(ν())
                 for (a <- Future.successful(_υ0fυ))
                   yield Right(outer_υ0iυ(a))
               }(_.get)
               Behaviors.same
           }
         }
         Behaviors.receive { case (given ActorContext[Π], Left(it)) =>
           if (it.fold(true)(_.compareAndSet(false, true))) {
             val _υ0kυ = given_ActorContext_Π.spawnAnonymous(_υ0jυ())
             _υ0kυ ! Left(None)
             Behaviors.empty
           } else {
             Behaviors.stopped
           }
         }
       }

   It is obvious that most of the code is the same with the example at 6, except
   for `line #19`: this corresponds there with two generators instead of one here.
   The test in `line #21` is different too, here being a check for `null` of the input
   name/value, while there being a comparison with `None` of the return value of
   the method that does the output. But the idea is the same: an outer and an inner,
   both nested, methods, the former corresponding to the behavior of the actor just
   after the prefixes, i.e., the replication itself, while the latter corresponding
   to the behavior after the input guard, when the actor sends itself a dummy message
   in order to continue replicating, just after spawning the actor for the replicated
   behavior, which in this case is none at all, because of the inaction. To continue
   from the input guard (again), the behavior is reverted back to the outer method,
   using its thunk in `line #09`.

1. macro instantiation

1. agent invocation -

### generateʹ


Optimizer
---------

There are two phases to the optimizer. Phase 1 is due to the fact that the generator
issues an extra method for each agent invocation: in some cases - when the invocation
is not preceded by prefixes - the call to this extra method can be replaced with the
invocation (the direct call to the agent), and this method removed. Phase 2 succeeds
phase 1, and is based on the fact that the code is alas generated uniformly whether
an expression is part of a summation or not; indifferently, the receive block `fold`s an
optional atomic boolean: this must not be the case except when the methods are actually
invoked as part of a summation. Particularly for "cases sum", these methods which are
invoked as part of a summation do not themselves `fold` an atomic boolean, they simply
perform (nested) case analysis and only on the exact (mis)match further invoke the actual
method which must `fold` an atomic boolean: therefore, this situation is handled separately,
and strictly those methods further invoked are optimized in phase 2, not the proxy methods
which perform case analysis.

There are two steps to each phase: the collection of data and the optimization itself. The
former occurs during code generation. The latter - just before the generated program is
converted to `String`, so while it is still a representation as `Scalameta` trees.

The simplest collection of data is for phase 2: for a non-unary summation, it marks the
names of the methods generated for each operand of the summation (because of `flatten`ing,
there is such a method for each operand), unless it is a "cases sum", when these methods
would be just proxies, so it marks instead the method indirectly associated with each proxy.

The marked methods do not get optimized in phase 2. However, there is still a potential for
correction, when the operand is a leaf with no prefixes; in this case, the behavior method,
rather than receiving a message itself, returns the behavior of another method instead,
by calling it: during the second step of phase 2, and unless phase 1 has removed it, the
called method's name is marked for exemption from phase 2 optimization, because the call
to it is detected (as part of phase 2 optimization). The (behavior) methods corresponding
to agents are exempted from phase 2 optimization.

The collection of data for phase 1 is more complex. It is a mutable map, one for each
equation. In five cases yet to be explained (summation, unary or non-unary composition,
sequence, and equation), it maps the name of a (behavior) method currently generated
to a list of all names of the (behavior) methods corresponding to agent invocations,
encountered before any of the five cases reoccurs again.

In order to do this, the [`generate`](#generate) method has a `collect1` parameter
that is a mutable list, appended exactly the names of the generated (behavior) methods
corresponding strictly to agent invocations: this is the case of an agent invocation
leaf when the code is generated for a sequence with this leaf. But the values in the
mutable map may also be indirect references to methods. Thus, in order to replace
a method call with an agent invocation, we must first have a name of this method;
second, we use the indirect reference; third, having a direct reference, we pattern
match and discover how the invocation takes place.

These three steps are referred to as `find`ing the invocation. Last, we `replace`
the call to the method we have a name for with the agent invocation.

On the other hand, we must also remove methods during phase 1, which is why the
receiver of `optimize1` is a `Scalameta` `Defn.Def`, while its return type is an
`Option[Defn.Def]`: it suffices to `flatMap` the method definitions resulting from
code generation using an `Option[Defn.Def]` as target type of the `flatMap` function,
in order to filter out those methods for which `optimize1` returns `None`.


Runtime
-------


Apps (examples)
---------------

The `examples` folder *must* have three sub-folders:

    ./examples/
       pisc/
       in/
       out/

The root project folder `pekko` contains three files: `pi.scala`, `pi_.scala`, and `main.scala.in`.

!!!Warning: do not delete them!!!

One can edit'em, though they're ready to generate a main `App`.

To get and run the examples, one can `source` the functions from `bin/pi.sh`.

To run an example, `cd` to `examples` and execute:

    ./examples $ pi -pekko ex.scala

or - if stopping output prefix replication -, add an underscore:

    ./examples $ pi_ -pekko ex.scala

To get the final source file `ex.scala` (from `out/ex.scala.out`), run:

    ./examples $ pio -pekko ex

To get the intermediary `in/ex.scala.in` file, execute the `pin` command in the `sbt` shell:

    sbt:π-Calculus[experimental]2Scala> pin -kk ex

Or, if you wish to disable the [optimizer](#optimizer):

    sbt:π-Calculus[experimental]2Scala> pin -kk -O0 ex

where `example/pisc/ex.pisc` contains the π-calculus source (equations binding agents to process
expressions).

In order to allow multiple `App`s, edit `examples/ex[12].scala` and add a top-level `package ex[12]` line.

If there are more `App`s' with agents that depend one to another, pass the `--interactive` option and all source files:

    ./examples $ pi -pekko --interactive ex1.scala ex2.scala

Note that [Scala Cli](https://scala-cli.virtuslab.org/) must be installed.
