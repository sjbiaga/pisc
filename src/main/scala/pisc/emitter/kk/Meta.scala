/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * [Except as contained in this notice, the name of Sebastian I. Gliţa-Catina
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * from Sebastian I. Gliţa-Catina.]
 */

package pisc
package emitter
package kk

import scala.meta.*
import dialects.Scala3


object Meta extends emitter.shared.actors.Meta:

  def dfn(υidυ: String, body: List[Stat]) =
    val bodyʹ = body match
      case (it: Term) :: Nil => it
      case _ => Term.Block(body)
    Defn.Def(Nil, υidυ, `(…)`(), `: Behavior[Π]`, bodyʹ)

  def dfn(υidυ: String, body: Term, args: String*) =
    Defn.Def(Nil, υidυ, `(…)`(args*), `: Behavior[Π]`, body)


  def `* = gACΠ.spawnAnonymous(…)`(* : String, `…`: Term): Stat =
    Defn.Val(Nil, `* <- …`(*) :: Nil, None, Term.Apply(Term.Select("given_ActorContext_Π", "spawnAnonymous"),
                                                       Term.ArgClause(`…` :: Nil)))

  def `* = gACΠ.spawnAnonymous(…)`(* : String, ** : String, `…`: String*): Stat =
    `* = gACΠ.spawnAnonymous(…)`(*, Term.Apply(\(**), Term.ArgClause(`…`.map(\(_)).toList)))


  def `* ! Left(None)`(* : Term) =
    Term.ApplyInfix(*, \("!"), Type.ArgClause(Nil), Term.ArgClause("πNone" :: Nil))

  val `self ! Left(None)` = `* ! Left(None)`(Term.Select("given_ActorContext_Π", "self"))

  def `* ! Left(it)`(* : Term) =
    Term.ApplyInfix(*, \("!"), Type.ArgClause(Nil), Term.ArgClause("it" :: Nil))


  def `List( *, … ).foreach`(* : String*)(`…`: Boolean = false): Term.Apply =
    if `…`
    then
      Term.Apply(`List( *, … ).foreach`(* *)().fun,
                 Term.ArgClause(Term.Apply(\("Some"),
                                           Term.ArgClause(Term.Apply(\("πAB"),
                                                                     Term.ArgClause(Lit.Boolean(false) :: Nil)) :: Nil)) :: Nil))
    else
      Term.Apply(Term.Select(Term.Apply(\("πLs"), Term.ArgClause(*.map(\(_)).toList)), "πforeach"), Term.ArgClause(Nil))


  private def `pipeToSelf { for * yield Right(⋯) } (_.get)`(* : List[Enumerator], `…`: Term.Apply)(** : Option[Term => Term])(using νs: Seq[String]): Term =
    `for * yield ()`(* *) match
      case it: Term.ForYield =>
        val body = Term.Apply(\("Right"), Term.ArgClause(**.fold(`…`)(_(`…`)) :: Nil))
        val block =
          if νs.isEmpty
          then it.copy(body = body)
          else Term.Block {
            νs.map(`* = gACΠ.spawnAnonymous(…)`(_, Term.Apply(\("ν"), Term.ArgClause(Nil)))).toList :+ it.copy(body = body)
          }
        Term.Apply(Term.Apply(Term.Select("given_ActorContext_Π", "pipeToSelf"),
                              Term.ArgClause(block :: Nil)),
                   Term.ArgClause(Term.AnonymousFunction(Term.Select(Term.Placeholder(), "get")) :: Nil))
      case _ =>
        `pipeToSelf { for * yield Right(⋯) } (_.get)`(`_ <- Future.unit` :: *, `…`)(**)

  def `Behaviors.receive { case Right(it) => it case _ => pipeToSelf(*); same }`(* : List[Enumerator], `…`: Term.Apply)(** : Option[Term => Term] = None)(using Seq[String]) =
    Term.Apply(Term.Select("Behaviors", "receive"),
               Term.ArgClause(Term.PartialFunction(
                                Case(Pat.Tuple(Pat.Given(Type.Apply(Type.Name("ActorContext"),
                                                                    Type.ArgClause(Type.Name("Π") :: Nil))) ::
                                               Pat.Extract(\("Right"), Pat.ArgClause(Pat.Var(\("it")) :: Nil)) :: Nil),
                                     None,
                                     Term.Block(`self ! Left(None)` :: \("it") :: Nil)) ::
                                Case(Pat.Tuple(Pat.Given(Type.Apply(Type.Name("ActorContext"),
                                                                    Type.ArgClause(Type.Name("Π") :: Nil))) ::
                                               Pat.Wildcard() :: Nil),
                                     None,
                                     Term.Block(Defn.GivenAlias(Nil, Name.Anonymous(), Nil, Type.Name("ExecutionContext"), Term.Select(Term.Name("given_ActorContext_Π"), Term.Name("executionContext"))) ::
                                                `pipeToSelf { for * yield Right(⋯) } (_.get)`(*, `…`)(**) ::
                                                `Behaviors.same` :: Nil)) :: Nil
                              ) :: Nil))

  def `Behaviors.receive { case _ => * }`(* : List[Stat]) =
    Term.Apply(Term.Select("Behaviors", "receive"),
               Term.ArgClause(Term.PartialFunction(
                                Case(Pat.Tuple(Pat.Given(Type.Apply(Type.Name("ActorContext"),
                                                                    Type.ArgClause(Type.Name("Π") :: Nil))) ::
                                               Pat.Wildcard() :: Nil),
                                     None,
                                     Term.Block(*)) :: Nil
                              ) :: Nil))

  def `Behaviors.receive { case Left(it) => if it * else stopped }`(* : List[Stat]) =
    Term.Apply(Term.Select("Behaviors", "receive"),
               Term.ArgClause(Term.PartialFunction(
                                Case(Pat.Tuple(Pat.Given(Type.Apply(Type.Name("ActorContext"),
                                                                    Type.ArgClause(Type.Name("Π") :: Nil))) ::
                                               Pat.Extract(\("Left"), Pat.ArgClause(Pat.Var("it") :: Nil)) :: Nil),
                                     None,
                                     Term.Block(Term.If(Term.Apply(Term.Apply(Term.Select("it", "fold"),
                                                                              Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                                                   Term.ArgClause(Term.AnonymousFunction(Term.Apply(Term.Select(Term.Placeholder(), "compareAndSet"), Term.ArgClause(Lit.Boolean(false) :: Lit.Boolean(true) :: Nil))) :: Nil)),
                                                        Term.Block(*),
                                                        Term.Block(`Behaviors.stopped` :: Nil)) :: Nil)) :: Nil
                              ) :: Nil))

  def `Behaviors.receive { case Left(it) => if it *; empty else stopped } (release?)`(* : List[Stat])(using `…`: Option[String]) =
    `…` match
      case Some(semaphore) =>
        Term.Apply(Term.Select("Behaviors", "receive"),
                   Term.ArgClause(Term.PartialFunction(
                                    Case(Pat.Tuple(Pat.Given(Type.Apply(Type.Name("ActorContext"),
                                                                        Type.ArgClause(Type.Name("Π") :: Nil))) ::
                                                   Pat.Extract(\("Left"), Pat.ArgClause(Pat.Var("it") :: Nil)) :: Nil),
                                         None,
                                         Term.Block(Term.If(Term.Apply(Term.Apply(Term.Select("it", "fold"),
                                                                                  Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                                                       Term.ArgClause(Term.AnonymousFunction(Term.Apply(Term.Select(Term.Placeholder(), "compareAndSet"), Term.ArgClause(Lit.Boolean(false) :: Lit.Boolean(true) :: Nil))) :: Nil)),
                                                            Term.Block(* :+ `*.release`(semaphore) :+ `Behaviors.empty`),
                                                            Term.Block(`*.release`(semaphore) :: `Behaviors.stopped` :: Nil)) :: Nil)) :: Nil
                                  ) :: Nil))
      case _ =>
        `Behaviors.receive { case Left(it) => if it *; empty else stopped }`(*)


  def release(using String): Term => Term =

    case Term.Block(stats) =>

      val statsʹ = stats.init

      stats.last match

        case Term.If(cond @ Term.ApplyInfix(_, Term.Name("===="), _, _), t, f) =>

          Term.Block(statsʹ :+ Term.If(cond, release(t), release(f)))

        case Term.If(cond, t, f) =>

          Term.Block(statsʹ :+ Term.If(cond, release(t), f))

        case Term.Apply(Term.Select(Term.Name("Behaviors"), Term.Name("receive")),
                        Term.PartialFunction(hd :: tl) :: Nil) =>

          val it = Term.Apply(Term.Select("Behaviors", "receive"),
                              Term.ArgClause(Term.PartialFunction(hd.copy(body = release(hd.body)) :: tl) :: Nil))

          Term.Block(statsʹ :+ it)

        case it: Term.Apply =>

          val df = statsʹ.last match

            case it: Defn.Def =>

              it.copy(body = release(it.body))

          Term.Block(statsʹ.init :+ df :+ it)

        case it =>

          Term.Block(statsʹ :+ `*.release`(summon[String]) :+ it)
