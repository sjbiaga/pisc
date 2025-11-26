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
package shared
package actors

import scala.annotation.tailrec

import scala.meta.*
import dialects.Scala3

import parser.Calculus.`(*)`


abstract trait Meta extends shared.Meta:

  inline implicit def \(* : Enumerator): List[Enumerator] = * :: Nil

  inline implicit def \\(* : Enumerator): Term = \(*)

  implicit def \(* : List[Enumerator]): Term =
    if *.nonEmpty then `for * yield ()`(* *)
    else \(`_ <- Future.unit`)


  val `_ <- Future.unit` =
    Enumerator.Generator(`* <- …`(), Term.Select("Future", "unit"))

  val `Future.unit` = Term.Select("Future", "unit")

  def `* <- Future.successful(*)`(* : (String, Term)): Enumerator.Generator =
    `* <- *`(*._1 -> Term.Apply(Term.Select("Future", "successful"), Term.ArgClause(*._2 :: Nil)))

  def `_ <- Future { * }`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(),
                         Term.Apply(\("Future"),
                                    Term.ArgClause(Term.Block(* :: Nil) :: Nil)))


  def dfn(body: List[Stat], recv: List[Stat]): `(*)` => Defn.Def =
    case `(*)`("Main", _) =>
      Defn.Def(Nil,
               "Main",
               Member.ParamClauseGroup(
                 Type.ParamClause(Nil),
                 Term.ParamClause(Term.Param(Nil, "args", `:`("List", "String"), None) :: Nil) :: Nil) :: Nil,
               `: Behavior[Π]`,
               Term.Block(body :+ `Behaviors.receive { case Left(it) => if it *; empty else stopped }`(recv)))
    case `(*)`(identifier, _, params*) =>
      Defn.Def(Nil,
               identifier,
               Member.ParamClauseGroup(
                 Type.ParamClause(Nil),
                 `(…)`(params.map(_.asSymbol.name)*).head.paramClauses
               ) :: Nil,
               `: Behavior[Π]`,
               Term.Block(body :+ `Behaviors.receive { case Left(it) => if it *; empty else stopped }`(recv)))

  protected def `(…)`(* : String*) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(*
                        .map(Term.Param(Nil, _, Some(Type.Name("()")), None))
                        .toList,
                       None) :: Nil
    ) :: Nil

  protected val `: Behavior[Π]` = `:`("Behavior", "Π")


  def `sleep(*.…)`(* : Long, `…`: String) =
    Term.Apply(\("πsleep"), Term.ArgClause(Lit.Long(*) :: \(`…`.toUpperCase) :: Nil))


  @tailrec
  final def `for * yield ()`(* : Enumerator*): Term =
    if *.nonEmpty
    then
      if !(*.head.isInstanceOf[Enumerator.Generator])
      then
        `for * yield ()`((`_ <- Future.unit` +: *)*)
      else if *.size == 1
      then
        *.head match
          case Enumerator.Generator(Pat.Wildcard(), it: Term.ForYield) =>
            `for * yield ()`(it.enums*)
          case Enumerator.Generator(Pat.Wildcard(), it) =>
            it
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
      else
        *.last match
          case Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name("Future"), Term.Name("unit"))) =>
            `for * yield ()`(*.init*)
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
    else
      `for * yield ()`(`_ <- Future.unit`)


  def `def * = …`(* : String, `…`: Term): Stat =
    Defn.Def(Nil, *, Nil, None, `…`)


  def `*.acquire`(* : String) = Term.Select(*, "acquire")

  def `*.release`(* : String) = Term.Select(*, "release")

  def `* = Semaphore(…)`(* : String, `…`: Int = 1): Stat =
    Defn.Val(Nil, `* <- …`(*) :: Nil, None, Term.Apply(\("πSem"), Term.ArgClause(Lit.Int(`…`) :: Nil)))


  val `Behaviors.empty` = Term.Select("Behaviors", "empty")

  val `Behaviors.same` = Term.Select("Behaviors", "same")

  val `Behaviors.stopped` = Term.Select("Behaviors", "stopped")

  val `Behaviors.ignore` = Term.Select("Behaviors", "ignore")


  def `Behaviors.receive { case Left(it) => if it *; empty else stopped }`(* : List[Stat]) =
    Term.Apply(Term.Select("Behaviors", "receive"),
               Term.ArgClause(Term.PartialFunction(
                                Case(Pat.Tuple(Pat.Given(Type.Apply(Type.Name("ActorContext"),
                                                                    Type.ArgClause(Type.Name("Π") :: Nil))) ::
                                               Pat.Extract(\("Left"), Pat.ArgClause(Pat.Var("it") :: Nil)) :: Nil),
                                     None,
                                     Term.Block(Term.If(Term.Apply(Term.Apply(Term.Select("it", "fold"),
                                                                              Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                                                   Term.ArgClause(Term.AnonymousFunction(Term.Apply(Term.Select(Term.Placeholder(), "compareAndSet"), Term.ArgClause(Lit.Boolean(false) :: Lit.Boolean(true) :: Nil))) :: Nil)),
                                                        Term.Block(* :+ `Behaviors.empty`),
                                                        Term.Block(`Behaviors.stopped` :: Nil)) :: Nil)) :: Nil
                              ) :: Nil))
