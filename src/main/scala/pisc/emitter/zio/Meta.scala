/*
 * Copyright (c) 2023-2026 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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
package zio

import scala.meta.*
import dialects.Scala3

import parser.Calculus.`(*)`


object Meta extends emitter.shared.effects.Meta:

  override protected lazy val \ = "ZIO"

  override protected lazy val \\ = "succeed"


  def defn(body: Term): `(*)` => Defn.Def =
    case `(*)`("Main", _) =>
      Defn.Def(Nil,
               "Main", `String*`("args"), `: UIO[Any]`,
               body)
    case `(*)`(identifier, _, _params*) =>
      val params = _params.map(_.asSymbol.name)
      Defn.Def(Nil,
               identifier, `(…)`(params*), `: UIO[Any]`,
               body)


  def `String*`(* : String) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(Term.Param(Nil, *, Some(Type.Repeated(\\("String"))), None) :: Nil,
                       None) :: Nil
    ) :: Nil

  def `(…)`(* : String*) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(*
                        .map(Term.Param(Nil, _, Some(\\("()")), None))
                        .toList,
                       None) :: Nil
    ) :: Nil


  val `: UIO[Any]` = `:`("UIO", "Any")

  val `ZIO.unit` = Term.Select(\, "unit")

  def `* <- ZIO.succeed(*)`(* : (String, Term)): Enumerator.Generator =
    `* <- *`(*._1 -> Term.Apply(Term.Select(\, \\), Term.ArgClause(*._2 :: Nil)))

  private val `ZIO.*`: Term => Boolean =
    case Term.Select(Term.Name(`\\`), _) => true
    case Term.Apply(it, _) => `ZIO.*`(it)
    case Term.ApplyType(it, _) => `ZIO.*`(it)
    case _ => false

  def `_ <- ZIO.succeed { * }`(* : Term): Enumerator.Generator =
    if `ZIO.*`(*)
    then
      Enumerator.Generator(`* <- …`(), *)
    else
      Enumerator.Generator(`* <- …`(), Term.Apply(Term.Select(\, \\), Term.ArgClause(Term.Block(* :: Nil) :: Nil)))

  def `_ <- ZIO.sleep(*.…)`(* : Long, `…`: String): Enumerator =
    Enumerator.Generator(`* <- …`(), Term.Apply(Term.Select(\, "sleep"), Term.ArgClause(Term.Select(Lit.Long(*), `…`) :: Nil)))


  def `_ <- *.whenZIO(….tryAcquire)`(* : Term, `…`: String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Apply(Term.Select(*, "whenZIO"), Term.ArgClause(Term.Select(`…`, "tryAcquire") :: Nil)))

  override def `* <- Semaphore(…)`(* : String, `…`: Int): Enumerator =
    Enumerator.Generator(`* <- …`(*),
                                Term.Apply(Term.ApplyType(\("Semaphore"), Type.ArgClause(\\("UIO") :: Nil)),
                                            Term.ArgClause(Lit.Int(`…`) :: Nil))).head


  def `List( *, … ).collectAllPar`(* : Term*): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => `ZIO.unit`
      case it => Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πcollectAllPar")

  def `List( *, … ).collectAllPar`(* : Term*)(`…`: String): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => `ZIO.unit`
      case it => Term.Apply(Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πcollectAllPar"), Term.ArgClause(`…` :: Nil))


  def `\\.\\\\ { def *(*: (), ⋯): UIO[Any] = …; * }`(* : String, `…`: Term, ** : String*): Term =
    Term.Apply(Term.Select(\, \\),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(**.map(Term.Param(Nil,
                                                                                       _,
                                                                                       Some(Type.Name("()")),
                                                                                       None)).toList,
                                                                     None) :: Nil) :: Nil,
                            `: UIO[Any]`,
                             `…`
                   ) :: \(*) :: Nil
                 ) :: Nil
               )
    )

  def `\\.\\\\ { lazy val *: UIO[Any] = …; * }`(* : String, `…`: Term): Term =
    Term.Apply(Term.Select(\, \\),
               Term.ArgClause(Term.Block(
                                Defn.Val(Mod.Lazy() :: Nil,
                                         `* <- …`(*) :: Nil,
                                         `: UIO[Any]`,
                                         `…`
                                ) :: \(*) :: Nil
                              ) :: Nil
               )
    )
