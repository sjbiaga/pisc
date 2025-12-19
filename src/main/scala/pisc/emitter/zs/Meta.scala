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
package zs

import scala.meta.*
import dialects.Scala3

import parser.Calculus.`(*)`


object Meta extends emitter.shared.effects.Meta:

  override protected lazy val \ = "ZStream"

  val `: ZStream[Any, Throwable, Unit]` =
    Some(Type.Apply(\\(\), Type.ArgClause(\\("Any") :: \\("Throwable") :: \\("Unit") :: Nil)))

  def defn(body: Term): `(*)` => Defn.Def =
    case `(*)`("Main", _) =>
      Defn.Def(Nil,
               "Main", `String*`("args"), `: ZStream[Any, Throwable, Unit]`,
               body)
    case `(*)`(identifier, _, _params*) =>
      val params = _params.map(_.asSymbol.name)
      Defn.Def(Nil,
               identifier, `(…)`(params*), `: ZStream[Any, Throwable, Unit]`,
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


  def `* <- ZStream.fromZIO(*)`(* : (String, Term)): Enumerator.Generator =
    `* <- *`(*._1 -> Term.Apply(Term.Select("ZStream", "fromZIO"), Term.ArgClause(*._2 :: Nil)))

  def `_ <- ZStream.fromZIO(*)`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Apply(Term.Select("ZStream", "fromZIO"), Term.ArgClause(* :: Nil)))

  private val `ZStream.fromZIO`: Term => Boolean =
    case Term.Select(Term.Name("ZStream"), Term.Name("fromZIO")) => true
    case Term.Apply(it, _) => `ZStream.fromZIO`(it)
    case Term.ApplyType(it, _) => `ZStream.fromZIO`(it)
    case _ => false

  def `ZStream.fromZIO(…)`(`…`: List[Enumerator]): List[Enumerator] =
    `…`.map {
      case it @ Enumerator.Generator(_, rhs) if `ZStream.fromZIO`(rhs) => it
      case it: Enumerator.Generator => it.copy(rhs = Term.Apply(Term.Select("ZStream", "fromZIO"), Term.ArgClause(it.rhs :: Nil)))
      case it => it
    }


  def `* <- Semaphore.make(…)`(* : String): Enumerator.Generator =
    `* <- ZStream.fromZIO(*)`(* -> Term.Apply(Term.Select("Semaphore", "make"),
                                              Term.ArgClause(Lit.Int(1) :: Nil)))


  def `List( *, … ).mergeAll`(* : Term*): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => \(Nil)
      case it => Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πmergeAll")

  def `List( *, … ).collectAllPar(…)`(* : Term*)(`…`: String): Term =
    *.flatMap {
      case Term.Select(Term.Name(`\\`), Term.Name("unit")) => None
      case it => Some(it)
    } match
      case Nil => \(Nil)
      case it => Term.Apply(Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πcollectAllPar"),
                            Term.ArgClause(`…` :: Nil))
