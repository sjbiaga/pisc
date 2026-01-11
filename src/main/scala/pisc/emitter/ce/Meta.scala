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
package ce

import scala.annotation.tailrec

import scala.meta.*
import dialects.Scala3

import parser.Calculus.`(*)`


abstract trait Meta extends emitter.shared.effects.Meta:

  override protected lazy val \ = "IO"

  def defn(body: Term): `(*)` => Defn.Def =
    case `(*)`("Main") =>
      Defn.Def(Nil,
               "Main",
               Member.ParamClauseGroup(
                 Type.ParamClause(Nil),
                 `String*`("args") :: `(using String)(using %, /, \\)`,
               ) :: Nil,
               `: IO[Any]`,
               body
      )
    case `(*)`(identifier, _params*) =>
      val params = _params.map(_.asSymbol.name)
      Defn.Def(Nil,
               identifier,
               Member.ParamClauseGroup(
                 Type.ParamClause(Nil),
                 `(…)`(params*) :: `(using String)(using %, /, \\)`,
               ) :: Nil,
               `: IO[Any]`,
               body
      )


  def `String*`(* : String) =
    Term.ParamClause(Term.Param(Nil, *, Some(Type.Repeated(\\("String"))), None) :: Nil
                    ,None)

  def `(…)`(* : String*) =
    Term.ParamClause(*.map(\(_)).map(Term.Param(Nil, _, Some(\\("()")), None)).toList
                    ,None)

  val `(using String)(using %, /, \\)` =
    Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                Name.Anonymous(), Some(\\("String")),
                                None) :: Nil
                    ,Some(Mod.Using())) ::
    Term.ParamClause(List("%", "/", "\\")
                       .map { it => Term.Param(Mod.Using() :: Nil,
                                               Name.Anonymous(), Some(\\(it)),
                                               None)
                       }
                    ,Some(Mod.Using())) ::
    Nil


  val `: IO[Any]` = `:`(\, "Any")

  val `IO.cede` = Term.Select(\, "cede")


  def `* <- IO.pure(*)`(* : (String, Term)): Enumerator.Generator =
    `* <- *`(*._1 -> Term.Apply(Term.Select(\, "pure"), Term.ArgClause(*._2 :: Nil)))

  private val `IO.*`: Term => Boolean =
    case Term.Select(Term.Name(`\\`), _) => true
    case Term.Apply(it, _) => `IO.*`(it)
    case Term.ApplyType(it, _) => `IO.*`(it)
    case _ => false

  def `_ <- IO { * }`(* : Term): Enumerator.Generator =
    if `IO.*`(*)
    then
      Enumerator.Generator(`* <- …`(), *)
    else
      Enumerator.Generator(`* <- …`(), Term.Apply(\(\), Term.ArgClause(Term.Block(* :: Nil) :: Nil)))

  def `_ <- IO.sleep(*.…)`(* : Long, `…`: String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Apply(Term.Select(\, "sleep"), Term.ArgClause(Term.Select(Lit.Long(*), `…`) :: Nil)))


  val `: String => IO[Any]` =
    `: IO[Any]`.map(Type.Function(Type.FuncParamClause(\\("String") :: Nil), _))


  def `IO { def *(*: ()): String => IO[Any] = { implicit ^ => … }; * }`(* : (String, String), `…`: Term): Term =
    Term.Apply(\(\),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *._1,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Term.Param(Nil,
                                                                                *._2,
                                                                                Some(\\("()")),
                                                                                None) :: Nil, None) :: Nil) :: Nil,
                            `: String => IO[Any]`,
                            Term.Block(
                              Term.Function(
                                Term.ParamClause(Term.Param(Mod.Implicit() :: Nil,
                                                            "^",
                                                            None,
                                                            None) :: Nil, None), `…`
                              ) :: Nil
                            )
                   ) :: \(*._1) :: Nil
                 ) :: Nil
               )
    )


  def `IO { lazy val *: String => IO[Any] = { implicit ^ => … }; * }`(* : String, `…`: Term): Term =
    Term.Apply(\(\),
               Term.ArgClause(
                 Term.Block(
                   Defn.Val(Mod.Lazy() :: Nil,
                            `* <- …`(*) :: Nil,
                            `: String => IO[Any]`,
                            Term.Block(
                              Term.Function(
                                Term.ParamClause(Term.Param(Mod.Implicit() :: Nil,
                                                            "^",
                                                            None,
                                                            None) :: Nil, None), `…`
                              ) :: Nil
                            )
                   ) :: \(*) :: Nil
                 ) :: Nil
               )
    )


object Meta extends emitter.ce.Meta:

  private def `π-supervised(*)`(* : Term): Option[Term] =
    * match
      case Term.Select(Term.Name(`\\`), Term.Name("unit" | "cede")) => None
      case _ => Some(Term.Apply(\("π-supervised"), Term.ArgClause(* :: Nil)))

  @tailrec
  def `List( *, … ).parSequence`(* : Term*): Term =
    if *.exists {
      case Term.Select(Term.Apply(Term.Name("πLs"), _), Term.Name("πparSequence")) => true
      case _ => false
    } then
      `List( *, … ).parSequence`((
        *.flatMap {
          case Term.Select(Term.Name(`\\`), Term.Name("unit" | "cede")) => None
          case Term.Select(Term.Apply(Term.Name("πLs"), ls), Term.Name("πparSequence")) =>
            ls.flatMap {
              case Term.Select(Term.Name(`\\`), Term.Name("unit" | "cede")) => None
              case Term.Apply(Term.Name("π-supervised"), it :: Nil) => Some(it)
              case it => Some(it)
            }
          case it => Some(it)
        })*)
      else
        Term.Select(Term.Apply(\("πLs"), Term.ArgClause(*.flatMap(`π-supervised(*)`).toList)), "πparSequence")
