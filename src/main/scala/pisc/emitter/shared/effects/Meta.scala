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
package effects

import scala.annotation.tailrec

import scala.meta.*
import dialects.Scala3

import parser.StochasticPi.Actions
import parser.Calculus.`(*)`


abstract trait Meta extends shared.Meta:

  inline implicit def \(* : Enumerator): List[Enumerator] = * :: Nil

  inline implicit def \\(* : Enumerator): Term = \(*)

  implicit def \(* : List[Enumerator]): Term =
    if *.nonEmpty then `for * yield ()`(* *)
    else \(`_ <- IO.unit`)

  val `: IO[Any]` = `:`("IO", "Any")

  val `_ <- IO.unit` = `_ <- IO.*`("unit")

  val `IO.cede` = Term.Select("IO", "cede")

  def `_ <- IO.*`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select("IO", *))

  def `* <- IO.pure(*)`(* : (String, Term)): Enumerator.Generator =
    `* <- *`(*._1 ->Term.Apply(Term.Select("IO", "pure"), Term.ArgClause(*._2 :: Nil)))

  def `_ <- IO { * }`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(),
                         Term.Apply(\("IO"),
                                    Term.ArgClause(Term.Block(* :: Nil) :: Nil)))

  def `_ <- IO.sleep(*.…)`(* : Long, `…`: String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(),
                         Term.Apply(Term.Select("IO", "sleep"),
                                    Term.ArgClause(Term.Select(Lit.Long(*), `…`) :: Nil)))


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
    Term.ParamClause(Term.Param(Nil, *, Some(Type.Repeated(Type.Name("String"))), None) :: Nil
                    ,None)

  def `(…)`(* : String*) =
    Term.ParamClause(*.map(\(_)).map(Term.Param(Nil, _, Some(Type.Name("()")), None)).toList
                    ,None)


  val `: String => IO[Any]` =
    `: IO[Any]`.map(Type.Function(Type.FuncParamClause(Type.Name("String") :: Nil), _))


  val `(using String)(using %, /, \\)` =
    Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                Name.Anonymous(), Some(Type.Name("String")),
                                None) :: Nil
                    ,Some(Mod.Using())) ::
    Term.ParamClause(List("%", "/", "\\")
                       .map { it => Term.Param(Mod.Using() :: Nil,
                                               Name.Anonymous(), Some(Type.Name(it)),
                                               None)
                       }
                    ,Some(Mod.Using())) ::
    Nil


  @tailrec
  final def `for * yield ()`(* : Enumerator*): Term =
    if *.nonEmpty
    then
      if !(*.head.isInstanceOf[Enumerator.Generator])
      then
        `for * yield ()`((`_ <- IO.unit` +: *)*)
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
          case Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name("IO"), Term.Name("unit" | "cede"))) =>
            `for * yield ()`(*.init*)
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
    else
      `for * yield ()`(`_ <- IO.unit`)


  def `_ <- *.acquire`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select(*, "acquire"))

  def `_ <- *.release`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select(*, "release"))

  def `* <- Semaphore[IO](…)`(* : String, `…`: Int): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(*),
                         Term.Apply(Term.ApplyType(\("Semaphore"),
                                                   Type.ArgClause(Type.Name("IO") :: Nil)),
                                    Term.ArgClause(Lit.Int(`…`) :: Nil)
                         )
    )


  def `IO { def *(*: ()): String => IO[Any] = { implicit ^ => … } * }`(* : (String, String), `…`: Term): Term =
    Term.Apply(\("IO"),
               Term.ArgClause(
                 Term.Block(
                   Defn.Def(Nil,
                            *._1,
                            Member.ParamClauseGroup(Type.ParamClause(Nil),
                                                    Term.ParamClause(Term.Param(Nil,
                                                                                *._2,
                                                                                Some(Type.Name("()")),
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


  def `IO { lazy val *: String => IO[Any] = { implicit ^ => … } * }`(* : String, `…`: Term): Term =
    Term.Apply(\("IO"),
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


  def `π-exclude`(enabled: Actions): Term =
    Term.Apply(\("π-exclude"),
               Term.ArgClause(enabled.map(Lit.String(_)).toList))
