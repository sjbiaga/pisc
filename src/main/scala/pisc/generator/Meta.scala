/*
 * Copyright (c) 2023-2024 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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
package generator

import scala.annotation.tailrec

import scala.meta._
import dialects.Scala3

import parser.Calculus.{ `(*)`, Expr }


object Meta:

  def defn(bind: `(*)`, prog: Term): Defn.Def =
    val identifier = bind.identifier
    val params = bind.params.map(_.asSymbol.name)

    if identifier == "Main" && params.isEmpty
    then
      Defn.Def(Nil,
               identifier,
               Member.ParamClauseGroup(
                 Type.ParamClause(Nil),
                 `String*`("args") :: `(using ^ : String)(using % : %, / : /)`,
               ) :: Nil,
               `: IO[Any]`,
               prog
      )
    else
      Defn.Def(Nil,
               identifier,
               Member.ParamClauseGroup(
                 Type.ParamClause(Nil),
                 `(…)`(params*) :: `(using ^ : String)(using % : %, / : /)`,
               ) :: Nil,
               `: IO[Any]`,
               prog
      )


  private def `* ==== …`(* : Any, ** : Term): Term =
    Term.ApplyInfix(s"${*}".parse[Term].get,
                    \("===="),
                    Type.ArgClause(Nil),
                    Term.ArgClause(** :: Nil, None))

  private def `… ==== *`(** : Term, * : Any): Term =
    Term.ApplyInfix(**,
                    \("===="),
                    Type.ArgClause(Nil),
                    Term.ArgClause(s"${*}".parse[Term].get :: Nil, None))

  def ==== : ((Any, Any)) => Term = {
    case (Symbol(x), Symbol(y)) => s"$x ==== $y".parse[Term].get
    case (Symbol(x), y: BigDecimal) => s"$x ==== $y".parse[Term].get
    case (Symbol(x), y: Boolean) => s"$x ==== $y".parse[Term].get
    case (Symbol(x), y: String) => s"$x ==== $y".parse[Term].get
    case (Symbol(x), Expr(y)) => `* ==== …`(x, y)
    case (x: BigDecimal, Symbol(y)) => s"$x ==== $y".parse[Term].get
    case (x: Boolean, Symbol(y)) => s"$x ==== $y".parse[Term].get
    case (x: String, Symbol(y)) => s"$x ==== $y".parse[Term].get
    case (Expr(x), Symbol(y)) => `… ==== *`(x, y)
    case (x: BigDecimal, y: BigDecimal) => s"$x ==== $y".parse[Term].get
    case (x: Boolean, y: Boolean) => s"$x ==== $y".parse[Term].get
    case (x: String, y: String) => s"$x ==== $y".parse[Term].get
    case (Expr(x), Expr(y)) => Term.ApplyInfix(x,
                                               \("===="),
                                               Type.ArgClause(Nil),
                                               Term.ArgClause(y :: Nil, None))
    case (x: BigDecimal, Expr(y)) => `* ==== …`(x, y)
    case (x: Boolean, Expr(y)) => `* ==== …`(x, y)
    case (x: String, Expr(y)) => `* ==== …`(x, y)
    case (Expr(x), y: BigDecimal) => `… ==== *`(x, y)
    case (Expr(x), y: Boolean) => `… ==== *`(x, y)
    case (Expr(x), y: String) => `… ==== *`(x, y)
  }


  val rate: Any => Term = {
    case w: Long if w < 0 => Term.Apply(\("∞"), Term.ArgClause(Lit.Long(-w) :: Nil, None))
    case r: BigDecimal => Term.Apply(\("ℝ⁺"),
                                     Term.ArgClause(Term.Apply(\("BigDecimal"),
                                                               Term.ArgClause(Lit.String(r.toString) :: Nil, None)) :: Nil,
                                                    None))
    case Expr(r) => Term.Apply(\("ℝ⁺"), Term.ArgClause(r :: Nil, None))
    case Symbol(r) => Term.Apply(\("ℝ⁺"), Term.ArgClause(\(r) :: Nil, None))
    case w: Long => Term.Apply(\("⊤"), Term.ArgClause(Lit.Long(w) :: Nil, None))
  }


  inline implicit def \(* : Enumerator): List[Enumerator] = * :: Nil

  inline implicit def \\(* : Enumerator): Term = \(*)

  implicit def \(* : List[Enumerator]): Term =
    if *.nonEmpty then `for * yield ()`(* *)
    else \(`_ <- IO.unit`)

  inline implicit def \(* : String): Term.Name = Term.Name(*)


  def `String*`(* : String) =
    Term.ParamClause(Term.Param(Nil, *, Some(Type.Repeated(Type.Name("String"))), None) :: Nil
                    ,None)

  def `(…)`(* : String*) =
    Term.ParamClause(*.map(\(_)).map(Term.Param(Nil, _, Some(Type.Name("()")), None)).toList
                    ,None)


  val `(using ^ : String)(using % : %, / : /)` =
    Term.ParamClause(Term.Param(Mod.Using() :: Nil,
                                "^", Some(Type.Name("String")),
                                None) :: Nil
                    ,Some(Mod.Using())) ::
    Term.ParamClause(List("%", "/")
                       .map { it => Term.Param(Mod.Using() :: Nil,
                                               it, Some(Type.Name(it)),
                                               None)
                       }
                    ,Some(Mod.Using())) ::
    Nil


  def `:`(name: String, clause: String): Option[Type.Apply] =
    Some(Type.Apply(Type.Name(name), Type.ArgClause(Type.Name(clause) :: Nil)))

  val `: IO[Any]` = `:`("IO", "Any")

  val `: IO[Unit]` = `:`("IO", "Unit")


  def `* <- …`(* : String*): Pat =
    if *.size == 0
    then
      Pat.Wildcard()
    else if *.size == 1
    then
      Pat.Var(*.head)
    else
      Pat.Tuple(*.map(\(_)).map(Pat.Var(_)).toList)


  def `* <- *`(* : (String, Term)): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(*._1), *._2)


  val `_ <- IO.unit` = `_ <- IO.*`("unit")

  val `IO.cede` = Term.Select("IO", "cede")


  def `_ <- *`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), *)


  def `_ <- IO.*`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select("IO", *))

  def `_ <- IO { * }`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(),
                         Term.Apply(\("IO"),
                                    Term.ArgClause(Term.Block(* :: Nil) :: Nil, None)))


  @tailrec
  def `for * yield ()`(* : Enumerator*): Term =
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


  private def `π-supervised(*)`(* : Term): Term =
    * match
      case Term.Select(Term.Name("IO"), Term.Name("unit" | "cede")) => *
      case _ => Term.Apply(\("π-supervised"), Term.ArgClause(* :: Nil, None))

  @tailrec
  def `( *, … ).parMapN { (_, …) => }`(* : Term*): Term =
    if *.exists {
      case Term.Apply(Term.Select(Term.Tuple(_), Term.Name("parMapN")), _) => true
      case _ => false
    } then
      `( *, … ).parMapN { (_, …) => }`((
        *.flatMap {
          case Term.Apply(Term.Select(Term.Tuple(ls), Term.Name("parMapN")), _) =>
            ls.map {
              case it @ Term.Select(Term.Name("IO"), Term.Name("unit" | "cede")) => it
              case Term.Apply(Term.Name("π-supervised"), it :: Nil) => it
            }
          case it => it :: Nil
        })*)
      else
        Term.Apply(
              Term.Select(Term.Tuple(*.map(`π-supervised(*)`).toList), "parMapN"),
              Term.ArgClause(Term.Block(
                               Term.Function(
                                 Term.ParamClause(
                                   List.fill(*.size)(Term.Param(Nil, Name.Placeholder(), None, None)),
                                   None
                                 ),
                                 Term.Block(Nil)
                               ) :: Nil
                             ) :: Nil,
                             None
              )
            )


  def `if * then … else …`(* : Term, `…`: Term*): Term.If =
    Term.If(*, `…`(0), `…`(1), Nil)


  val `: String => IO[Unit]` =
    Some(Type.Function(
           Type.FuncParamClause(Type.Name("String") :: Nil),
           Type.Apply(Type.Name("IO"),
                      Type.ArgClause(Type.Name("Unit") :: Nil))))


  def `IO { def *(*: ()): String => IO[Unit] = { implicit ^ => … } * }`(* : (String, String), `…`: Term): Term =
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
                            `: String => IO[Unit]`,
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
                 , None
               )
    )


  def `IO { lazy val *: String => IO[Unit] = { implicit ^ => … } * }`(* : String, `…`: Term): Term =
    Term.Apply(\("IO"),
               Term.ArgClause(
                 Term.Block(
                   Defn.Val(Mod.Lazy() :: Nil,
                            `* <- …`(*) :: Nil,
                            `: String => IO[Unit]`,
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
                 , None
               )
    )


  val `()(null)`: Term =
    Term.Apply(\("()"), Term.ArgClause(Lit.Null() :: Nil, None))
