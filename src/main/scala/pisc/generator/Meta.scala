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

import scala.meta._
import dialects.Scala3

import parser.StochasticPi.Actions
import parser.Calculus.{ `(*)`, Expr }


object Meta:

  def defn(bind: `(*)`, prog: Term): Defn.Def =
    val identifier = bind.identifier.asSymbol.name
    val params = bind.params.map(_.asSymbol.name)

    if identifier == "Main"
    then
      Defn.Def(Nil,
               identifier,
               Member.ParamClauseGroup(
                 Type.ParamClause(Nil),
                 List(`String*`("args"), `(using % : %, / : /)(implicit ^ : String)`),
               ) :: Nil,
               `: IO[Unit]`,
               prog
      )
    else
      Defn.Def(Nil,
               identifier,
               Member.ParamClauseGroup(
                 Type.ParamClause(Nil),
                 List(`(…)`(params*), `(using % : %, / : /)(implicit ^ : String)`),
               ) :: Nil,
               `: IO[Unit]`,
               prog
      )


  def ==== : ((Any, Any)) => Term = {
    case (Symbol(x), Symbol(y)) => s"$x ==== $y".parse[Term].get
    case (Symbol(x), y: BigDecimal) => s"$x ==== $y".parse[Term].get
    case (Symbol(x), y: Boolean) => s"$x ==== $y".parse[Term].get
    case (Symbol(x), y: String) => s"$x ==== $y".parse[Term].get
    case (Symbol(x), Expr(y)) => s"$x ==== $y".parse[Term].get
    case (x: BigDecimal, Symbol(y)) => s"$x ==== $y".parse[Term].get
    case (x: Boolean, Symbol(y)) => s"$x ==== $y".parse[Term].get
    case (x: String, Symbol(y)) => s"$x ==== $y".parse[Term].get
    case (Expr(x), Symbol(y)) => s"$x ==== $y".parse[Term].get
    case (x: BigDecimal, y: BigDecimal) => s"$x ==== $y".parse[Term].get
    case (x: Boolean, y: Boolean) => s"$x ==== $y".parse[Term].get
    case (x: String, y: String) => s"$x ==== $y".parse[Term].get
    case (Expr(x), Expr(y)) => s"$x ==== $y".parse[Term].get
    case (x: BigDecimal, Expr(y)) => s"$x ==== $y".parse[Term].get
    case (x: Boolean, Expr(y)) => s"$x ==== $y".parse[Term].get
    case (x: String, Expr(y)) => s"$x ==== $y".parse[Term].get
    case (Expr(x), y: BigDecimal) => s"$x ==== $y".parse[Term].get
    case (Expr(x), y: Boolean) => s"$x ==== $y".parse[Term].get
    case (Expr(x), y: String) => s"$x ==== $y".parse[Term].get
  }


  val rate: Option[Any] => Term = {
    case Some(w: Long) if w < 0 => s"`∞`(${-w}L)".parse[Term].get
    case Some(w: Long) if w > 0 => s"`⊤`(${w}L)".parse[Term].get
    case Some(r: BigDecimal) => s"`ℝ⁺`(BigDecimal($r))".parse[Term].get
    case Some(Expr(r)) => s"`ℝ⁺`($r)".parse[Term].get
    case Some(Symbol(r)) => s"`ℝ⁺`($r)".parse[Term].get
    case _ => s"`⊤`(1L)".parse[Term].get
  }


  inline implicit def \(* : Enumerator): List[Enumerator] = * :: Nil

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


  val `(using % : %, / : /)(implicit ^ : String)` =
    Term.ParamClause(Term.Param(Mod.Implicit() :: Nil,
                                "^", Some(Type.Name("String")),
                                None) ::
                       List("%", "/")
                       .map { it => Term.Param(Mod.Implicit() :: Nil,
                                               it,
                                               Some(Type.Name(it)),
                                               None)
                       }
                    ,Some(Mod.Implicit()))


  def `:`(name: String, clause: String): Option[Type.Apply] =
    Some(Type.Apply(Type.Name(name), Type.ArgClause(Type.Name(clause) :: Nil)))


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
          case Enumerator.Generator(Pat.Wildcard(), Term.Apply(Term.Apply(Term.Name(_), List(Term.Apply(Term.Name("⊤" | "∞" | "ℝ⁺"), _), _)), _)) =>
            Term.ForYield(*.toList, Lit.Unit())
          case Enumerator.Generator(Pat.Wildcard(), it) =>
            it
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
      else
        *.last match
          case Enumerator.Generator(Pat.Wildcard(), Term.Select(Term.Name("IO"), Term.Name("unit"))) =>
            Term.ForYield(*.init.toList, Lit.Unit())
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
    else
      `for * yield ()`(`_ <- IO.unit`)


  private def `π-supervised(*)`(* : Term): Term =
    * match
      case Term.Select(Term.Name("IO"), Term.Name("unit")) => *
      case _ => Term.Apply(\("π-supervised"), Term.ArgClause(* :: Nil, None))

  def `( *, … ).parMapN { (_, …) => }`(* : Term*): Term =
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
