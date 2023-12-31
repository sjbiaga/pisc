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

import java.util.UUID

import scala.annotation.tailrec
import scala.meta._

import parser.Calculus._


object Program {

  def apply(bind: List[Bind]): List[String] =
    bind.map { case (bind, sum) => defn(bind, sum).toString }


  def defn(bind: `()`, sum: `+`): Defn.Def = {
    val identifier = bind.identifier.asSymbol.name
    val params = bind.params.map(_.asSymbol.name)

    Defn.Def(Nil,
             identifier, `()`(params: _*), `: IO[Unit]`,
             body(sum)())
  }


  def body(node: AST)
          (implicit semaphore: Option[String] = None): List[Enumerator.Generator] = {
    var * = List[Enumerator.Generator]()

    node match {

      // SUMMATION /////////////////////////////////////////////////////////////

      case it @ `+`(operand, _, _*) =>
        semaphore.map(* :+= `_ <- *.acquire`(_))

        implicit val sem = Some(uid)

        * :+= `* <- Semaphore[IO](1)`(sem.get)

        * :+= `_ <- IO.race ( *, … )`(body(operand), body(`+`(it.choices.tail)))

      case `+`(operand, _*) =>
        * = body(operand)

      case _: `+` => ???

      ///////////////////////////////////////////////////////////// summation //


      // COMPOSITION ///////////////////////////////////////////////////////////

      case it @ `|`(_, _, _*) =>
        semaphore.map(* :+= `_ <- *.acquire`(_))

        val fy = it.components.foldLeft(List[Term.ForYield]())(_ :+ body(_)())

        * :+= `_ <- *`(`( *, … ).parMapN { (_, …) => }`(fy: _*))

      case `|`(operand, _*) =>
        * = body(operand)

      case _: `|` =>
        * :+= `_ <- IO.unit`

        semaphore.map(* :+= `_ <- *.acquire`(_))

        * = `_ <- *`(`for * yield ()`(* : _*))

      /////////////////////////////////////////////////////////// composition //


      // RESTRICTION | PREFIXES | (MIS)MATCH | IF THEN ELSE | REPLICATION //////

      case ν(λ(Symbol(name))) =>
        * = `* <- *`(name -> "ν")

      case `τ` =>
        * = `_ <- *`("τ")


      case π(λ(Symbol(_)), par, true) if !par.isSymbol => ??? // not binding a name - caught by parser

      case π(ch,  _, _) if !ch.isSymbol => ??? // not a channel name - caught by parser

      case π(λ(Symbol(ch)), λ(Symbol(arg)), false) =>
        * = `_ <- *`(s"$ch($arg)".parse[Term].get)

      case π(λ(Symbol(ch)), λ(Expr(expr)), false) =>
        * = `_ <- *`(s"$ch($expr)".parse[Term].get)

      case π(λ(Symbol(ch)), λ(arg), false) =>
        * = `_ <- *`(s"$ch($arg)".parse[Term].get)

      case π(λ(Symbol(ch)), λ(Symbol(par)), true) =>
        * = `* <- *`(par -> s"$ch()".parse[Term].get)


      case `[]`(((λ(lhs), λ(rhs)), mismatch), sum) =>
        if (mismatch)
          * = `_ <- *`(`if * then IO.cede else …`(===(lhs -> rhs), body(sum)()))
        else
          * = `_ <- *`(`if !* then IO.cede else …`(===(lhs -> rhs), body(sum)()))


      case `?:`(((λ(lhs), λ(rhs)), mismatch), t, f) =>
        if (mismatch)
          * = `_ <- *`(`if * then … else …`(===(lhs -> rhs), body(f)(), body(t)()))
        else
          * = `_ <- *`(`if * then … else …`(===(lhs -> rhs), body(t)(), body(f)()))


      case `!`(sum) =>
        val uuid = uid

        val it =
          `for * yield ()` {
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                body(sum)(),
                `for * yield ()`(`_ <- IO.unit`, `_ <- *`(uuid))
              )
            }
          }

        * :+= `* <- *`("`$uuid`", `IO { lazy val *: IO[Unit] = …; * }`(uuid, it))
        * :+= `_ <- *`("`$uuid`")

      ////// restriction | prefixes | (mis)match | if then else | replication //


      // AGENT CALL ////////////////////////////////////////////////////////////

      case `()`(λ(Symbol(identifier)), qual, params @ _*) =>
        semaphore.map(* :+= `_ <- *.acquire`(_))

        val args = params.map {
          case λ(Symbol(name)) => name
          case λ(value) =>
            value match {
              case it: BigDecimal => s"BigDecimal($it)"
              case it: String => s"$it"
              case Expr(it) => s"$it"
            }
        }

        if (qual.isEmpty)
          * :+= `_ <- *`(s"`$identifier`(${args.mkString(", ")})".parse[Term].get)
        else
          * :+= `_ <- *`(s"${qual.mkString(".")}.`π`.`$identifier`(${args.mkString(", ")})".parse[Term].get)

      case _: `()` => ??? // impossible by syntax

      //////////////////////////////////////////////////////////// agent call //


      // SEQUENCE //////////////////////////////////////////////////////////////
      // followed possibly either by agent call or another process expression //

      case `.`(ast, it @ _*) if it.isEmpty =>
        * = body(ast)

      case `.`(ast, it @ _*) =>
        semaphore.map(* :+= `_ <- *.acquire`(_))

        * = (it :+ ast).foldLeft(*)(_ ++ body(_)())

        * = `_ <- *`(`for * yield ()`(* : _*))

      ////////////////////////////////////////////////////////////// sequence //

      case it => ???

    }

    *

  }

  def uid = UUID.randomUUID.toString

  def === : ((AnyRef, AnyRef)) => Term = {
    case (Symbol(x), Symbol(y)) => s"$x === $y".parse[Term].get
    case (Symbol(x), y: String) => s"$x === $y".parse[Term].get
    case (Symbol(x), Expr(y)) => s"$x === $y".parse[Term].get
    case (x: String, Symbol(y)) => s"$x === $y".parse[Term].get
    case (Expr(x), Symbol(y)) => s"$x === $y".parse[Term].get
    case (x: String, y: String) => s"$x === $y".parse[Term].get
    case (x: String, Expr(y)) => s"$x === $y".parse[Term].get
    case (Expr(x), y: String) => s"$x === $y".parse[Term].get
  }


  @inline implicit def \(* : Enumerator.Generator): List[Enumerator.Generator] = * :: Nil

  @tailrec
  @inline implicit def \(* : List[Enumerator.Generator]): Term.ForYield =
    if (*.nonEmpty) `for * yield ()`(* : _*)
    else \(`_ <- IO.unit`)

  @inline implicit def \(* : String): Term.Name = Term.Name(*)


  def `()`(* : String*) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil), // []
      Term.ParamClause(*
                        .map(\(_))
                        .map(Term.Param(Nil, _, Some(Type.Name("()")), None))
                        .toList,
                       None) :: Nil
    ) :: Nil


  def `:`(name: String, clause: String): Option[Type.Apply] =
    Some(Type.Apply(Type.Name(name), Type.ArgClause(Type.Name(clause) :: Nil)))


  val `: IO[Unit]` = `:`("IO", "Unit")


  def `* <- …`(* : String*): Pat =
    if (*.size == 0)
      Pat.Wildcard()
    else if (*.size == 1)
      Pat.Var(*.head)
    else
      Pat.Tuple(*.map(\(_)).map(Pat.Var(_)).toList)


  def `* <- *`(* : (String, Term)): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(*._1), *._2)


  val `_ <- IO.unit` = `_ <- IO.*`("unit")


  def `_ <- *`(* : Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), *)


  def `_ <- IO.*`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select("IO", *))


  @tailrec
  def `for * yield ()`(* : Enumerator.Generator*): Term.ForYield =
    if (*.nonEmpty)
      if (*.size == 1)
        *.head match {
          case Enumerator.Generator(Pat.Wildcard(), it: Term.ForYield) =>
            it
          case _ =>
            Term.ForYield(*.toList, Lit.Unit())
        }
      else Term.ForYield(*.toList, Lit.Unit())
    else `for * yield ()`(`_ <- IO.unit`)


  def `_ <- *.acquire`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Select(*, "acquire"))


  def `* <- Semaphore[IO](1)`(* : String): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(*),
                         Term.Apply(Term.ApplyType("Semaphore",
                                                   Type.ArgClause(Type.Name("IO") :: Nil)),
                                    Term.ArgClause(Lit.Int(1) :: Nil, None)
                         )
    )


  def `_ <- IO.race ( *, … )`(* : Term.ForYield*): Enumerator.Generator =
    `_ <- *`(Term.Apply(Term.Select("IO", "race"),
                        Term.ArgClause(List(*(0), *(1)), None)))


  def `( *, … ).parMapN { (_, …) => }`(* : Term.ForYield*): Term =
    Term.Apply(
      Term.Select(Term.Tuple(*.toList), "parMapN"),
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


  def `if * then IO.cede else …`(* : Term, `…`: Term.ForYield): Term.If =
    Term.If(*, Term.Select("IO", "cede"), `…`, Nil)

  def `if !* then IO.cede else …`(* : Term, `…`: Term.ForYield): Term.If =
    Term.If(Term.ApplyUnary("!", *), Term.Select("IO", "cede"), `…`, Nil)


  def `if * then … else …`(* : Term, `…`: Term.ForYield*): Term.If =
    Term.If(*, `…`(0), `…`(1), Nil)


  def `IO { lazy val *: IO[Unit] = …; * }`(* : String, `…`: Term.ForYield): Term =
    Term.Apply("IO",
               Term.ArgClause(Term.Block(
                                Defn.Val(Mod.Lazy() :: Nil,
                                         `* <- …`(*) :: Nil,
                                         `: IO[Unit]`,
                                         `…`
                                ) :: \(*) :: Nil
                              ) :: Nil,
                              None
               )
    )

}
