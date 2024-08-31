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

import java.util.UUID

import scala.meta._
import dialects.Scala3

import parser.Calculus._
import parser.`Pre | AST`
import generator.Meta._


object Program:

  def apply(bind: List[Bind]): List[String] =
    bind.map { case (bind, sum) => defn(bind, body(sum)()).toString }


  def body(node: `Pre | AST`)
          (implicit semaphore: Option[String] = None): List[Enumerator] =
    var * = List[Enumerator]()

    node match

      // SUMMATION /////////////////////////////////////////////////////////////

      case `∅` =>
        val ** = `_ <- IO.unit`

        semaphore
          .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
          .getOrElse(* ++= **)

      case `+`(operand) =>
        * = body(operand)

      case it: `+` =>
        implicit val sem = Some(id)

        val ios = it.choices.foldLeft(List[Term]())(_ :+ body(_))

        val ** = List(
          `* <- Semaphore[IO](1)`(sem.get),
          `_ <- *`(`( *, … ).parMapN { (_, …) => }`(ios*))
        )

        semaphore
          .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
          .getOrElse(* ++= **)

      ///////////////////////////////////////////////////////////// summation //


      // COMPOSITION ///////////////////////////////////////////////////////////

      case `|`(operand) =>
        * = body(operand)

      case it: `|` =>
        val ios = it.components.foldLeft(List[Term]())(_ :+ body(_)())

        val ** = `_ <- *`(`( *, … ).parMapN { (_, …) => }`(ios*))

        semaphore
          .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
          .getOrElse(* ++= **)

      /////////////////////////////////////////////////////////// composition //


      // SEQUENCE //////////////////////////////////////////////////////////////

      case `.`(end, it*) =>
        val ** = (it :+ end).foldLeft(*)(_ ++ body(_)())

        semaphore
          .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
          .getOrElse(* ++= **)

      ////////////////////////////////////////////////////////////// sequence //


      // RESTRICTION | PREFIXES ////////////////////////////////////////////////

      case ν(names*) =>
        * = names.map { it => `* <- *`(it -> "ν") }.toList

      case τ(Some(Left(enums))) =>
        * :+= `_ <- *`("τ")
        * ++= enums

      case τ(Some(Right(term))) =>
        * :+= `_ <- *`("τ")
        * :+= `_ <- IO { * }`(term)

      case τ(_) =>
        * = `_ <- *`("τ")


      case π(λ(Symbol(ch)), λ(Symbol(arg)), false, Some(Left(enums))) =>
        val code = `for * yield ()`(enums*)
        * = `_ <- *`(Term.Apply(
                       Term.Apply(\(ch), Term.ArgClause(\(arg)::Nil, None)),
                       Term.ArgClause(code::Nil, None)
                     ))

      case π(λ(Symbol(ch)), λ(Symbol(arg)), false, Some(Right(term))) =>
        val code = `for * yield ()`(`_ <- IO { * }`(term))
        * = `_ <- *`(Term.Apply(
                       Term.Apply(\(ch), Term.ArgClause(\(arg)::Nil, None)),
                       Term.ArgClause(code::Nil, None)
                     ))

      case π(λ(Symbol(ch)), λ(Symbol(arg)), false, _) =>
        * = `_ <- *`(Term.Apply(\(ch), Term.ArgClause(\(arg)::Nil, None)))

      case π(λ(Symbol(ch)), λ(Expr(term)), false, Some(Left(enums))) =>
        val code = `for * yield ()`(enums*)
        * = `_ <- *`(Term.Apply(
                       Term.Apply(\(ch), Term.ArgClause(term::Nil, None)),
                       Term.ArgClause(code::Nil, None)
                     ))

      case π(λ(Symbol(ch)), λ(Expr(term)), false, Some(Right(term2))) =>
        val code = `for * yield ()`(`_ <- IO { * }`(term2))
        * = `_ <- *`(Term.Apply(
                       Term.Apply(\(ch), Term.ArgClause(term::Nil, None)),
                       Term.ArgClause(code::Nil, None)
                     ))

      case π(λ(Symbol(ch)), λ(Expr(term)), false, _) =>
        * = `_ <- *`(Term.Apply(\(ch), Term.ArgClause(term::Nil, None)))

      case π(λ(Symbol(ch)), λ(arg), false, Some(Left(enums))) =>
        val code = `for * yield ()`(enums*)
        * = `_ <- *`(Term.Apply(
                       Term.Apply(\(ch), Term.ArgClause(s"$arg".parse[Term].get::Nil, None)),
                       Term.ArgClause(code::Nil, None)
                     ))

      case π(λ(Symbol(ch)), λ(arg), false, Some(Right(term))) =>
        val code = `for * yield ()`(`_ <- IO { * }`(term))
        * = `_ <- *`(Term.Apply(
                       Term.Apply(\(ch), Term.ArgClause(s"$arg".parse[Term].get::Nil, None)),
                       Term.ArgClause(code::Nil, None)
                     ))

      case π(λ(Symbol(ch)), λ(arg), false, _) =>
        * = `_ <- *`(Term.Apply(\(ch), Term.ArgClause(s"$arg".parse[Term].get::Nil, None)))

      case π(_, _, true, Some(Left(_))) => ??? // Scalameta Enumerator - caught by parser

      case π(λ(Symbol(ch)), λ(Symbol(par)), true, Some(Right(code))) =>
        * = `* <- *`(par -> Term.Apply(
                              Term.Apply(\(ch), Term.ArgClause(Nil, None)),
                              Term.ArgClause(code::Nil, None)
                     ))

      case π(λ(Symbol(ch)), λ(Symbol(par)), true, _) =>
        * = `* <- *`(par -> Term.Apply(\(ch), Term.ArgClause(Nil, None)))

      case _: π => ??? // caught by parser

      //////////////////////////////////////////////// restriction | prefixes //


      // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR ////////////////////////////

      case `?:`(((λ(lhs), λ(rhs)), mismatch), t, f) =>
        if mismatch
        then
          * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), body(f)(), body(t)()))
        else
          * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), body(t)(), body(f)()))

      //////////////////////////// (mis)match | if then else | elvis operator //


      ////// REPLICATION ///////////////////////////////////////////////////////

      case `!`(Some(π @ π(_, λ(Symbol(par)), true, _)), sum) =>
        val uuid = id

        val `!.π⋯` = body(π)() :+ `_ <- *`(s"$uuid($par)".parse[Term].get)

        val it = Term.If(Term.ApplyUnary("!", par),
                         `IO.cede`,
                         `( *, … ).parMapN { (_, …) => }`(
                           `for * yield ()`(body(sum)()*),
                           `for * yield ()`(`!.π⋯`*)
                         )
                 )

        * :+= `* <- *`(uuid -> `IO { def *(*: ()): IO[Unit] = …; * }`(uuid -> par, it))
        * ++= `!.π⋯`

      case `!`(Some(μ), sum) =>
        val uuid = id
        val uuid2 = id

        val `body(μ)()` = body(μ)() match
          case (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
            it.copy(pat = Pat.Var(uuid2)) :: tl

        val `!.μ⋯` = `body(μ)()` :+ `_ <- *` { Term.If(Term.ApplyInfix(\(uuid2), \("eq"),
                                                                       Type.ArgClause(Nil),
                                                                       Term.ArgClause(\("None") :: Nil, None)),
                                                       `IO.cede`,
                                                       uuid,
                                                       Nil)
                                             }

        val it = `( *, … ).parMapN { (_, …) => }`(
                   `for * yield ()`(body(sum)()*),
                   `for * yield ()`(`!.μ⋯`*)
                 )

        * :+= `* <- *`(uuid -> `IO { lazy val *: IO[Unit] = …; * }`(uuid, it))
        * ++= `!.μ⋯`

      case `!`(_, sum) =>
        val uuid = id

        val it = `( *, … ).parMapN { (_, …) => }`(
                   body(sum)(),
                   `for * yield ()`(`_ <- IO.unit`, `_ <- *`(uuid))
                 )

        * :+= `* <- *`(uuid, `IO { lazy val *: IO[Unit] = …; * }`(uuid, it))
        * :+= `_ <- *`(uuid)

      /////////////////////////////////////////////////////////// replication //


      // AGENT CALL ////////////////////////////////////////////////////////////

      case `(*)`(λ(Symbol(identifier)), qual, params*) =>
        val args = params.map {
          case λ(Symbol(name)) => s"`$name`"
          case λ(value) =>
            value match {
              case it: BigDecimal => s"BigDecimal($it)"
              case it: String => s"$it"
              case Expr(it) => s"$it"
            }
        }

        if qual.isEmpty
        then
          * :+= `_ <- *`(s"`$identifier`(${args.mkString(", ")})".parse[Term].get)
        else
          * :+= `_ <- *`(s"${qual.mkString(".")}.`π`.`$identifier`(${args.mkString(", ")})".parse[Term].get)

      case _: `(*)` => ??? // impossible by syntax

      //////////////////////////////////////////////////////////// agent call //

    *

  def id = "_" + UUID.randomUUID.toString.replaceAll("-", "_")
