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

import scala.annotation.tailrec

import scala.meta._

import parser.Calculus._
import generator.Meta.{ `()` => _, _ }


object Program:

  def apply(bind: List[Bind]): List[String] =
    bind.map { case (bind, sum) => defn(bind, body(sum)()).toString }


  def body(node: AST)
          (implicit semaphore: Option[String] = None): List[Enumerator.Generator] =
    var * = List[Enumerator.Generator]()

    node match

      // SUMMATION /////////////////////////////////////////////////////////////

      case _: `𝟎`.type =>

      case it: `+` if it.choices.size > 1 =>
        semaphore.map(* :+= `_ <- *.acquire`(_))

        implicit val sem = Some(id)

        * :+= `* <- Semaphore[IO](1)`(sem.get)

        * :+= `_ <- IO.race ( *, … )`(body(it.choices.head), body(`+`(it.choices.tail: _*)))

      case `+`(operand, _*) =>
        * = body(operand)

      case _: `+` => ???

      ///////////////////////////////////////////////////////////// summation //


      // COMPOSITION ///////////////////////////////////////////////////////////

      case it: `|` if it.components.size > 1 =>
        semaphore.map(* :+= `_ <- *.acquire`(_))

        val fy = it.components.foldLeft(List[Term.ForYield]())(_ :+ body(_)())

        * :+= `_ <- *`(`( *, … ).parMapN { (_, …) => }`(fy: _*))

      case `|`(`.`(_: `𝟎`.type, it*), _*) if it.isEmpty => ???

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

      case `τ`(Some(term)) =>
        * :+= `_ <- *`("τ")
        * :+= `_ <- IO { * }`(term)

      case `τ`(_) =>
        * = `_ <- *`("τ")


      case π(λ(Symbol(_)), par, true) if !par.isSymbol => ??? // not binding a name - caught by parser

      case π(ch,  _, _) if !ch.isSymbol => ??? // not a channel name - caught by parser

      case π(λ(Symbol(ch)), λ(Symbol(arg)), false) =>
        * = `_ <- *`(s"$ch($arg)".parse[Term].get)

      case π(λ(Symbol(ch)), λ(Expr(term)), false) =>
        * = `_ <- *`(Term.Apply(Term.Name(ch), Term.ArgClause(term::Nil, None)))

      case π(λ(Symbol(ch)), λ(arg), false) =>
        * = `_ <- *`(s"$ch($arg)".parse[Term].get)

      case π(λ(Symbol(ch)), λ(Symbol(par)), true) =>
        * = `* <- *`(par -> s"$ch()".parse[Term].get)


      case `?:`(((λ(lhs), λ(rhs)), mismatch), t, f) =>
        if mismatch
        then
          * = `_ <- *`(`if * then … else …`(===(lhs -> rhs), body(f)(), body(t)()))
        else
          * = `_ <- *`(`if * then … else …`(===(lhs -> rhs), body(t)(), body(f)()))


      case `!`(sum) =>
        val uuid = id

        val it =
          `for * yield ()` {
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                body(sum)(),
                `for * yield ()`(`_ <- IO.unit`, `_ <- *`(uuid))
              )
            }
          }

        * :+= `* <- *`(uuid, `IO { lazy val *: IO[Unit] = …; * }`(uuid, it))
        * :+= `_ <- *`(uuid)

      ////// restriction | prefixes | (mis)match | if then else | replication //


      // AGENT CALL ////////////////////////////////////////////////////////////

      case `()`(λ(Symbol(identifier)), qual, params: _*) =>
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

        if qual.isEmpty
        then
          * :+= `_ <- *`(s"`$identifier`(${args.mkString(", ")})".parse[Term].get)
        else
          * :+= `_ <- *`(s"${qual.mkString(".")}.`π`.`$identifier`(${args.mkString(", ")})".parse[Term].get)

      case _: `()` => ??? // impossible by syntax

      //////////////////////////////////////////////////////////// agent call //


      // SEQUENCE //////////////////////////////////////////////////////////////
      // followed possibly either by agent call or another process expression //

      case `.`(end, it: _*) if it.isEmpty =>
        * = body(end)

      case `.`(end, it: _*) =>
        semaphore.map(* :+= `_ <- *.acquire`(_))

        * = (it :+ end).foldLeft(*)(_ ++ body(_)())

        * = `_ <- *`(`for * yield ()`(* : _*))

      ////////////////////////////////////////////////////////////// sequence //

      case it => ???

    *

  def id = UUID.randomUUID.toString
