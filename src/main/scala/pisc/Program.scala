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
          (implicit semaphore: Option[String] = None): List[Enumerator] =
    var * = List[Enumerator]()

    node match

      // SUMMATION /////////////////////////////////////////////////////////////

      case it if `𝟎` == it =>
        semaphore.map(* :+= `_ <- *.acquire`(_))

        * :+= `_ <- *`("𝟎")

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

      case it @ `|`(operand, _*) =>
        * = body(operand)

      /////////////////////////////////////////////////////////// composition //


      // SEQUENCE //////////////////////////////////////////////////////////////

      case `.`(end, it: _*) =>
        semaphore.map(* :+= `_ <- *.acquire`(_))

        * = (it :+ end).foldLeft(*)(_ ++ body(_)())

        * = `_ <- *`(`for * yield ()`(* : _*))

      ////////////////////////////////////////////////////////////// sequence //


      // RESTRICTION | PREFIXES ////////////////////////////////////////////////

      case ν(λ(Symbol(name))) =>
        * = `* <- *`(name -> "ν")

      case `τ`(Some(Left(enums))) =>
        * :+= `_ <- *`("τ")
        * ++= enums

      case `τ`(Some(Right(term))) =>
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

      case `!`(Some(π @ π(_, λ(Symbol(name)), true)), sum) =>
        val uuid = id

        val `!.πP` = body(π)() :+ `_ <- *`(s"$uuid($name)".parse[Term].get)

        val it =
          `for * yield ()`(
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                `for * yield ()`(body(sum)(): _*),
                `for * yield ()`(`!.πP`: _*)
              )
            }
          )

        * :+= `* <- *`(uuid -> `IO { def *(*: ()): IO[Unit] = …; * }`(uuid -> name, it))
        * ++= `!.πP`

      case `!`(Some(μ), sum) =>
        val uuid = id

        val `!.μP` = body(μ)() :+ `_ <- *`(uuid)

        val it =
          `for * yield ()`(
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                `for * yield ()`(body(sum)(): _*),
                `for * yield ()`(`!.μP`: _*)
              )
            }
          )

        * :+= `* <- *`(uuid -> `IO { lazy val *: IO[Unit] = …; * }`(uuid, it))
        * ++= `!.μP`

      case `!`(_, sum) =>
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

      /////////////////////////////////////////////////////////// replication //


      // AGENT CALL ////////////////////////////////////////////////////////////

      case `()`(λ(Symbol(identifier)), qual, params: _*) =>
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

      case it => ???

    *

  def id = "_" + UUID.randomUUID.toString.replaceAll("-", "_")
