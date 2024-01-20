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
import scala.collection.mutable.{ HashMap => Map, LinkedHashSet => Set }

import scala.meta._

import parser.Calculus.{ rate => _, _ }
import generator.Meta.{ `()` => _, _ }


object Program:

  def apply(prog: List[Bind]): List[String] =
    prog.map { case (bind, Some(sum)) => defn(bind, body(sum)()).toString
               case _ => "" }


  def body(node: AST)(init: Boolean = true): List[Enumerator] =
    var * = List[Enumerator]()

    node match

      // SUMMATION /////////////////////////////////////////////////////////////

      case it if `𝟎` == it =>
        * :+= `_ <- *`("𝟎")

      case it: `+` if it.choices.size > 1 =>

        val fy = it.choices.foldLeft(List[Term.ForYield]())(_ :+ body(_)(false))

        if init then * ++= `… = *; _ <- %.update(…)`(it.enabled)

        * :+= `_ <- *`(`( *, … ).parMapN { (_, …) => }`(fy: _*))

      case it @ `+`(_, operand, _*) =>
        if init then * ++= `… = *; _ <- %.update(…)`(it.enabled)

        * ++= body(operand)(false)

      case _: `+` => ???

      ///////////////////////////////////////////////////////////// summation //


      // COMPOSITION ///////////////////////////////////////////////////////////

      case it: `|` if it.components.size > 1 =>
        val fy = it.components.foldLeft(List[Term.ForYield]())(_ :+ body(_)(false))

        * :+= `_ <- *`(`( *, … ).parMapN { (_, …) => }`(fy: _*))

      case `|`(operand, _*) =>
        * = body(operand)(false)

      case _: `|` => ???

      /////////////////////////////////////////////////////////// composition //


      // SEQUENCE //////////////////////////////////////////////////////////////

      case `.`(end, it: _*) =>
        * = (it :+ end).foldLeft(*)(_ ++ body(_)(false))

        * = `_ <- *`(`for * yield ()`(* : _*))

      ////////////////////////////////////////////////////////////// sequence //


      // RESTRICTION | PREFIXES | (MIS)MATCH | IF THEN ELSE | REPLICATION //////

      case ν(λ(Symbol(name))) =>
        * = `* <- *`(name -> "ν")


      case it @ `τ`(Some(Left(enums)), r) =>
        * :+= `_ <- *`(s"τ(${rate(r)})(\"${it.uuid}\")".parse[Term].get)
        * ++= enums

      case it @ `τ`(Some(Right(term)), r) =>
        * :+= `_ <- *`(s"τ(${rate(r)})(\"${it.uuid}\")".parse[Term].get)
        * :+= `_ <- IO { * }`(term)

      case it @ `τ`(_, r) =>
        * :+= `_ <- *`(s"τ(${rate(r)})(\"${it.uuid}\")".parse[Term].get)


      case it @ π(λ(Symbol(_)), par, true, _) if !par.isSymbol => ??? // not binding a name - caught by parser

      case it @ π(ch,  _, _, _) if !ch.isSymbol => ??? // not a channel name - caught by parser

      case it @ π(λ(Symbol(ch)), λ(Symbol(arg)), false, r) =>
        * = `_ <- *`(s"$ch(${rate(r)}, $arg)(\"${it.uuid}\")".parse[Term].get)

      case it @ π(λ(Symbol(ch)), λ(Expr(expr)), false, r) =>
        * = `_ <- *`(s"$ch(${rate(r)}, $expr)(\"${it.uuid}\")".parse[Term].get)

      case it @ π(λ(Symbol(ch)), λ(arg), false, r) =>
        * = `_ <- *`(s"$ch(${rate(r)}, $arg)(\"${it.uuid}\")".parse[Term].get)

      case it @ π(λ(Symbol(ch)), λ(Symbol(par)), true, r) =>
        * = Enumerator.Generator(Pat.Tuple(List(Pat.Var(par), Pat.Wildcard())),
                                 s"$ch(${rate(r)})(\"${it.uuid}\")".parse[Term].get)


      case it @ `?:`(((λ(lhs), λ(rhs)), mismatch), t, f) =>
        if mismatch
        then
          * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), body(f)(), body(t)()))
        else
          * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), body(t)(), body(f)()))


      case `!`(Some(π @ π(_, λ(Symbol(name)), true, _)), sum) =>
        val uuid = id

        val `!.πP` = body(π)() ++
                     `_ <- *`(s"$uuid($name)(`π-uuid`)".parse[Term].get)

        val it =
          `for * yield ()`(
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                `for * yield ()`(body(sum)(false): _*),
                `for * yield ()`(`!.πP`: _*)
              )
            }
          )

        * :+= `* <- *`(uuid -> `IO { def *(*: ()): String => IO[Unit] = { implicit ^ => … } * }`(uuid -> name, it))
        * ++= `!.πP`

      case `!`(Some(μ), sum) =>
        val uuid = id

        val `!.μP` = body(μ)() ++
                     `_ <- *`(s"$uuid(`π-uuid`)".parse[Term].get)

        val it =
          `for * yield ()`(
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                `for * yield ()`(body(sum)(false): _*),
                `for * yield ()`(`!.μP`: _*)
              )
            }
          )

        * :+= `* <- *`(uuid -> `IO { lazy val *: String => IO[Unit] = { implicit ^ => … } * }`(uuid, it))
        * ++= `!.μP`

      case `!`(_, sum) =>
        val uuid = id

        val `!P` = `_ <- IO.unit` ++
                   `… = *; _ <- %.update(…)`(sum.enabled) ++
                   `_ <- *`(s"$uuid(`π-uuid`)".parse[Term].get)

        val it =
          `for * yield ()`(
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                `for * yield ()`(body(sum)(false): _*),
                `for * yield ()`(`!P`: _*)
              )
            }
          )

        * :+= `* <- *`(uuid -> `IO { lazy val *: String => IO[Unit] = { implicit ^ => … } * }`(uuid, it))
        * ++= `!P`

      ////// restriction | prefixes | (mis)match | if then else | replication //


      // AGENT CALL ////////////////////////////////////////////////////////////

      case `()`(λ(Symbol(identifier)), params: _*) =>
        val args = params.map {
          case λ(Symbol(name)) => name
          case λ(value) =>
            value match {
              case it: BigDecimal => s"BigDecimal($it)"
              case it: String => s"$it"
              case Expr(it) => s"$it"
            }
        }

        * :+= `_ <- *`(s"`$identifier`(${args.mkString(", ")})(using `π-uuid`)".parse[Term].get)

      case _: `()` => ??? // impossible by syntax

      //////////////////////////////////////////////////////////// agent call //

      case _ => ???

    *

  def id = "_" + UUID.randomUUID.toString.replaceAll("-", "_")
