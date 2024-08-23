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

package pixc

import java.util.UUID

import scala.meta._
import dialects.Scala3

import parser.Calculus._
import parser.`Pre | AST`
import generator.Meta._


object Program:

  def apply(prog: List[Bind]): List[String] =
    prog.map { (bind, sum) => defn(bind, body(sum)).toString }


  def body(node: `Pre | AST`): List[Enumerator] =
    var * = List[Enumerator]()

    node match

      // SUMMATION /////////////////////////////////////////////////////////////

      case `∅` =>
        * = `_ <- IO.unit`

      case `+`(_, operand) =>
        * = body(operand)

      case it: `+` =>
        val ios = it.choices.foldLeft(List[Term]())(_ :+ body(_))

        * = `_ <- *`(`( *, … ).parMapN { (_, …) => }`(ios*))

      ///////////////////////////////////////////////////////////// summation //


      // COMPOSITION ///////////////////////////////////////////////////////////

      case `|`(operand) =>
        * = body(operand)

      case it: `|` =>
        val ios = it.components.foldLeft(List[Term]())(_ :+ body(_))

        * = `_ <- *`(`( *, … ).parMapN { (_, …) => }`(ios*))

      /////////////////////////////////////////////////////////// composition //


      // SEQUENCE //////////////////////////////////////////////////////////////

      case `.`(end, it*) =>
        * = it.headOption match
          case Some(xa @ χ(_, Some(sum), r)) =>
            val ios = body(flatten(`+`(null, `|`(`.`(sum), `.`(end, it.tail*)))))

            `_ <- *`(s"""τ(${rate(r)})("${xa.uuid}")""".parse[Term].get) :: body2(xa, ios)

          case Some(xa @ χ(name, _, r)) =>
            `_ <- *`(s"""τ(${rate(r)})("${xa.uuid}")""".parse[Term].get) ::
            `_ <- *`(Term.Apply(
                       Term.Apply(\("}{"), Term.ArgClause(\(name) :: Nil, None)),
                       Term.ArgClause(\(")(") :: Nil, None)
                     )) ::
            body(`.`(end, it.tail*))

          case Some(_) =>
            body(it.head) ++ body(`.`(end, it.tail*))

          case _ =>
            body(end)

      case _: χ => ??? // handled above

      ////////////////////////////////////////////////////////////// sequence //


      // RESTRICTION | PREFIXES ////////////////////////////////////////////////

      case ν(names*) =>
        * = names.map { it => `* <- *`(it -> "ν") }.toList


      case it @ τ(Some(Left(enums)), r) =>
        * = `_ <- *`(s"""τ(${rate(r)})("${it.uuid}")""".parse[Term].get)
        * ++= enums

      case it @ τ(Some(Right(term)), r) =>
        * = `_ <- *`(s"""τ(${rate(r)})("${it.uuid}")""".parse[Term].get)
        * :+= `_ <- IO { * }`(term)

      case it @ τ(_, r) =>
        * = `_ <- *`(s"""τ(${rate(r)})("${it.uuid}")""".parse[Term].get)


      case it @ π(λ(Symbol(ch)), λ(Symbol(arg)), false, r, Some(Left(enums))) =>
        val code = `for * yield ()`(enums*)
        * = `_ <- *`(Term.Apply(
                       Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: \(arg) :: \(")(") :: Nil, None)),
                         Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                       ),
                       Term.ArgClause(code::Nil, None)
                     ))

      case it @ π(λ(Symbol(ch)), λ(Symbol(arg)), false, r, Some(Right(term))) =>
        val code = `for * yield ()`(`_ <- IO { * }`(term))
        * = `_ <- *`(Term.Apply(
                       Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: \(arg) :: \(")(") :: Nil, None)),
                         Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                       ),
                       Term.ArgClause(code::Nil, None)
                     ))

      case it @ π(λ(Symbol(ch)), λ(Symbol(arg)), false, r, _) =>
        * = `_ <- *`(Term.Apply(
                       Term.Apply( \(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: \(arg) :: \(")(") :: Nil, None)),
                       Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                     ))

      case it @ π(λ(Symbol(ch)), λ(Expr(term)), false, r, Some(Left(enums))) =>
        val code = `for * yield ()`(enums*)
        * = `_ <- *`(Term.Apply(
                       Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: term :: \(")(") :: Nil, None)),
                         Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                       ),
                       Term.ArgClause(code::Nil, None)
                     ))

      case it @ π(λ(Symbol(ch)), λ(Expr(term)), false, r, Some(Right(term2))) =>
        val code = `for * yield ()`(`_ <- IO { * }`(term2))
        * = `_ <- *`(Term.Apply(
                       Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: term :: \(")(") :: Nil, None)),
                         Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                       ),
                       Term.ArgClause(code::Nil, None)
                     ))

      case it @ π(λ(Symbol(ch)), λ(Expr(term)), false, r, _) =>
        * = `_ <- *`(Term.Apply(
                       Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: term :: \(")(") :: Nil, None)),
                       Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                     ))

      case it @ π(λ(Symbol(ch)), λ(arg), false, r, Some(Left(enums))) =>
        val code = `for * yield ()`(enums*)
        * = `_ <- *`(Term.Apply(
                       Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: s"$arg".parse[Term].get :: \(")(") :: Nil, None)),
                         Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                       ),
                       Term.ArgClause(code::Nil, None)
                     ))

      case it @ π(λ(Symbol(ch)), λ(arg), false, r, Some(Right(term))) =>
        val code = `for * yield ()`(`_ <- IO { * }`(term))
        * = `_ <- *`(Term.Apply(
                       Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: s"$arg".parse[Term].get :: \(")(") :: Nil, None)),
                         Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                       ),
                       Term.ArgClause(code::Nil, None)
                     ))

      case it @ π(λ(Symbol(ch)), λ(arg), false, r, _) =>
        * = `_ <- *`(Term.Apply(
                       Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: s"$arg".parse[Term].get :: \(")(") :: Nil, None)),
                       Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                     ))

      case π(_, _, true, _, Some(Left(_))) => ??? // Scalameta Enumerator - caught by parser

      case it @ π(λ(Symbol(ch)), λ(Symbol(par)), true, r, Some(Right(code))) =>
        * = Enumerator.Generator(Pat.Tuple(List(Pat.Var(par), Pat.Wildcard())),
                                 Term.Apply(
                                   Term.Apply(
                                     Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: \(")(") :: Nil, None)),
                                     Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                                   ),
                                   Term.ArgClause(code::Nil, None)
                                 ))

      case it @ π(λ(Symbol(ch)), λ(Symbol(par)), true, r, _) =>
        * = Enumerator.Generator(Pat.Tuple(List(Pat.Var(par), Pat.Wildcard())),
                                 Term.Apply(
                                   Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: \(")(") :: Nil, None)),
                                   Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                                 ))

      case _: π => ??? // caught by parser

      //////////////////////////////////////////////// restriction | prefixes //


      // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR ////////////////////////////

      case it @ `?:`(((λ(lhs), λ(rhs)), mismatch), t, f) =>
        val t_* = body(t)
        val f_* = body(f)
        if mismatch
        then
          * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), f_*, t_*))
        else
          * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), t_*, f_*))

      //////////////////////////// (mis)match | if then else | elvis operator //


      // REPLICATION ///////////////////////////////////////////////////////////

      case `!`(Some(π @ π(_, λ(Symbol(par)), true, _, _)), sum) =>
        val uuid = id

        val `!.π⋯` = body(π) ++
                     `_ <- *`(s"$uuid($par)(`π-uuid`)".parse[Term].get)

        val it =
          `for * yield ()`(
            `_ <- *` {
              Term.If(Term.ApplyUnary("!", par),
                      `IO.cede`,
                      `( *, … ).parMapN { (_, …) => }`(
                        `for * yield ()`(body(sum)*),
                        `for * yield ()`(`!.π⋯`*)
                      )
              )
            }
          )

        * = `* <- *`(uuid -> `IO { def *(*: ()): String => IO[Unit] = { implicit ^ => … } * }`(uuid -> par, it))
        * ++= `!.π⋯`

      case `!`(Some(μ), sum) =>
        val uuid = id
        val uuid2 = id

        val `body(μ)` = body(μ) match
          case (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
            it.copy(pat = Pat.Var(uuid2)) :: tl

        val `!.μ⋯` = `body(μ)` :+ `_ <- *` { Term.If(Term.ApplyInfix(\(uuid2), \("=="),
                                                                     Type.ArgClause(Nil),
                                                                     Term.ArgClause(Lit.Null() :: Nil, None)),
                                                     `IO.cede`,
                                                     s"$uuid(`π-uuid`)".parse[Term].get,
                                                     Nil)
                                           }

        val it =
          `for * yield ()`(
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                `for * yield ()`(body(sum)*),
                `for * yield ()`(`!.μ⋯`*)
              )
            }
          )

        * = `* <- *`(uuid -> `IO { lazy val *: String => IO[Unit] = { implicit ^ => … } * }`(uuid, it))
        * ++= `!.μ⋯`

      case `!`(_, sum) =>
        val uuid = id

        val `!⋯` = `_ <- *`(s"$uuid(`π-uuid`)".parse[Term].get)

        val it =
          `for * yield ()`(
            `_ <- *` {
              `( *, … ).parMapN { (_, …) => }`(
                `for * yield ()`(body(sum)*),
                `for * yield ()`(`!⋯`*)
              )
            }
          )

        * = `* <- *`(uuid -> `IO { lazy val *: String => IO[Unit] = { implicit ^ => … } * }`(uuid, it))
        * ++= `!⋯`

      /////////////////////////////////////////////////////////// replication //


      // INVOCATION ////////////////////////////////////////////////////////////

      case `(*)`(λ(Symbol(identifier)), params*) =>
        val args = params.map {
          case λ(Symbol(name)) => s"`$name`"
          case λ(value) =>
            value match {
              case it: BigDecimal => s"BigDecimal($it)"
              case it: String => s"$it"
              case Expr(it) => s"$it"
            }
        }

        * = `_ <- *`(s"`$identifier`(`)(`)(${args.mkString(", ")})(using `π-uuid`)".parse[Term].get)

      case _: `(*)` => ??? // impossible by syntax

      //////////////////////////////////////////////////////////// invocation //


      // TRANSACTION ///////////////////////////////////////////////////////////

      case `[]`(name, `+`(_, it)) =>
        * = `_ <- *`(`( *, … ).parMapN { (_, …) => }`(`IO.cede`, `* <- χ; _ <- }{()(, *)`(name) ++ body(it)))

      case _: `[]` => ??? // zero or more - caught by parser

      /////////////////////////////////////////////////////////// transaction //

    *

  def body2(xa: χ, ios: List[Enumerator]): List[Enumerator] =
    val sem = id

    val ios1 = `_ <- *`(`π-disable`(xa.uuid, xa.sum.get.enabled)) :: body(xa.sum.get)
    val ios2 = `_ <- *`(`( *, … ).parMapN { (_, …) => }`(`IO.cede`, `* <- χ; _ <- }{()(, *)`(xa.name) ++ ios))

    `* <- Semaphore[IO](1)`(sem) ::
    `_ <- *`(`( *, … ).parMapN { (_, …) => }`(`tryAcquire.ifM`(sem, ios1),
                                              `tryAcquire.ifM`(sem, ios2))) :: Nil


  def id = "_" + UUID.randomUUID.toString.replaceAll("-", "_")
