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

  def apply(prog: List[Bind]): List[String] =
    prog.map { (bind, sum) => defn(bind, sum.generate).toString }


  extension(node: `Pre | AST`)

    def generate: List[Enumerator] =
      var * = List[Enumerator]()

      node match

        // SUMMATION ///////////////////////////////////////////////////////////

        case `∅` =>
          * = `_ <- IO.unit`

        case `+`(_, operand) =>
          * = operand.generate

        case it: `+` =>
          val ios = it.choices.foldLeft(List[Term]())(_ :+ _.generate)

          * = `_ <- *`(`( *, … ).parMapN { (_, …) => }`(ios*))

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case `|`(operand) =>
          * = operand.generate

        case it: `|` =>
          val ios = it.components.foldLeft(List[Term]())(_ :+ _.generate)

          * = `_ <- *`(`( *, … ).parMapN { (_, …) => }`(ios*))

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, it*) =>
          * = (it :+ end).foldLeft(*)(_ ++ _.generate)

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

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
                           Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: \(arg) :: Nil, None)),
                           Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                         ),
                         Term.ArgClause(code::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(Symbol(arg)), false, r, Some(Right(term))) =>
          val code = `for * yield ()`(`_ <- IO { * }`(term))
          * = `_ <- *`(Term.Apply(
                         Term.Apply(
                           Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: \(arg) :: Nil, None)),
                           Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                         ),
                         Term.ArgClause(code::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(Symbol(arg)), false, r, _) =>
          * = `_ <- *`(Term.Apply(
                         Term.Apply( \(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: \(arg) :: Nil, None)),
                         Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(Expr(term)), false, r, Some(Left(enums))) =>
          val code = `for * yield ()`(enums*)
          * = `_ <- *`(Term.Apply(
                         Term.Apply(
                           Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get::term::Nil, None)),
                           Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                         ),
                         Term.ArgClause(code::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(Expr(term)), false, r, Some(Right(term2))) =>
          val code = `for * yield ()`(`_ <- IO { * }`(term2))
          * = `_ <- *`(Term.Apply(
                         Term.Apply(
                           Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get::term::Nil, None)),
                           Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                         ),
                         Term.ArgClause(code::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(Expr(term)), false, r, _) =>
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get::term::Nil, None)),
                         Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(arg), false, r, Some(Left(enums))) =>
          val code = `for * yield ()`(enums*)
          * = `_ <- *`(Term.Apply(
                         Term.Apply(
                           Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get::s"$arg".parse[Term].get::Nil, None)),
                           Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                         ),
                         Term.ArgClause(code::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(arg), false, r, Some(Right(term))) =>
          val code = `for * yield ()`(`_ <- IO { * }`(term))
          * = `_ <- *`(Term.Apply(
                         Term.Apply(
                           Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get::s"$arg".parse[Term].get::Nil, None)),
                           Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                         ),
                         Term.ArgClause(code::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(arg), false, r, _) =>
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get::s"$arg".parse[Term].get::Nil, None)),
                         Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                       ))

        case π(_, _, true, _, Some(Left(_))) => ??? // Scalameta Enumerator - caught by parser

        case it @ π(λ(Symbol(ch)), λ(Symbol(par)), true, r, Some(Right(code))) =>
          * = Enumerator.Generator(Pat.Tuple(List(Pat.Var(par), Pat.Wildcard())),
                                   Term.Apply(
                                     Term.Apply(
                                       Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get::Nil, None)),
                                       Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                                     ),
                                     Term.ArgClause(code::Nil, None)
                                   ))

        case it @ π(λ(Symbol(ch)), λ(Symbol(par)), true, r, _) =>
          * = Enumerator.Generator(Pat.Tuple(List(Pat.Var(par), Pat.Wildcard())),
                                   Term.Apply(
                                     Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get::Nil, None)),
                                     Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                                   ))

        case _: π => ??? // caught by parser

        ////////////////////////////////////////////// restriction | prefixes //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case `?:`(((λ(lhs), λ(rhs)), mismatch), t, f) =>
          * = f.map(_.generate).getOrElse(`_ <- *`(`π-exclude`(t.enabled)))

          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), *, t.generate))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), t.generate, *))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case `!`(Some(π @ π(_, λ(Symbol(par)), true, _, _)), sum) =>
          val uuid = id

          val `!.π⋯` = π.generate ++
                       `_ <- *`(s"$uuid($par)(`π-uuid`)".parse[Term].get)

          val it = Term.If(Term.ApplyUnary("!", par),
                           `IO.cede`,
                           `( *, … ).parMapN { (_, …) => }`(
                             sum.generate,
                             `!.π⋯`
                           )
                   )

          * = `* <- *`(uuid -> `IO { def *(*: ()): String => IO[Unit] = { implicit ^ => … } * }`(uuid -> par, it)) :: `!.π⋯`

        case `!`(Some(μ), sum) =>
          val uuid = id
          val uuid2 = id

          val `μ.generate` = μ.generate match
            case (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
              it.copy(pat = Pat.Var(uuid2)) :: tl

          val `!.μ⋯` = `μ.generate` :+ `_ <- *` { Term.If(Term.ApplyInfix(\(uuid2), \("eq"),
                                                                          Type.ArgClause(Nil),
                                                                          Term.ArgClause(Lit.Null() :: Nil, None)),
                                                          `IO.cede`,
                                                          s"$uuid(`π-uuid`)".parse[Term].get,
                                                          Nil)
                                                }

          val it = `( *, … ).parMapN { (_, …) => }`(
                     sum.generate,
                     `!.μ⋯`
                   )

          * = `* <- *`(uuid -> `IO { lazy val *: String => IO[Unit] = { implicit ^ => … } * }`(uuid, it)) :: `!.μ⋯`

        case _ : `!` => ??? // caught by 'parse'

        ///////////////////////////////////////////////////////// replication //


        // ENCODING ////////////////////////////////////////////////////////////

        case `[|]`(_, sum, Some(assign)) =>
          val ** = assign
            .map(Pat.Var(_) -> _)
            .map(Enumerator.Val(_, _))
            .toList
          * = ** ++ sum.generate

        case `[|]`(_, sum, _) =>
         * = sum.generate

        case _: `{}` => ???

        //////////////////////////////////////////////////////////// encoding //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, params*) =>
          val args = params.map {
            case λ(Symbol(name)) => s"`$name`"
            case λ(value) =>
              value match {
                case it: BigDecimal => s"BigDecimal($it)"
                case it: Boolean => it.toString
                case it: String => it.toString
                case Expr(it) => it.toString
              }
          }

          * = `_ <- *`(s"`$identifier`(${args.mkString(", ")})(using `π-uuid`)".parse[Term].get)

        ////////////////////////////////////////////////////////// invocation //

      *

  def id = "_" + UUID.randomUUID.toString.replaceAll("-", "_")
