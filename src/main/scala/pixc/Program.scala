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
          * =
            if it.isEmpty
            then
              end.generate

            else
              val ios = `.`(end, it.tail*).generate

              it.head match
                case xa @ χ(Right(`⟦⟧`(_, _, uuid, Symbol(name), _)), r) =>
                  `_ <- *`(s"""τ(${rate(r)})("${xa.uuid}")""".parse[Term].get) ::
                  `_ <- *`(`( * ).parMap1 { (_, …) => }`(`* <- }{(*)()()`(name, uuid) :: ios)) :: Nil

                case xa @ χ(Left(Symbol(name)), r) =>
                  `_ <- *`(s"""τ(${rate(r)})("${xa.uuid}")""".parse[Term].get) ::
                  `_ <- }{(*)()()`(name) :: ios

                case _ =>
                  it.head.generate ++ ios

        case _: χ => ??? // handled above

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> "ν") }.toList


        case it @ τ(Some((Left(enums)), _), r) =>
          * = `_ <- *`(s"""τ(${rate(r)})("${it.uuid}")""".parse[Term].get)
          * ++= enums

        case it @ τ(Some((Right(term)), _), r) =>
          * = `_ <- *`(s"""τ(${rate(r)})("${it.uuid}")""".parse[Term].get)
          * :+= `_ <- IO { * }`(term)

        case it @ τ(_, r) =>
          * = `_ <- *`(s"""τ(${rate(r)})("${it.uuid}")""".parse[Term].get)


        case it @ π(λ(Symbol(ch)), λ(Symbol(arg)), false, r, Some((Left(enums)), _)) =>
          val code = `for * yield ()`(enums*)
          * = `_ <- *`(Term.Apply(
                         Term.Apply(
                           Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: \(arg) :: \(")(") :: Nil, None)),
                           Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                         ),
                         Term.ArgClause(code::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(Symbol(arg)), false, r, Some((Right(term)), _)) =>
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

        case it @ π(λ(Symbol(ch)), λ(Expr(term)), false, r, Some((Left(enums)), _)) =>
          val code = `for * yield ()`(enums*)
          * = `_ <- *`(Term.Apply(
                         Term.Apply(
                           Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: term :: \(")(") :: Nil, None)),
                           Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                         ),
                         Term.ArgClause(code::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(Expr(term)), false, r, Some((Right(term2)), _)) =>
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

        case it @ π(λ(Symbol(ch)), λ(arg), false, r, Some((Left(enums)), _)) =>
          val code = `for * yield ()`(enums*)
          * = `_ <- *`(Term.Apply(
                         Term.Apply(
                           Term.Apply(\(ch), Term.ArgClause(s"${rate(r)}".parse[Term].get :: s"$arg".parse[Term].get :: \(")(") :: Nil, None)),
                           Term.ArgClause(s""""${it.uuid}"""".parse[Term].get::Nil, None)
                         ),
                         Term.ArgClause(code::Nil, None)
                       ))

        case it @ π(λ(Symbol(ch)), λ(arg), false, r, Some((Right(term)), _)) =>
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

        case π(_, _, true, _, Some((Left(_), _))) => ??? // Scalameta Enumerator - caught by parser

        case it @ π(λ(Symbol(ch)), λ(Symbol(par)), true, r, Some((Right(code), _))) =>
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

        ////////////////////////////////////////////// restriction | prefixes //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case `?:`(((λ(lhs), λ(rhs)), mismatch), t, Some(f)) =>
          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), f.generate, t.generate))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), t.generate, f.generate))

        case it: `?:` =>
          def cases(sum: `+`): Term =
            sum match
              case `+`(_, `|`(`.`(`?:`(((λ(lhs), λ(rhs)), mismatch), t, None)))) =>
                if mismatch
                then
                  `if * then … else …`(====(lhs -> rhs), `_ <- *`(`π-exclude`(t.enabled)), cases(t))
                else
                  `if * then … else …`(====(lhs -> rhs), cases(t), `_ <- *`(`π-exclude`(t.enabled)))
              case _ =>
                sum.generate

          * = `_ <- *`(cases(`+`(null, `|`(`.`(it)))))

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

        case `⟦⟧`(_, sum, uuid, Symbol(name), assign) =>
          val ** = assign
            .map { _.map(_.name -> _.name)
                    .map(Pat.Var(_) -> _)
                    .map(Enumerator.Val(_, _))
                    .toList
            }.getOrElse(Nil)

          * = `_ <- *`(`( * ).parMap1 { (_, …) => }`(`* <- χ; _ <- }{()(, *)`(name, uuid) ++ sum.generate))

        case `⟦⟧`(Encoding(_, _, _, _, bound), _sum, _, _, assign) =>
          val ** = assign
            .map { _.map(_.name -> _.name)
                    .map(Pat.Var(_) -> _)
                    .map(Enumerator.Val(_, _))
                    .toList
            }.getOrElse(Nil)

          val n = assign.map(_.size).getOrElse(0)

          val sum = ( if bound.size == n
                      then
                        _sum
                      else
                        `+`(null, `|`(`.`(_sum, ν(bound.drop(n).map(_.name).toSeq*))))
                    )

          * = ** ++ sum.generate

        case _: `{}` => ???

        //////////////////////////////////////////////////////////// encoding //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, _, params*) =>
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

          * = `_ <- *`(s"`$identifier`(`)(`)(${args.mkString(", ")})(using `π-uuid`)".parse[Term].get)

        ////////////////////////////////////////////////////////// invocation //

      *


  def id = "_" + UUID.randomUUID.toString.replaceAll("-", "_")
