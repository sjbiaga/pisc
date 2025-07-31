/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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
package generator

import scala.meta.*
import dialects.Scala3

import parser.Calculus.*
import Meta.*


object Program:

  extension (node: Pre | AST)

    def generate(using id: => String): List[Enumerator] =
      var * = List[Enumerator]()

      node match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>
          * = `_ <- IO.unit`

        case +(_, operand) =>
          * = operand.generate

        case it: + =>
          val ios = it.choices.foldRight(List[Term]())(_.generate :: _)

          * = `_ <- *`(`List( *, … ).parSequence`(ios*))

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(operand) =>
          * = operand.generate

        case it: ∥ =>
          val ios = it.components.foldRight(List[Term]())(_.generate :: _)

          * = `_ <- *`(`List( *, … ).parSequence`(ios*))

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, ps*) =>
          * = ps.foldRight(end.generate) {

            case (xa @ χ(Right(`⟦⟧`(_, _, _, υidυ, Symbol(name), _)), r), ios) =>
              `_ <- *`(Term.Apply(
                         Term.Apply(\("τ"),
                                    Term.ArgClause(rate(r.get)::Nil)),
                         Term.ArgClause(Lit.String(xa.υidυ)::Nil))) ::
              `_ <- *`(`List( *, … ).parSequence`(`* <- }{(*)()()`(name, υidυ) :: ios)) :: Nil

            case (xa @ χ(Left(Symbol(name)), r), ios) =>
              `_ <- *`(Term.Apply(
                         Term.Apply(\("τ"),
                                    Term.ArgClause(rate(r.get)::Nil)),
                         Term.ArgClause(Lit.String(xa.υidυ)::Nil))) ::
              `_ <- }{(*)()()`(name) :: ios

            case (it, ios) =>
              it.generate ::: ios

          }

        case _: χ => ??? // handled above

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> "ν") }.toList


        case it @ τ(r, Some((Left(enums)), _)) =>
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\("τ"),
                                    Term.ArgClause(rate(r.get)::Nil)),
                         Term.ArgClause(Lit.String(it.υidυ)::Nil)))
          * :::= enums

        case it @ τ(r, Some((Right(term)), _)) =>
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\("τ"),
                                    Term.ArgClause(rate(r.get)::Nil)),
                         Term.ArgClause(Lit.String(it.υidυ)::Nil)))
          * :+= `_ <- IO { * }`(term)

        case it @ τ(r, _) =>
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\("τ"),
                                    Term.ArgClause(rate(r.get)::Nil)),
                         Term.ArgClause(Lit.String(it.υidυ)::Nil)))


        case it @ π(λ(Symbol(ch)), arg, nu @ (None | Some("ν")), r, code) =>
          nu match
            case None =>
            case _ =>
              val λ(Symbol(par)) = arg
              * = ν(par).generate

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(
                                 Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: \(")(") :: Nil)),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)
                               ),
                               Term.ArgClause(expr::Nil)
                             ))
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- IO { * }`(term))
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(
                                 Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: \(")(") :: Nil)),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)
                               ),
                               Term.ArgClause(expr::Nil)
                             ))
            case _ =>
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: \(")(") :: Nil)),
                               Term.ArgClause(Lit.String(it.υidυ)::Nil)
                             ))

        case π(λ(Symbol(ch)), λ(params: List[`λ`]), Some(cons), _, code) =>
          val args = params.map {
            case λ @ λ(Symbol(_)) if λ.`type`.isDefined => id
            case λ(Symbol(par)) => par
          }

          * = `* :: … :: * = *`(cons -> ch, args*)

          params.zipWithIndex.foreach {
            case (λ @ λ(Symbol(arg)), i) =>
              val par = args(i)
              λ.`type` match
                case Some((tpe, Some(refined))) =>
                  * :+= `* = *: * …`(arg, par, tpe, refined)
                case Some((tpe, _)) =>
                  * :+= `* = *: *`(arg, par, tpe)
                case _ =>
          }

          code match
            case Some((Right(term), _)) =>
              * :+= `_ <- IO { * }`(term)
            case _ =>

        case it @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code) =>
          val par = if λ.`type`.isDefined then id else arg

          code match
            case Some((Right(term), _)) =>
              * = Enumerator.Generator(Pat.Tuple(List(Pat.Var(par), Pat.Wildcard())),
                                       Term.Apply(
                                         Term.Apply(
                                           Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: \(")(") :: Nil)),
                                           Term.ArgClause(Lit.String(it.υidυ)::Nil)
                                         ),
                                         Term.ArgClause(term::Nil)
                                       ))
            case _ =>
              * = Enumerator.Generator(Pat.Tuple(List(Pat.Var(par), Pat.Wildcard())),
                                       Term.Apply(
                                         Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: \(")(") :: Nil)),
                                         Term.ArgClause(Lit.String(it.υidυ)::Nil)
                                       ))

          λ.`type` match
            case Some((tpe, Some(refined))) =>
              * :+= `* = *: * …`(arg, par, tpe, refined)
            case Some((tpe, _)) =>
              * :+= `* = *: *`(arg, par, tpe)
            case _ =>

        case _: π => ??? // caught by parser

        ////////////////////////////////////////////// restriction | prefixes //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((lhs, rhs), mismatch), t, Some(f)) =>
          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), f.generate, t.generate))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), t.generate, f.generate))

        case it: ?: =>
          def cases(sum: +): Term =
            sum match
              case +(_, ∥(`.`(?:(((lhs, rhs), mismatch), t, None)))) =>
                if mismatch
                then
                  `if * then … else …`(====(lhs, rhs), `_ <- *`(`π-exclude`(t.enabled)), cases(t))
                else
                  `if * then … else …`(====(lhs, rhs), cases(t), `_ <- *`(`π-exclude`(t.enabled)))
              case _ =>
                sum.generate

          * = `_ <- *`(cases(`+`(null, ∥(`.`(it)))))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(pace, Some(π @ π(λ(Symbol(ch)), λ(Symbol(par)), Some("ν"), _, _)), sum) =>
          val υidυ = id

          val `!.π⋯` = π.generate :+ `_ <- *`(s"$υidυ(`π-uuid`)".parse[Term].get)

          val `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.π⋯`).getOrElse(`!.π⋯`)

          val it = `List( *, … ).parSequence`(
                     sum.generate,
                     `!⋯`
                   )

          * = `* <- *`(υidυ -> `IO { def *(*: ()): String => IO[Any] = { implicit ^ => … } * }`(υidυ -> par, it)) :: `!.π⋯`

        case !(pace, Some(π @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), _, _)), sum) =>
          val par = if λ.`type`.isDefined then id else arg

          val υidυ = id

          val πʹ = {
            def idʹ: String = π.υidυ
            π.copy(name = λ.copy()(using None))(idʹ)
          }
          val `!.π⋯` = πʹ.generate :+ `_ <- *`(s"$υidυ($arg)(`π-uuid`)".parse[Term].get)

          val `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.π⋯`).getOrElse(`!.π⋯`)

          val `val` =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `val * = *: * …`(arg, par, tpe, refined) :: Nil
              case Some((tpe, _)) =>
                `val * = *: *`(arg, par, tpe) :: Nil
              case _ => Nil

          val it = Term.Block(`val` :::
                              `List( *, … ).parSequence`(
                                sum.generate,
                                `!⋯`
                              ) :: Nil
                   )

          * = `* <- *`(υidυ -> `IO { def *(*: ()): String => IO[Any] = { implicit ^ => … } * }`(υidυ -> par, it)) :: `!.π⋯`

        case !(pace, Some(μ), sum) =>
          val υidυ = id

          val `!.μ⋯` = μ.generate :+ `_ <- *`(s"$υidυ(`π-uuid`)".parse[Term].get)

          val `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.μ⋯`).getOrElse(`!.μ⋯`)

          val it = `List( *, … ).parSequence`(
                     sum.generate,
                     `!⋯`
                   )

          * = `* <- *`(υidυ -> `IO { lazy val *: String => IO[Any] = { implicit ^ => … } * }`(υidυ, it)) :: `!.μ⋯`

        case _ : ! => ??? // caught by 'parse'

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(_, variables, _sum, xid, name, assignment) =>
          val ** = assignment
            .map(_.name -> _.name)
            .map(Pat.Var(_) -> _)
            .map(Enumerator.Val(_, _))
            .toList

          val n = assignment.size

          val sum = if (variables.size == n)
                    then
                      _sum
                    else
                      `+`(null, ∥(`.`(_sum, ν(variables.drop(n).map(_.name).toSeq*))))

          * = ** ::: sum.generate

          name match
            case Symbol(it) =>
              * = `_ <- *`(`List( *, … ).parSequence`(`* <- χ; _ <- }{()(, *)`(it, xid) ::: *))
            case _ =>

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, params*) =>
          val args = params.map(_.toTerm).toList

          * = `_ <- *`(Term.Apply(
                         Term.Apply(
                           Term.Apply(\(identifier), Term.ArgClause(\(")(")::Nil)),
                           Term.ArgClause(args)),
                         Term.ArgClause(\("π-uuid")::Nil, Some(Mod.Using()))))

        ////////////////////////////////////////////////////////// invocation //

      *


  final class Main:

    def apply(prog: List[Bind]): List[String] =
      val id = new helper.υidυ

      ( prog.head match
          case (`(*)`(_, λ(parallelism: Lit.Int)), _) =>
            Defn.Val(Nil, Pat.Var("π-parallelism") :: Nil, None, parallelism).toString :: Nil
          case _ => Nil
      ) :::
      prog
        .tail
        .map(_ -> _.generate(using id()))
        .map(_.swap)
        .map(defn(_)(_).toString)
