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

package pisc
package emitter
package zs

import scala.meta.*
import dialects.Scala3

import parser.Calculus.*
import zs.Meta.*


object Program:

  private def pace(args: List[Term])(using pace: Option[(Long, String)]) =
    pace match
      case Some((time, unit)) => Term.Select(Lit.Long(time), unit) :: args
      case _ => args

  extension (self: Pre | AST)(using id: => String, ^ : (Enumerator.Generator, Term.Name))

    def emitʹ: List[Enumerator] =

      self match

        case ∥(_, operand) =>
          operand.emitʹ

        case it @ `.`(?:(_, _, None)) =>
          def cases(sum: +): Term =
            sum match
              case +(_, ∥(_, `.`(?:(((lhs, rhs), mismatch), t, None)))) =>
                if mismatch
                then
                  `if * then … else …`(====(lhs, rhs), `_ <- ZStream.fromZIO(*)`(`π-exclude`(t.enabled)), cases(t))
                else
                  `if * then … else …`(====(lhs, rhs), cases(t), `_ <- ZStream.fromZIO(*)`(`π-exclude`(t.enabled)))
              case _ =>
                sum.emit

          `_ <- *`(cases(`+`(-1, ∥(-1, it))))

        case _ => ???

    def emit(implicit _pace: Option[(Long, String)] = None): List[Enumerator] =

      var * = List[Enumerator]()

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>

        case +(_, operand) =>
          * = operand.emit

        case it: + if it.scaling == -1 && it.choices.forall { case ∥(-1, `.`(?:(_, _, None))) => true case _ => false } =>

          val zss = it.choices.foldRight(List[Term]())(_.emitʹ :: _)

          * = `_ <- *`(`List( *, … ).collectAllPar`(zss*))

        case it: + =>
          val zss = it.choices.foldRight(List[Term]())(_.emit :: _)

          * = `_ <- *`(`List( *, … ).collectAllPar`(zss*))

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(_, operand) =>
          * = operand.emit

        case it: ∥ =>
          val zss = it.components.foldRight(List[Term]())(_.emit :: _)

          * = `_ <- *`(`List( *, … ).collectAllPar`(zss*))

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, it*) =>
          * = (it :+ end).foldLeft(*)(_ ::: _.emit)

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> "ν") }.toList

        case it @ τ(r, Some((Left(enums), _))) =>
          * = `_ <- *`(Term.Apply(Term.Apply(\("τ"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                  Term.ArgClause(Lit.String(it.υidυ) :: Nil)))
          * = * ::: `ZStream.fromZIO(…)`(enums)

        case it @ τ(r, Some((Right(term), _))) =>
          * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(\("τ"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                  Term.ArgClause(term :: Nil)))

        case it @ τ(r, _) =>
          * = `_ <- *`(Term.Apply(Term.Apply(\("τ"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                  Term.ArgClause(Lit.String(it.υidυ) :: Nil)))


        case it @ π(λ(Symbol(ch)), λ(Symbol(par)), Some(nu @ "ν"), r, code) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, nu), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, nu), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Select(ch, nu), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

        case it @ π(λ(Symbol(ch)), λ(arg: Term), None, r, code) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "*"), Term.ArgClause(rate(r.get) :: pace(arg :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "*"), Term.ArgClause(rate(r.get) :: pace(arg :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "*"), Term.ArgClause(rate(r.get) :: pace(arg :: Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

        case it @ π(λ(Symbol(ch)), arg, None, r, code) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

        case it @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code) =>

          val par = if λ.`type`.isDefined then id else arg

          code match
            case Some((Right(term), _)) =>
              val expr = term
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          λ.`type` match
            case Some((tpe, Some(refined))) =>
              * :+= `* = *: * …`(arg, par, tpe, refined)
            case Some((tpe, _)) =>
              * :+= `* = *: *`(arg, par, tpe)
            case _ =>

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
              * :+= `_ <- ZStream.fromZIO(*)`(term)
            case _ =>

        case _: π => ??? // caught by parser

        ////////////////////////////////////////////// restriction | prefixes //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((lhs, rhs), mismatch), t, f) =>
          * = f.fold(`_ <- ZStream.fromZIO(*)`(`π-exclude`(t.enabled)): List[Enumerator])(_.emit)

          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), *, t.emit))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), t.emit, *))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(1, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), λ(Symbol(par)), Some(nu @ "ν"), r, code)), sum) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code)), sum) =>
          val par = if λ.`type`.isDefined then id else arg

          code match
            case Some((Right(term), _)) =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(term :: Nil)))
            case _ =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          λ.`type` match
            case Some((tpe, Some(refined))) =>
              * :+= `* = *: * …`(arg, par, tpe, refined)
            case Some((tpe, _)) =>
              * :+= `* = *: *`(arg, par, tpe)
            case _ =>

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), λ(arg: Term), None, r, code)), sum) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), arg, None, r, code)), sum) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(it @ τ(r, code)), sum) =>

          code match
            case Some((Left(enums), _)) =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select("τ", "!"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))
              * = * ::: `ZStream.fromZIO(…)`(enums)
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select("τ", "!"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select("τ", "!"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit()

        case !(parallelism, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), λ(Symbol(par)), Some(nu @ "ν"), r, code)), sum) if parallelism < -1 =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `* <- +`(par, -parallelism,
                           Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "!"), "+"), nu), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)),
                           sum.emit())
            case Some((Right(term), _)) =>
              val expr = term
              * = `* <- +`(par, -parallelism,
                           Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "!"), "+"), nu), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)),
                           sum.emit())
            case _ =>
              * = `* <- +`(par, -parallelism,
                           Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "!"), "+"), nu), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                           sum.emit())

        case !(parallelism, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code)), sum) if parallelism < -1 =>
          val par = if λ.`type`.isDefined then id else arg

          λ.`type` match
            case Some((tpe, Some(refined))) =>
              * :+= `* = *: * …`(arg, par, tpe, refined)
            case Some((tpe, _)) =>
              * :+= `* = *: *`(arg, par, tpe)
            case _ =>

          code match
            case Some((Right(term), _)) =>
              * = `* <- +`(par, -parallelism,
                           Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "+"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(term :: Nil)),
                           * ::: sum.emit())
            case _ =>
              * = `* <- +`(par, -parallelism,
                           Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "+"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                           * ::: sum.emit())

        case !(parallelism, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), λ(arg: Term), None, r, code)), sum) if parallelism < -1 =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- +`(-parallelism,
                           Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "!"), "+"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)),
                           sum.emit())
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- +`(-parallelism,
                           Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "!"), "+"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)),
                           sum.emit())
            case _ =>
              * = `_ <- +`(-parallelism,
                           Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "!"), "+"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                           sum.emit())

        case !(parallelism, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), arg, None, r, code)), sum) if parallelism < -1 =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- +`(-parallelism,
                           Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "+"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)),
                           sum.emit())
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- +`(-parallelism,
                           Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "+"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)),
                           sum.emit())
            case _ =>
              * = `_ <- +`(-parallelism,
                           Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "+"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                           sum.emit())

        case !(parallelism, given Option[(Long, String)], Some(it @ τ(r, code)), sum) if parallelism < -1 =>

          code match
            case Some((Left(enums), _)) =>
              * = `_ <- +`(-parallelism,
                           Term.Apply(Term.Apply(Term.Select(Term.Select("τ", "!"), "+"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                           `ZStream.fromZIO(…)`(enums) ::: sum.emit())
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- +`(-parallelism,
                           Term.Apply(Term.Apply(Term.Apply(Term.Select("τ", "!"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)),
                           sum.emit())
            case _ =>
              * = `_ <- +`(-parallelism,
                           Term.Apply(Term.Apply(Term.Select("τ", "!"), Term.ArgClause(rate(r.get) :: pace(Nil))),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                           sum.emit())

        case !(-1, given Option[(Long, String)], Some(π @ π(_, λ @ λ(Symbol(arg)), Some(_), _, _)), sum) if λ.`type`.isDefined =>
          val par = id

          val υidυ = id

          val πʹ = {
            def idʹ: String = π.υidυ
            π.copy(name = λ.copy()(using None))(idʹ)
          }

          var `!.π⋯` = πʹ.emit :+ ^._1 :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(arg :: Nil)),
                                                              Term.ArgClause(^._2 :: Nil)))

          val `val` =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `val * = *: * …`(arg, par, tpe, refined) :: Nil
              case Some((tpe, _)) =>
                `val * = *: *`(arg, par, tpe) :: Nil
              case _ => Nil

          val `!⋯` =
            Term.Block(`val` :+
                       `List( *, … ).collectAllPar`(
                         sum.emit(),
                         `!.π⋯`
                       ))

          * = `* <- *`(υidυ -> `\\.succeed { def *(*: ()): String => ZStream[Any, Throwable, Unit] = { implicit ^ => … }; * }`(υidυ -> par, `!⋯`)) :: `!.π⋯`

        case !(-1, given Option[(Long, String)], Some(π @ π(_, λ(Symbol(par)), Some(_), _, _)), sum) =>
          val υidυ = id

          var `!.π⋯` = π.emit :+ ^._1 :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(par :: Nil)),
                                                             Term.ArgClause(^._2 :: Nil)))

          val `!⋯` =
            `List( *, … ).collectAllPar`(
              sum.emit(),
              `!.π⋯`
            )

          * = `* <- *`(υidυ -> `\\.succeed { def *(*: ()): String => ZStream[Any, Throwable, Unit] = { implicit ^ => … }; * }`(υidυ -> par, `!⋯`)) :: `!.π⋯`

        case !(-1, given Option[(Long, String)], Some(μ), sum) =>
          val υidυ = id

          var `!.μ⋯` = μ.emit :+ ^._1 :+ `_ <- *`(Term.Apply(\(υidυ), Term.ArgClause(^._2 :: Nil)))

          val `!⋯` =
            `List( *, … ).collectAllPar`(
              sum.emit(),
              `!.μ⋯`
            )

          * = `* <- *`(υidυ -> `\\.succeed { lazy val *: String => ZStream[Any, Throwable, Unit] = { implicit ^ => … }; * }`(υidυ, `!⋯`)) :: `!.μ⋯`

        case !(parallelism, given Option[(Long, String)], Some(π @ π(_, λ @ λ(Symbol(arg)), Some(_), _, _)), sum) if λ.`type`.isDefined =>
          val par = id
          val υidυ = id
          val scope = id
          val sem = id

          val πʹ = {
            def idʹ: String = π.υidυ
            π.copy(name = λ.copy()(using None))(idʹ)
          }

          var `!.π⋯` = ^._1 :: `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(scope :: Term.Select(arg, "get") :: Nil)),
                                                   Term.ArgClause(^._2 :: Nil)))

          `!.π⋯` = `* <- ….runLast; _ <- …`(arg, (πʹ.emit: Term) -> `!.π⋯`)

          `!.π⋯` = `_ <- *`(`ZStream.fromZIO(Scope.make)(…).provideLayer(*)`(`*.withPermitScoped`(sem) :: `!.π⋯`, scope))

          val `val` =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `val * = *: * …`(arg, par, tpe, refined) :: Nil
              case Some((tpe, _)) =>
                `val * = *: *`(arg, par, tpe) :: Nil
              case _ => Nil

          val `!⋯` =
            Term.Block(`val` :+
                       `List( *, … ).collectAllParZIO`(
                         sum.emit() :+ `*.close(Exit.unit)`(scope),
                         `!.π⋯`
                       ))

          * = `* <- Semaphore.make(…)`(sem, parallelism) ::
              `* <- *`(υidυ -> `\\.succeed { def *(*: Scope.Closeable, *: ()): String => ZIO[Any, Throwable, Unit] = { implicit ^ => … }; * }`(υidυ -> scope -> par, `!⋯`)) :: `!.π⋯`

        case !(parallelism, given Option[(Long, String)], Some(π @ π(_, λ(Symbol(par)), Some(_), _, _)), sum) =>
          val υidυ = id
          val scope = id
          val sem = id

          var `!.π⋯` = * :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(scope :: Term.Select(par, "get") :: Nil)),
                                                Term.ArgClause(^._2 :: Nil)))

          `!.π⋯` = `* <- ….runLast; _ <- …`(par, (π.emit: Term) -> `!.π⋯`)

          `!.π⋯` = `_ <- *`(`ZStream.fromZIO(Scope.make)(…).provideLayer(*)`(`*.withPermitScoped`(sem) :: `!.π⋯`, scope))

          val `!⋯` = `List( *, … ).collectAllParZIO`(
                       sum.emit() :+ `*.close(Exit.unit)`(scope),
                       `!.π⋯`
                     )

          * = `* <- Semaphore.make(…)`(sem, parallelism) ::
              `* <- *`(υidυ -> `\\.succeed { def *(*: Scope.Closeable, *: ()): String => ZIO[Any, Throwable, Unit] = { implicit ^ => … }; * }`(υidυ -> scope -> par, `!⋯`)) :: `!.π⋯`

        case !(parallelism, given Option[(Long, String)], Some(μ), sum) =>
          val par = id
          val υidυ = id
          val scope = id
          val sem = id

          var `!.μ⋯` = * :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(scope :: Nil)),
                                                Term.ArgClause(^._2 :: Nil)))

          val `μ.emit` = μ.emit match
            case (hd @ Enumerator.Generator(_, _)) :: tl =>
              hd.copy(pat = Pat.Var(par)) :: tl

          `!.μ⋯` = `* <- ….runLast; _ <- …`(par, (`μ.emit`: Term) -> `!.μ⋯`)

          `!.μ⋯` = `_ <- *`(`ZStream.fromZIO(Scope.make)(…).provideLayer(*)`(`*.withPermitScoped`(sem) :: `!.μ⋯`, scope))

          val `!⋯` = `List( *, … ).collectAllParZIO`(
                       sum.emit() :+ `*.close(Exit.unit)`(scope),
                       `!.μ⋯`
                     )

          * = `* <- Semaphore.make(…)`(sem, parallelism) ::
              `* <- *`(υidυ -> `\\.succeed { def *(*: Scope.Closeable): String => ZIO[Any, Throwable, Unit] = { implicit ^ => … }; * }`(υidυ -> scope, `!⋯`)) :: `!.μ⋯`

        case _ : ! => ??? // caught by 'parse'

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(_, variables, _sum, _, assignment) =>
          * = assignment
            .map(_.name -> _.name)
            .map(Pat.Var(_) -> _)
            .map(Enumerator.Val(_, _))
            .toList

          val n = assignment.size

          val sum = if (variables.size == n)
                    then
                      _sum
                    else
                      `+`(-1, ∥(-1, `.`(_sum, ν(variables.drop(n).map(_.name).toSeq*))))

          * = * ::: sum.emit

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, params*) =>
          val args = params.map(_.toTerm).toList

          * = ^._1 :: `_ <- *`(Term.Apply(Term.Apply(\(identifier), Term.ArgClause(args)),
                                          Term.ArgClause(^._2 :: Nil, Some(Mod.Using()))))

        ////////////////////////////////////////////////////////// invocation //

      *


  final class Main:

    def apply(prog: List[Bind]): List[Stat] =
      val id = new helper.υidυ

      val `^-υidυ` = id()

      given (Enumerator.Generator, Term.Name) =
        (`* <- ZStream.fromZIO(*)`(`^-υidυ` -> \("π-uuid")), \(`^-υidυ`))

      ( prog.tail.head match
          case (`(*)`(_, λ(parallelism: Lit.Int)), _) =>
            Defn.Val(Nil, Pat.Var("π-parallelism") :: Nil, None, parallelism)
      ) ::
      prog
        .drop(2)
        .map(_ -> _.emit(using id()))
        .map(_.swap)
        .map(defn(_)(_))
