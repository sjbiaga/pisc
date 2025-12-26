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
package fs2

import scala.meta.*
import dialects.Scala3

import parser.Calculus.*
import fs2.Meta.*


object Program:

  extension (self: Pre | AST)(using id: => String)

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
                  `if * then … else …`(====(lhs, rhs), `_ <- Stream.eval(*)`(`π-exclude[F]`(t.enabled)), cases(t))
                else
                  `if * then … else …`(====(lhs, rhs), cases(t), `_ <- Stream.eval(*)`(`π-exclude[F]`(t.enabled)))
              case _ =>
                sum.emit

          `_ <- *`(cases(`+`(-1, ∥(-1, it))))

        case _ => ???

    def emit: List[Enumerator] =

      var * = List[Enumerator]()

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>

        case +(_, operand) =>
          * = operand.emit

        case it: + if it.scaling == -1 && it.choices.forall { case ∥(-1, `.`(?:(_, _, None))) => true case _ => false } =>

          val sfs = it.choices.foldRight(List[Term]())(_.emitʹ :: _)

          * = `_ <- *`(`List( *, … ).parSequence`(sfs*))

        case it: + =>
          val sfs = it.choices.foldRight(List[Term]())(_.emit :: _)

          * = `_ <- *`(`List( *, … ).parSequence`(sfs*))

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(_, operand) =>
          * = operand.emit

        case it: ∥ =>
          val sfs = it.components.foldRight(List[Term]())(_.emit :: _)

          * = `_ <- *`(`List( *, … ).parSequence`(sfs*))

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, it*) =>
          * = (it :+ end).foldLeft(*)(_ ::: _.emit)

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> `*[F]`("ν")) }.toList

        case it @ τ(r, Some((Left(enums), _))) =>
          * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(rate(r.get) :: Nil)),
                                  Term.ArgClause(Lit.String(it.υidυ) :: Nil)))
          * = * ::: `Stream.eval(…)`(enums)

        case it @ τ(r, Some((Right(term), _))) =>
          * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(rate(r.get) :: Nil)),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                  Term.ArgClause(term :: Nil)))

        case it @ τ(r, _) =>
          * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(rate(r.get) :: Nil)),
                                  Term.ArgClause(Lit.String(it.υidυ) :: Nil)))


        case it @ π(λ(Symbol(ch)), λ(Symbol(par)), Some(nu @ "ν"), r, code) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, nu), Term.ArgClause(rate(r.get) :: Nil)),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, nu), Term.ArgClause(rate(r.get) :: Nil)),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Select(ch, nu), Term.ArgClause(rate(r.get) :: Nil)),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

        case it @ π(λ(Symbol(ch)), λ(arg: Term), None, r, code) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "*"), Term.ArgClause(rate(r.get) :: arg :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "*"), Term.ArgClause(rate(r.get) :: arg :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "*"), Term.ArgClause(rate(r.get) :: arg :: Nil)),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

        case it @ π(λ(Symbol(ch)), arg, None, r, code) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

        case it @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code) =>

          val par = if λ.`type`.isDefined then id else arg

          code match
            case Some((Right(term), _)) =>
              val expr = term
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: Nil)),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: Nil)),
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
              * :+= `_ <- Stream.eval(*)`(term)
            case _ =>

        case _: π => ??? // caught by parser

        ////////////////////////////////////////////// restriction | prefixes //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((lhs, rhs), mismatch), t, f) =>
          * = f.fold(`_ <- Stream.eval(*)`(`π-exclude[F]`(t.enabled)): List[Enumerator])(_.emit)

          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), *, t.emit))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), t.emit, *))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(_, Some((time, unit)), Some(it @ π(λ(Symbol(ch)), λ(Symbol(par)), Some(nu @ "ν"), r, code)), sum) =>
          val pace = Term.Select(Lit.Long(time), unit)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(rate(r.get) :: pace :: Nil)),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(rate(r.get) :: pace :: Nil)),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(rate(r.get) :: pace :: Nil)),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit

        case !(_, None, Some(it @ π(λ(Symbol(ch)), λ(Symbol(par)), Some(nu @ "ν"), r, code)), sum) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(rate(r.get) :: Nil)),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(rate(r.get) :: Nil)),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(rate(r.get) :: Nil)),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit

        case !(_, Some((time, unit)), Some(it @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code)), sum) =>
          val pace = Term.Select(Lit.Long(time), unit)

          val par = if λ.`type`.isDefined then id else arg

          code match
            case Some((Right(term), _)) =>
              val expr = term
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: pace :: Nil)),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: pace :: Nil)),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          λ.`type` match
            case Some((tpe, Some(refined))) =>
              * :+= `* = *: * …`(arg, par, tpe, refined)
            case Some((tpe, _)) =>
              * :+= `* = *: *`(arg, par, tpe)
            case _ =>

          * = * ::: sum.emit

        case !(_, None, Some(it @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code)), sum) =>
          val par = if λ.`type`.isDefined then id else arg

          code match
            case Some((Right(term), _)) =>
              val expr = term
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: Nil)),
                                                        Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                             Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `* <- *`(par -> Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: Nil)),
                                             Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          λ.`type` match
            case Some((tpe, Some(refined))) =>
              * :+= `* = *: * …`(arg, par, tpe, refined)
            case Some((tpe, _)) =>
              * :+= `* = *: *`(arg, par, tpe)
            case _ =>

          * = * ::: sum.emit

        case !(_, Some((time, unit)), Some(it @ π(λ(Symbol(ch)), λ(arg: Term), None, r, code)), sum) =>
          val pace = Term.Select(Lit.Long(time), unit)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), Term.ArgClause(rate(r.get) :: pace :: arg :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), Term.ArgClause(rate(r.get) :: pace :: arg :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), Term.ArgClause(rate(r.get) :: pace :: arg :: Nil)),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit

        case !(_, None, Some(it @ π(λ(Symbol(ch)), λ(arg: Term), None, r, code)), sum) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), Term.ArgClause(rate(r.get) :: arg :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), Term.ArgClause(rate(r.get) :: arg :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), Term.ArgClause(rate(r.get) :: arg :: Nil)),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit

        case !(_, Some((time, unit)), Some(it @ π(λ(Symbol(ch)), arg, None, r, code)), sum) =>
          val pace = Term.Select(Lit.Long(time), unit)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: pace :: arg.toTerm :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: pace :: arg.toTerm :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: pace :: arg.toTerm :: Nil)),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit

        case !(_, None, Some(it @ π(λ(Symbol(ch)), arg, None, r, code)), sum) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit

        case !(_, Some((time, unit)), Some(it @ τ(r, code)), sum) =>
          val pace = Term.Select(Lit.Long(time), unit)

          code match
            case Some((Left(enums), _)) =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(rate(r.get) :: pace :: Nil)),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))
              * = * ::: `Stream.eval(…)`(enums)
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(rate(r.get) :: pace :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(rate(r.get) :: pace :: Nil)),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit

        case !(_, None, Some(it @ τ(r, code)), sum) =>

          code match
            case Some((Left(enums), _)) =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(rate(r.get) :: Nil)),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))
              * = * ::: `Stream.eval(…)`(enums)
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(rate(r.get) :: Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ) :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(rate(r.get) :: Nil)),
                                      Term.ArgClause(Lit.String(it.υidυ) :: Nil)))

          * = * ::: sum.emit

        case _ : ! => ??? // caught by 'parse'

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(_, variables, _sum, _, assignment) =>
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
                      `+`(-1, ∥(-1, `.`(_sum, ν(variables.drop(n).map(_.name).toSeq*))))

          * = ** ::: sum.emit

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, params*) =>
          val args = params.map(_.toTerm).toList

          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(identifier), Term.ArgClause(args)),
                         Term.ArgClause(\("π-uuid")::Nil, Some(Mod.Using()))))

        ////////////////////////////////////////////////////////// invocation //

      *



  final class Main:
    implicit private def `*[F]`(* : List[Enumerator]): Term =
      if *.nonEmpty then `for *[F] yield ()`(* *)
      else \(`_ <- \\.unit`)

    def apply(prog: List[Bind]): List[Stat] =
      val id = new helper.υidυ
      prog
        .drop(2)
        .map(_ -> _.emit(using id()))
        .map(_.swap)
        .map(defn(_)(_))
