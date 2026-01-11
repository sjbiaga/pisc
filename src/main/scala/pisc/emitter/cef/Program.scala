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
package cef

import scala.meta.*
import dialects.Scala3

import parser.StochasticPi.Actions
import parser.Calculus.*
import parser.μ
import cef.Meta.*


object Program:

  extension (self: Pre)(using id: => String)

    def emit(* : List[Enumerator]): Term =

      implicit val υidυ = id

      self match

        // PREFIXES ////////////////////////////////////////////////////////////

        case it @ τ(r, Some((Left(enums), _))) =>
          `*.flatMap { null else … }`(Term.Apply(Term.Apply(\("τ"), Term.ArgClause(rate(r.get)::Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                                      enums ::: *)

        case it @ τ(r, Some((Right(term), _))) =>
          `*.flatMap { null else … }`(Term.Apply(Term.Apply(\("τ"), Term.ArgClause(rate(r.get)::Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                                      `_ <- IO { * }`(term) :: *)

        case it @ τ(r, _) =>
          `*.flatMap { null else … }`(Term.Apply(Term.Apply(\("τ"), Term.ArgClause(rate(r.get)::Nil)),
                                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                                      *)


        case it @ π(λ(Symbol(ch)), arg, None, r, code) =>
          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              `*.flatMap { null else … }`(Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                                                Term.ArgClause(Lit.String(it.υidυ)::Nil)), Term.ArgClause(expr::Nil)),
                                          *)
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- IO { * }`(term))
              `*.flatMap { null else … }`(Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                                                Term.ArgClause(Lit.String(it.υidυ)::Nil)), Term.ArgClause(expr::Nil)),
                                          *)
            case _ =>
              `*.flatMap { null else … }`(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                                     Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                                          *)

        case it @ π(λ(Symbol(ch)), λ(Symbol(par)), Some("ν"), _, _) =>
          val parʹ = if ch == par then id else par
          val ** = if ch == par then `* <- IO.pure(*)`(par -> parʹ) else `_ <- \\.unit`
          `for * yield ()`(
            `* <- *`(parʹ -> "ν"),
            `_ <- *`(it.copy(name = λ(Symbol(parʹ)), polarity = None)(it.υidυ).emit(** :: *))
          )

        case it @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code) =>
          val par = if λ.`type`.isDefined then id else arg

          val ** =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `* = *: * …`(arg, par, tpe, refined) :: *
              case Some((tpe, _)) =>
                `* = *: *`(arg, par, tpe) :: *
              case _ =>
                *

          code match
            case Some((Right(term), _)) =>
              `*.flatMap { null else … }`(Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get)::Nil)),
                                                                Term.ArgClause(Lit.String(it.υidυ)::Nil)), Term.ArgClause(term::Nil)),
                                          **)(par)

            case _ =>
              `*.flatMap { null else … }`(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get)::Nil)),
                                                     Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                                          **)(par)

        case _ => ??? // caught by parser

        //////////////////////////////////////////////////////////// prefixes //


  extension (self: AST)(using id: => String, ^ : (Enumerator.Generator, Term.Name))

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
                  `if * then … else …`(====(lhs, rhs), `_ <- *`(`π-exclude`(t.enabled)), cases(t))
                else
                  `if * then … else …`(====(lhs, rhs), cases(t), `_ <- *`(`π-exclude`(t.enabled)))
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
          val ios = it.choices.foldRight(List[Term]())(_.emitʹ :: _)

          * = `_ <- *`(`List( *, … ).parSequence`(ios*))

        case it: + =>
          val ios = it.choices.foldRight(List[Term]())(_.emit :: _)

          * = `_ <- *`(`List( *, … ).parSequence`(ios*))

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(_, operand) =>
          * = operand.emit

        case it: ∥ =>
          val ios = it.components.foldRight(List[Term]())(_.emit :: _)

          * = `_ <- *`(`List( *, … ).parSequence`(ios*))

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, ps*) =>

          val υidυ = Actions(ps*).headOption

          * = ps.foldRight(end.emit) {

            case (ν(names*), ios) =>
              names.map { it => `* <- *`(it -> "ν") }.toList ::: ios

            case (π(λ(Symbol(ch)), λ(params: List[`λ`]), Some(cons), _, code), ios) =>
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

              * ::: ios

            case (it: μ, ios) if υidυ.get eq it.υidυ =>
              `_ <- *`(it.emit(ios))

            case (it, ios) =>
              import ce.Program.{ emit => cemit }
              it.cemit ::: ios

          }

        //////////////////////////////////////////////////////////// sequence //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((lhs, rhs), mismatch), t, f) =>
          * = f.fold(`_ <- *`(`π-exclude`(t.enabled)): List[Enumerator])(_.emit)

          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), *, t.emit))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), t.emit, *))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(parallelism, pace, Some(π @ π(_, λ(Symbol(par)), Some("ν"), _, _)), sum) =>
          val υidυ = id

          var `!.π⋯` = ^._1 :: `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(par :: Nil)),
                                                   Term.ArgClause(^._2 :: Nil)))

          `!.π⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.π⋯`).getOrElse(`!.π⋯`)

          `!.π⋯` = `_ <- *`(π.emit(`!.π⋯`)) :: Nil

          val sem = if parallelism < 0 then null else id

          val `!⋯` =
            if parallelism < 0
            then
              `List( *, … ).parSequence`(
                sum.emit,
                `!.π⋯`
              )
            else
              `!.π⋯` = `_ <- *.acquire`(sem) :: `!.π⋯`
              `List( *, … ).parSequence`(
                sum.emit :+ `_ <- *.release`(sem),
                `!.π⋯`
              )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `IO { def *(*: ()): String => IO[Any] = { implicit ^ => … }; * }`(υidυ -> par, `!⋯`)) :: `!.π⋯`
          else
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `IO { def *(*: ()): String => IO[Any] = { implicit ^ => … }; * }`(υidυ -> par, `!⋯`)) :: `!.π⋯`

        case !(parallelism, pace, Some(π @ π(_, λ @ λ(Symbol(arg)), Some(_), _, _)), sum) =>
          val par = if λ.`type`.isDefined then id else arg

          val υidυ = id

          val πʹ = {
            def idʹ: String = π.υidυ
            π.copy(name = λ.copy()(using None))(idʹ)
          }

          var `!.π⋯` = ^._1 :: `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(arg :: Nil)),
                                                   Term.ArgClause(^._2 :: Nil)))

          `!.π⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.π⋯`).getOrElse(`!.π⋯`)

          `!.π⋯` = `_ <- *`(πʹ.emit(`!.π⋯`)) :: Nil

          val `val` =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `val * = *: * …`(arg, par, tpe, refined) :: Nil
              case Some((tpe, _)) =>
                `val * = *: *`(arg, par, tpe) :: Nil
              case _ => Nil

          val sem = if parallelism < 0 then null else id

          val `!⋯` =
            if parallelism < 0
            then
              Term.Block(`val` :+
                         `List( *, … ).parSequence`(
                           sum.emit,
                           `!.π⋯`
                         ))
            else
              `!.π⋯` = `_ <- *.acquire`(sem) :: `!.π⋯`
              Term.Block(`val` :+
                         `List( *, … ).parSequence`(
                           sum.emit :+ `_ <- *.release`(sem),
                           `!.π⋯`
                         ))

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `IO { def *(*: ()): String => IO[Any] = { implicit ^ => … }; * }`(υidυ -> par, `!⋯`)) :: `!.π⋯`
          else
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `IO { def *(*: ()): String => IO[Any] = { implicit ^ => … }; * }`(υidυ -> par, `!⋯`)) :: `!.π⋯`

        case !(parallelism, pace, Some(μ), sum) =>
          val υidυ = id

          var `!.μ⋯` = ^._1 :: `_ <- *`(Term.Apply(\(υidυ), Term.ArgClause(^._2 :: Nil)))

          `!.μ⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.μ⋯`).getOrElse(`!.μ⋯`)

          `!.μ⋯` = `_ <- *`(μ.emit(`!.μ⋯`)) :: Nil

          val sem = if parallelism < 0 then null else id

          val `!⋯` =
            if parallelism < 0
            then
              `List( *, … ).parSequence`(
                sum.emit,
                `!.μ⋯`
              )
            else
              `!.μ⋯` = `_ <- *.acquire`(sem) :: `!.μ⋯`
              `List( *, … ).parSequence`(
                sum.emit :+ `_ <- *.release`(sem),
                `!.μ⋯`
              )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `IO { lazy val *: String => IO[Any] = { implicit ^ => … }; * }`(υidυ, `!⋯`)) :: `!.μ⋯`
          else
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `IO { lazy val *: String => IO[Any] = { implicit ^ => … }; * }`(υidυ, `!⋯`)) :: `!.μ⋯`

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
        (`* <- *`(`^-υidυ` -> \("π-uuid")), \(`^-υidυ`))

      ( prog.tail.head match
          case (`(*)`(_, λ(parallelism: Lit.Int)), _) =>
            Defn.Val(Nil, Pat.Var("π-parallelism") :: Nil, None, parallelism)
      ) ::
      prog
        .drop(2)
        .map(_ -> _.emit(using id()))
        .map(_.swap)
        .map(defn(_)(_))
