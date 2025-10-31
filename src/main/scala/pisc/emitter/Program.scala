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

import scala.meta.*
import dialects.Scala3

import parser.Calculus.*
import Meta.*


object Program:

  extension (node: Pre | AST)

    def emit(using id: => String)
            (implicit semaphore: Option[String] = None): List[Enumerator] =
      var * = List[Enumerator]()

      node match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>
          val ** = `_ <- IO.unit`

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* :::= **)

        case +(operand) =>
          * = operand.emit

        case it: + =>
          implicit val sem = Some(id)

          val ios = it.choices.foldRight(List[Term]())(_.emit :: _)

          val ** = List(
            `* <- Semaphore[IO](…)`(sem.get),
            `_ <- *`(`List( *, … ).parSequence`(ios*))
          )

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* :::= **)

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(operand) =>
          * = operand.emit

        case it: ∥ =>
          val ios = it.components.foldRight(List[Term]())(_.emit() :: _)

          val ** = `_ <- *`(`List( *, … ).parSequence`(ios*))

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* :::= **)

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case it @ `.`(?:(_, _, None)) if semaphore.nonEmpty =>
          def cases(sum: +): Term =
            sum match
              case +(∥(`.`(?:(((lhs, rhs), mismatch), t, None)))) =>
                if mismatch
                then
                  `if * then … else …`(====(lhs, rhs), Nil, cases(t))
                else
                  `if * then … else …`(====(lhs, rhs), cases(t), Nil)
              case _ =>
                `_ <- *.tryAcquire.ifM`(semaphore.get, sum.emit())

          * = `_ <- *`(cases(`+`(∥(it))))

        case `.`(end, it*) =>
          val ** = (it :+ end).foldLeft(*)(_ ::: _.emit())

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* :::= **)

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> "ν") }.toList

        case τ(Some((Left(enums), _))) =>
          * :+= `_ <- *`("τ")
          * :::= enums

        case τ(Some((Right(term), _))) =>
          * :+= `_ <- *`("τ")
          * :+= `_ <- IO { * }`(term)

        case τ(_) =>
          * = `_ <- *`("τ")


        case π(λ(Symbol(ch)), nu @ (None | Some("ν")), code, params*) =>
          nu match
            case None =>
            case _ =>
              * = ν(params.filter(_.isSymbol).map(_.asSymbol.name)*).emit()

          val args = params.map(_.toTerm).toList

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(args)),
                               Term.ArgClause(expr::Nil)
                             ))
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- IO { * }`(term))
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(args)),
                               Term.ArgClause(expr::Nil)
                             ))
            case _ =>
              * :+= `_ <- *`(Term.Apply(\(ch), Term.ArgClause(args)))

        case π(λ(Symbol(ch)), Some(cons), code, params*) if cons.nonEmpty =>
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

        case π(λ(Symbol(ch)), Some(_), code, params*) =>
          val args = params.map {
            case λ @ λ(Symbol(_)) if λ.`type`.isDefined => id
            case λ(Symbol(par)) => par
          }

          code match
            case Some((Right(term), _)) =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*),
                                       Term.Apply(Term.Apply(\(ch), Term.ArgClause(Nil)),
                                                  Term.ArgClause(term::Nil)
                                       ))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(\(ch), Term.ArgClause(Nil)))

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

        case _: π => ??? // caught by parser

        ////////////////////////////////////////////// restriction | prefixes //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((lhs, rhs), mismatch), t, f) =>
          * = f.map(_.emit()).getOrElse(Nil)

          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), *, t.emit()))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), t.emit(), *))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(parallelism, pace, Some(π @ π(λ(Symbol(ch)), Some("ν"), _, params*)), sum) =>
          val args = params.filter(_.isSymbol).map(_.asSymbol.name)

          val υidυ = id
          val υidυ2 = id

          val `π.emit()` = π.emit() match
            case ls =>
              ls.drop(args.size) match
                case (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
                  ls.take(args.size) ::: it.copy(pat = Pat.Var(υidυ2)) :: tl

          var `!.π⋯` = `π.emit()` :+ `_ <- *` { Term.If(Term.ApplyInfix(\(υidυ2), \("eq"),
                                                                        Type.ArgClause(Nil),
                                                                        Term.ArgClause(\("None") :: Nil)),
                                                        `IO.cede`,
                                                        Term.Apply(\(υidυ),
                                                                   Term.ArgClause(args.map(\(_)).toList)),
                                                        Nil)
                                              }

          var `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.π⋯`).getOrElse(`!.π⋯`)

          val sem = if parallelism < 0 then null else id

          val it =
            if parallelism < 0
            then
              `List( *, … ).parSequence`(
                sum.emit(),
                `!⋯`
              )
            else
              `!.π⋯` = `_ <- *.acquire`(sem) :: `!.π⋯`
              `!⋯` = `_ <- *.acquire`(sem) :: `!⋯`
              `List( *, … ).parSequence`(
                sum.emit() :+ `_ <- *.release`(sem),
                `!⋯`
              )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `IO { def *(*: ()): IO[Any] = …; * }`(υidυ, it, args*)) :: `!.π⋯`
          else
            * = `* <- Semaphore[IO](…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `IO { def *(*: ()): IO[Any] = …; * }`(υidυ, it, args*)) :: `!.π⋯`

        case !(parallelism, pace, Some(π @ π(λ(Symbol(ch)), Some(_), code, params*)), sum) =>
          val args = params.map {
            case λ @ λ(Symbol(_)) if λ.`type`.isDefined => id
            case λ(Symbol(par)) => par
          }

          val υidυ = id

          val πʹ = Pre.π(λ(Symbol(ch)), Some(""), code, params.map(_.copy()(using None))*)

          var `!.π⋯` = πʹ.emit() :+ `_ <- *`(Term.Apply(\(υidυ),
                                                        Term.ArgClause(params.map(_.asSymbol.name).map(\(_)).toList)))

          var `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.π⋯`).getOrElse(`!.π⋯`)

          val `val` = params.zipWithIndex.flatMap {
            case (λ @ λ(Symbol(arg)), i) if λ.`type`.isDefined =>
              val par = args(i)
              λ.`type`.get match
                case (tpe, Some(refined)) =>
                  Some(`val * = *: * …`(arg, par, tpe, refined))
                case (tpe, _) =>
                  Some(`val * = *: *`(arg, par, tpe))
            case _ => None
          }.toList

          val sem = if parallelism < 0 then null else id

          val it =
            if parallelism < 0
            then
              Term.If(Term.ApplyUnary("!", args.head),
                      `IO.cede`,
                      Term.Block(`val` :+
                                 `List( *, … ).parSequence`(
                                   sum.emit(),
                                   `!⋯`
                                 ))
                     )
            else
              `!.π⋯` = `_ <- *.acquire`(sem) :: `!.π⋯`
              `!⋯` = `_ <- *.acquire`(sem) :: `!⋯`
              Term.If(Term.ApplyUnary("!", args.head),
                      `IO.cede`,
                      Term.Block(`val` :+
                                 `List( *, … ).parSequence`(
                                   sum.emit() :+ `_ <- *.release`(sem),
                                   `!⋯`
                                 ))
                     )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `IO { def *(*: ()): IO[Any] = …; * }`(υidυ, it, args*)) :: `!.π⋯`
          else
            * = `* <- Semaphore[IO](…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `IO { def *(*: ()): IO[Any] = …; * }`(υidυ, it, args*)) :: `!.π⋯`

        case !(parallelism, pace, Some(μ), sum) =>
          val υidυ = id
          val υidυ2 = id

          val `μ.emit()` = μ.emit() match
            case (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
              it.copy(pat = Pat.Var(υidυ2)) :: tl

          var `!.μ⋯` = `μ.emit()` :+ `_ <- *` { Term.If(Term.ApplyInfix(\(υidυ2), \("eq"),
                                                                        Type.ArgClause(Nil),
                                                                        Term.ArgClause(\("None") :: Nil)),
                                                        `IO.cede`,
                                                        υidυ,
                                                        Nil)
                                              }

          var `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.μ⋯`).getOrElse(`!.μ⋯`)

          val sem = if parallelism < 0 then null else id

          val it =
            if parallelism < 0
            then
              `List( *, … ).parSequence`(
                sum.emit(),
                `!⋯`
              )
            else
              `!.μ⋯` = `_ <- *.acquire`(sem) :: `!.μ⋯`
              `!⋯` = `_ <- *.acquire`(sem) :: `!⋯`
              `List( *, … ).parSequence`(
                sum.emit() :+ `_ <- *.release`(sem),
                `!⋯`
              )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `!.μ⋯`
          else
            * = `* <- Semaphore[IO](…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `!.μ⋯`

        case !(parallelism, pace, _, sum) =>
          val υidυ = id

          var `!.⋯` = `_ <- *`(υidυ) :: Nil

          var `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.⋯`).getOrElse(`!.⋯`)

          val sem = if parallelism < 0 then null else id

          val it =
            if parallelism < 0
            then
              `List( *, … ).parSequence`(
                sum.emit(),
                `_ <- IO.unit` :: `!⋯`
              )
            else
              `!.⋯` = `_ <- *.acquire`(sem) :: `!.⋯`
              `!⋯` = `_ <- *.acquire`(sem) :: `!⋯`
              `List( *, … ).parSequence`(
                sum.emit() :+ `_ <- *.release`(sem),
                `_ <- IO.unit` :: `!⋯`
              )

          if parallelism < 0
          then
            * = `* <- *`(υidυ, `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `!.⋯`
          else
            * = `* <- Semaphore[IO](…)`(sem, parallelism) ::
                `* <- *`(υidυ, `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `!.⋯`

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
                      `+`(∥(`.`(_sum, ν(variables.drop(n).map(_.name).toSeq*))))

          * = ** ::: sum.emit()

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, qual, params*) =>
          val args = params.map(_.toTerm).toList

          val term = qual match
            case h :: t => (t.map(\(_)) :+ \("π") :+ \(identifier)).foldLeft(h: Term)(Term.Select(_, _))
            case _ => \(identifier)

          * :+= `_ <- *`(Term.Apply(term, Term.ArgClause(args)))

        ////////////////////////////////////////////////////////// invocation //

      *


  final class Main:

    def apply(prog: List[Bind]): List[String] =
      val id = new helper.υidυ
      prog
        .map(_ -> _.emit(using id()))
        .map(_.swap)
        .map(defn(_)(_).toString)
