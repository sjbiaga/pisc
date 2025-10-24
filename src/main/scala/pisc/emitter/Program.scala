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
            `* <- Semaphore[IO](1)`(sem.get),
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


        case π(λ(Symbol(ch)), arg, nu @ (None | Some("ν")), code) =>
          nu match
            case None =>
            case _ =>
              val λ(Symbol(par)) = arg
              * = ν(par).emit()

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(arg.toTerm::Nil)),
                               Term.ArgClause(expr::Nil)
                             ))
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- IO { * }`(term))
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(arg.toTerm::Nil)),
                               Term.ArgClause(expr::Nil)
                             ))
            case _ =>
              * :+= `_ <- *`(Term.Apply(\(ch), Term.ArgClause(arg.toTerm::Nil)))

        case π(λ(Symbol(ch)), λ(params: List[`λ`]), Some(cons), code) =>
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

        case π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), code) =>
          val par = if λ.`type`.isDefined then id else arg

          code match
            case Some((Right(term), _)) =>
              * = `* <- *`(par -> Term.Apply(
                                    Term.Apply(\(ch), Term.ArgClause(Nil)),
                                    Term.ArgClause(term::Nil)
                           ))
            case _ =>
              * = `* <- *`(par -> Term.Apply(\(ch), Term.ArgClause(Nil)))

          λ.`type` match
            case Some((tpe, Some(refined))) =>
              * :+= `* = *: * …`(arg, par, tpe, refined)
            case Some((tpe, _)) =>
              * :+= `* = *: *`(arg, par, tpe)
            case _ =>

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

        case !(pace, Some(π @ π(_, λ(Symbol(par)), Some("ν"), _)), sum) =>
          val υidυ = id
          val υidυ2 = id

          val `π.emit()` = π.emit() match
            case hd :: (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
              hd :: it.copy(pat = Pat.Var(υidυ2)) :: tl

          val `!.π⋯` = `π.emit()` :+ `_ <- *` { Term.If(Term.ApplyInfix(\(υidυ2), \("eq"),
                                                                        Type.ArgClause(Nil),
                                                                        Term.ArgClause(\("None") :: Nil)),
                                                        `IO.cede`,
                                                        Term.Apply(\(υidυ),
                                                                   Term.ArgClause(\(par) :: Nil)),
                                                        Nil)
                                              }

          val `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.π⋯`).getOrElse(`!.π⋯`)

          val it = `List( *, … ).parSequence`(
                     sum.emit(),
                     `!⋯`
                   )

          * = `* <- *`(υidυ -> `IO { def *(*: ()): IO[Any] = …; * }`(υidυ -> par, it)) :: `!.π⋯`

        case !(pace, Some(π @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), _)), sum) =>
          val par = if λ.`type`.isDefined then id else arg

          val υidυ = id

          val πʹ = π.copy(name = λ.copy()(using None))
          val `!.π⋯` = πʹ.emit() :+ `_ <- *`(Term.Apply(\(υidυ),
                                                            Term.ArgClause(\(arg) :: Nil)))

          val `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.π⋯`).getOrElse(`!.π⋯`)

          val `val` =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `val * = *: * …`(arg, par, tpe, refined) :: Nil
              case Some((tpe, _)) =>
                `val * = *: *`(arg, par, tpe) :: Nil
              case _ => Nil

          val it = Term.If(Term.ApplyUnary("!", par),
                           `IO.cede`,
                           Term.Block(`val` :::
                             `List( *, … ).parSequence`(
                               sum.emit(),
                               `!⋯`
                             ) :: Nil
                           )
                   )

          * = `* <- *`(υidυ -> `IO { def *(*: ()): IO[Any] = …; * }`(υidυ -> par, it)) :: `!.π⋯`

        case !(pace, Some(μ), sum) =>
          val υidυ = id
          val υidυ2 = id

          val `μ.emit()` = μ.emit() match
            case (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
              it.copy(pat = Pat.Var(υidυ2)) :: tl

          val `!.μ⋯` = `μ.emit()` :+ `_ <- *` { Term.If(Term.ApplyInfix(\(υidυ2), \("eq"),
                                                                        Type.ArgClause(Nil),
                                                                        Term.ArgClause(\("None") :: Nil)),
                                                        `IO.cede`,
                                                        υidυ,
                                                        Nil)
                                              }

          val `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!.μ⋯`).getOrElse(`!.μ⋯`)

          val it = `List( *, … ).parSequence`(
                     sum.emit(),
                     `!⋯`
                   )

          * = `* <- *`(υidυ -> `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `!.μ⋯`

        case !(pace, _, sum) =>
          val υidυ = id

          val `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `_ <- *`(υidυ)).getOrElse(`_ <- *`(υidυ) :: Nil)

          val it = `List( *, … ).parSequence`(
                     sum.emit(),
                     `_ <- IO.unit` :: `!⋯`
                   )

          * = `* <- *`(υidυ, `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `_ <- *`(υidυ)

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
