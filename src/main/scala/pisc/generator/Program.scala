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
package generator

import scala.meta.*
import dialects.Scala3

import parser.Calculus.*
import Meta.*


object Program:

  extension (node: Pre | AST)

    def generate(using id: => String)
                (implicit semaphore: Option[String] = None): List[Enumerator] =
      var * = List[Enumerator]()

      node match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅(_) =>
          val ** = `_ <- IO.unit`

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* ++= **)

        case +(operand) =>
          * = operand.generate

        case it: + =>
          implicit val sem = Some(id)

          val ios = it.choices.foldRight(List[Term]())(_.generate :: _)

          val ** = List(
            `* <- Semaphore[IO](1)`(sem.get),
            `_ <- *`(`NonEmptyList( *, … ).parSequence`(ios*))
          )

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* ++= **)

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(operand) =>
          * = operand.generate

        case it: ∥ =>
          val ios = it.components.foldRight(List[Term]())(_.generate() :: _)

          val ** = `_ <- *`(`NonEmptyList( *, … ).parSequence`(ios*))

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* ++= **)

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
                `_ <- *.tryAcquire.ifM`(semaphore.get, sum.generate())

          * = `_ <- *`(cases(`+`(∥(it))))

        case `.`(end, it*) =>
          val ** = (it :+ end).foldLeft(*)(_ ++ _.generate())

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* ++= **)

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { (c, n) => `* <- *`(n -> Term.Apply(\("ν"), Term.ArgClause(Lit.Int(c) :: Nil, None))) }.toList

        case τ(Some((Left(enums), _))) =>
          * :+= `_ <- *`("τ")
          * ++= enums

        case τ(Some((Right(term), _))) =>
          * :+= `_ <- *`("τ")
          * :+= `_ <- IO { * }`(term)

        case τ(_) =>
          * = `_ <- *`("τ")


        case π(λ(Symbol(ch)), false, Some((Left(enums), _)), params*) =>
          val code = `for * yield ()`(enums*)
          val args = params.map(_.toTerm).toList

          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(args, None)),
                         Term.ArgClause(code::Nil, None)
                       ))

        case π(λ(Symbol(ch)), false, Some((Right(term), _)), params*) =>
          val code = `for * yield ()`(`_ <- IO { * }`(term))
          val args = params.map(_.toTerm).toList

          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(args, None)),
                         Term.ArgClause(code::Nil, None)
                       ))

        case π(λ(Symbol(ch)), false, _, params*) =>
          val args = params.map(_.toTerm).toList

          * = `_ <- *`(Term.Apply(\(ch), Term.ArgClause(args, None)))

        case π(_, true, Some((Left(_), _)), _*) => ??? // Scalameta Enumerator - caught by parser

        case π(λ(Symbol(ch)), true, Some((Right(code), _)), params*) =>
          val args = params.map {
            case λ(Symbol(name)) => name
          }
          * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(
                                                           Term.Apply(\(ch), Term.ArgClause(Nil, None)),
                                                           Term.ArgClause(code::Nil, None)
                                   ))

        case π(λ(Symbol(ch)), true, _, params*) =>
          val args = params.map {
            case λ(Symbol(name)) => name
          }
          * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(\(ch), Term.ArgClause(Nil, None)))

        case _: π => ??? // caught by parser

        ////////////////////////////////////////////// restriction | prefixes //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((lhs, rhs), mismatch), t, f) =>
          * = f.map(_.generate()).getOrElse(Nil)

          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), *, t.generate()))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), t.generate(), *))

        ////////////////////////// (mis)match | if then else | elvis operator //


        ////// REPLICATION /////////////////////////////////////////////////////

        case `!`(Some(π @ π(_, true, _, params*)), sum) =>
          val υidυ = id

          val args = params.map(_.asSymbol.name)

          val `!.π⋯` = π.generate() :+ `_ <- *`(Term.Apply(\(υidυ),
                                                           Term.ArgClause(args.map(\(_)).toList, None)))

          val it = Term.If(Term.ApplyUnary("!", args.head),
                           `IO.cede`,
                           `NonEmptyList( *, … ).parSequence`(
                             sum.generate(),
                             `!.π⋯`
                           )
                   )

          * = `* <- *`(υidυ -> `IO { def *(*: ()): IO[Any] = …; * }`(υidυ, it, args*)) :: `!.π⋯`

        case !(Some(μ), sum) =>
          val υidυ = id
          val υidυ2 = id

          val `μ.generate()` = μ.generate() match
            case (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
              it.copy(pat = Pat.Var(υidυ2)) :: tl

          val `!.μ⋯` = `μ.generate()` :+ `_ <- *` { Term.If(Term.ApplyInfix(\(υidυ2), \("eq"),
                                                                            Type.ArgClause(Nil),
                                                                            Term.ArgClause(\("None") :: Nil, None)),
                                                            `IO.cede`,
                                                            υidυ,
                                                            Nil)
                                                  }

          val it = `NonEmptyList( *, … ).parSequence`(
                     sum.generate(),
                     `!.μ⋯`
                   )

          * = `* <- *`(υidυ -> `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `!.μ⋯`

        case !(_, sum) =>
          val υidυ = id

          val it = `NonEmptyList( *, … ).parSequence`(
                     sum.generate(),
                     `_ <- IO.unit` :: `_ <- *`(υidυ)
                   )

          * = `* <- *`(υidυ, `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `_ <- *`(υidυ)

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(_, variables, _sum, assign) =>
          val ** = assign
            .map(_.name -> _.name)
            .map(Pat.Var(_) -> _)
            .map(Enumerator.Val(_, _))
            .toList

          val n = assign.size

          val sum = if (variables.size == n)
                    then
                      _sum
                    else
                      `+`(∥(`.`(_sum, ν(variables.drop(n).map(Int.MaxValue -> _.name).toSeq*))))

          * = ** ++ sum.generate()

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, qual, params*) =>
          val args = params.map(_.toTerm).toList

          val term = qual match
            case h :: t => (t.map(\(_)) :+ \("π") :+ \(identifier)).foldLeft(h: Term)(Term.Select(_, _))
            case _ => \(identifier)

          * :+= `_ <- *`(Term.Apply(term, Term.ArgClause(args, None)))

        ////////////////////////////////////////////////////////// invocation //

      *


  final class Main:

    def apply(prog: List[Bind]): List[String] =
      val id = new helper.υidυ
      prog.map(_ -> _.generate(using id())).map(_.swap).map(defn(_)(_).toString)
