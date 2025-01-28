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

import scala.meta._
import dialects.Scala3

import parser.Calculus._
import Meta._


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

          val ios = it.choices.foldLeft(List[Term]())(_ :+ _.generate)

          val ** = List(
            `* <- Semaphore[IO](1)`(sem.get),
            `_ <- *`(`NonEmptyList( *, … ).parTraverse(identity)`(ios*))
          )

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* ++= **)

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(operand) =>
          * = operand.generate

        case it: ∥ =>
          val ios = it.components.foldLeft(List[Term]())(_ :+ _.generate())

          val ** = `_ <- *`(`NonEmptyList( *, … ).parTraverse(identity)`(ios*))

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* ++= **)

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case it @ `.`(?:(_, _, None)) if semaphore.nonEmpty =>
          def cases(sum: +): Term =
            sum match
              case +(∥(`.`(?:(((λ(lhs), λ(rhs)), mismatch), t, None)))) =>
                if mismatch
                then
                  `if * then … else …`(====(lhs -> rhs), Nil, cases(t))
                else
                  `if * then … else …`(====(lhs -> rhs), cases(t), Nil)
              case _ =>
                `_ <- *.tryAcquire.ifM`(semaphore.get, sum.generate())

          * = `_ <- *`(cases(`+`(∥(it))))

        case `.`(end, it*) =>
          val ** =
            if it.isEmpty
            then
              end.generate()

            else
              val ios = `.`(end, it.tail*).generate()

              it.head match
                case χ(Right(`⟦⟧`(_, _, _, υidυ, Symbol(name), _))) =>
                  `_ <- *`(`NonEmptyList( *, … ).parTraverse(identity)`(`* <- }{(*)()()`(name, υidυ) :: ios)) :: Nil

                case χ(Left(Symbol(name))) =>
                  `_ <- }{(*)()()`(name) :: ios

                case _ =>
                  it.head.generate() ++ ios

          semaphore
            .map(* :+= `_ <- *.tryAcquire.ifM`(_, **))
            .getOrElse(* ++= **)

        case _: χ => ??? // handled above

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> "ν") }.toList


        case τ(Some((Left(enums), _))) =>
          * = `_ <- *`("τ")
          * ++= enums

        case τ(Some((Right(term), _))) =>
          * = `_ <- *`("τ")
          * :+= `_ <- IO { * }`(term)

        case τ(_) =>
          * = `_ <- *`("τ")


        case π(ch, _, _, _) if !ch.isSymbol => ??? // not a channel name - caught by parser

        case π(λ(Symbol(_)), par, true, _) if !par.isSymbol => ??? // not binding a name - caught by parser

        case π(λ(Symbol(ch)), λ(Symbol(arg)), false, Some((Left(enums), _))) =>
          val code = `for * yield ()`(enums*)
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(\(arg) :: \(")(") :: Nil, None)),
                         Term.ArgClause(code::Nil, None)
                       ))

        case π(λ(Symbol(ch)), λ(Symbol(arg)), false, Some((Right(term), _))) =>
          val code = `for * yield ()`(`_ <- IO { * }`(term))
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(\(arg) :: \(")(") :: Nil, None)),
                         Term.ArgClause(code::Nil, None)
                       ))

        case π(λ(Symbol(ch)), λ(Symbol(arg)), false, _) =>
          * = `_ <- *`(Term.Apply(\(ch), Term.ArgClause(\(arg) :: \(")(") :: Nil, None)))

        case π(λ(Symbol(ch)), λ(Expr(term)), false, Some((Left(enums), _))) =>
          val code = `for * yield ()`(enums*)
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(term :: \(")(") :: Nil, None)),
                         Term.ArgClause(code::Nil, None)
                       ))

        case π(λ(Symbol(ch)), λ(Expr(term)), false, Some((Right(term2), _))) =>
          val code = `for * yield ()`(`_ <- IO { * }`(term2))
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(term :: \(")(") :: Nil, None)),
                         Term.ArgClause(code::Nil, None)
                       ))

        case π(λ(Symbol(ch)), λ(Expr(term)), false, _) =>
          * = `_ <- *`(Term.Apply(\(ch), Term.ArgClause(term :: \(")(") :: Nil, None)))

        case π(λ(Symbol(ch)), λ(arg), false, Some((Left(enums), _))) =>
          val code = `for * yield ()`(enums*)
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(s"$arg".parse[Term].get :: \(")(") :: Nil, None)),
                         Term.ArgClause(code::Nil, None)
                       ))

        case π(λ(Symbol(ch)), λ(arg), false, Some((Right(term), _))) =>
          val code = `for * yield ()`(`_ <- IO { * }`(term))
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\(ch), Term.ArgClause(s"$arg".parse[Term].get :: \(")(") :: Nil, None)),
                         Term.ArgClause(code::Nil, None)
                       ))

        case π(λ(Symbol(ch)), λ(arg), false, _) =>
          * = `_ <- *`(Term.Apply(\(ch), Term.ArgClause(s"$arg".parse[Term].get :: \(")(") :: Nil, None)))

        case π(_, _, true, Some((Left(_), _))) => ??? // Scalameta Enumerator - caught by parser

        case π(λ(Symbol(ch)), λ(Symbol(par)), true, Some((Right(code), _))) =>
          * = `* <- *`(par -> Term.Apply(
                                Term.Apply(\(ch), Term.ArgClause(\(")(") :: Nil, None)),
                                Term.ArgClause(code::Nil, None)
                       ))

        case π(λ(Symbol(ch)), λ(Symbol(par)), true, _) =>
          * = `* <- *`(par -> Term.Apply(\(ch), Term.ArgClause(\(")(") :: Nil, None)))

        case _: π => ??? // caught by parser

        ////////////////////////////////////////////// restriction | prefixes //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((λ(lhs), λ(rhs)), mismatch), t, f) =>
          * = f.map(_.generate()).getOrElse(Nil)

          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), *, t.generate()))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs -> rhs), t.generate(), *))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(Some(π @ π(_, λ(Symbol(par)), true, _)), sum) =>
          val υidυ = id

          val `!.π⋯` = π.generate() :+ `_ <- *`(Term.Apply(\(υidυ),
                                                           Term.ArgClause(\(par) :: Nil, None)))

          val it = Term.If(Term.ApplyUnary("!", par),
                           `IO.cede`,
                           `NonEmptyList( *, … ).parTraverse(identity)`(
                             sum.generate(),
                             `!.π⋯`
                           )
                   )

          * = `* <- *`(υidυ -> `IO { def *(*: ()): IO[Any] = …; * }`(υidυ -> par, it)) :: `!.π⋯`

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

          val it = `NonEmptyList( *, … ).parTraverse(identity)`(
                     sum.generate(),
                     `!.μ⋯`
                   )

          * = `* <- *`(υidυ -> `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `!.μ⋯`

        case !(_, sum) =>
          val υidυ = id

          val it = `NonEmptyList( *, … ).parTraverse(identity)`(
                     sum.generate(),
                     `_ <- IO.unit` :: `_ <- *`(υidυ)
                   )

          * = `* <- *`(υidυ, `IO { lazy val *: IO[Any] = …; * }`(υidυ, it)) :: `_ <- *`(υidυ)

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(_, variables, _sum, υidυ, name, assign) =>
          val ** = assign
            .map { _.map(_.name -> _.name)
                    .map(Pat.Var(_) -> _)
                    .map(Enumerator.Val(_, _))
                    .toList
            }.getOrElse(Nil)

          val n = assign.map(_.size).getOrElse(0)

          val sum = ( if variables.size == n
                      then
                        _sum
                      else
                        `+`(∥(`.`(_sum, ν(variables.drop(n).map(_.name).toSeq*))))
                    )

          * = ** ++ sum.generate()

          name match
            case Symbol(it) =>
              * = `_ <- *`(`NonEmptyList( *, … ).parTraverse(identity)`(`* <- χ; _ <- }{()(, *)`(it, υidυ) ++ *))
            case _ =>

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, qual, params*) =>
          val args = params.map {
            case λ(Symbol(name)) => s"`$name`"
            case λ(value) =>
              value match
                case it: BigDecimal => s"BigDecimal($it)"
                case it: Boolean => it.toString
                case it: String => it.toString
                case Expr(it) => it.toString
          }

          qual match
            case Nil =>
              * :+= `_ <- *`(s"""`$identifier`(`)(`)(${args.mkString(", ")})""".parse[Term].get)
            case _ =>
              * :+= `_ <- *`(s"""${qual.mkString(".")}.π.`$identifier`(`)(`)(${args.mkString(", ")})""".parse[Term].get)

        ////////////////////////////////////////////////////////// invocation //

      *


  final class Main:

    def apply(prog: List[Bind]): List[String] =
      val id = new helper.υidυ
      prog.map(_ -> _.generate(using id())).map(_.swap).map(defn(_)(_).toString)
