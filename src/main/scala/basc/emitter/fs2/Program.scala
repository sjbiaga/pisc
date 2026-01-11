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

package basc
package emitter
package fs2

import scala.meta.*
import dialects.Scala3

import parser.Calculus.*
import fs2.Meta.*


object Program:

  private def pace(args: List[Term])(using pace: Option[(Long, String)]) =
    pace match
      case Some((time, unit)) => Term.Select(Lit.Long(time), unit) :: args
      case _ => args

  extension (self: Pre | AST)(using id: => String, `^,set,get`: ((Enumerator.Generator, Term.Name), (Enumerator.Generator, Enumerator.Generator, Term.Name)))

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

    def emit(implicit _pace: Option[(Long, String)] = None): List[Enumerator] =

      implicit val ^ = `^,set,get`._1
      val sg = `^,set,get`._2

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
          * :+= sg._2
          * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                    Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)))
          * = * ::: `Stream.eval(…)`(enums)

        case it @ τ(r, Some((Right(term), _))) =>
          * :+= sg._2
          * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                               Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                    Term.ArgClause(term :: Nil)))

        case it @ τ(r, _) =>
          * :+= sg._2
          * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                    Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)))


        case it @ π(dir, λ(Symbol(ch)), λ(Symbol(par)), Some(nu @ "ν"), r, code) =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), nu), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                     Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                          Term.ArgClause(\(s"π-$dir") :: Nil)),
                                               Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), nu), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                     Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                          Term.ArgClause(\(s"π-$dir") :: Nil)),
                                               Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), nu), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                          Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                               Term.ArgClause(\(s"π-$dir") :: Nil)))

        case it @ π(dir, λ(Symbol(ch)), λ(arg: Term), None, r, code) =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: \("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$dir") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: \("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$dir") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: \("}{") :: Nil))),
                                                   Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                        Term.ArgClause(\(s"π-$dir") :: Nil)))

        case it @ π(dir, λ(Symbol(ch)), arg, None, r, code) =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "π"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: \("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$dir") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "π"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: \("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$dir") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "π"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: \("}{") :: Nil))),
                                                   Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                        Term.ArgClause(\(s"π-$dir") :: Nil)))

        case it @ π(dir, λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code) =>

          val par = if λ.`type`.isDefined then id else arg

          * :+= sg._2

          code match
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "π"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                     Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                          Term.ArgClause(\(s"π-$dir") :: Nil)),
                                               Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(ch, "π"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                          Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                               Term.ArgClause(\(s"π-$dir") :: Nil)))

          λ.`type` match
            case Some((tpe, Some(refined))) =>
              * :+= `* = *: * …`(arg, par, tpe, refined)
            case Some((tpe, _)) =>
              * :+= `* = *: *`(arg, par, tpe)
            case _ =>

        case π(dir, λ(Symbol(ch)), λ(params: List[`λ`]), Some(cons), _, code) =>
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

        case it @ ζ(cap, name, _, r, code) =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(name, "ζ"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$cap") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(name, "ζ"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$cap") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(name, "ζ"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                   Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                        Term.ArgClause(\(s"π-$cap") :: Nil)))

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

        case !(1, given Option[(Long, String)], Some(it @ π(dir, λ(Symbol(ch)), λ(Symbol(par)), Some(nu @ "ν"), r, code)), sum) =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), nu), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                     Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                          Term.ArgClause(\(s"π-$dir") :: Nil)),
                                               Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), nu), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                     Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                          Term.ArgClause(\(s"π-$dir") :: Nil)),
                                               Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), nu), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                          Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                               Term.ArgClause(\(s"π-$dir") :: Nil)))

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(it @ π(dir, λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code)), sum) =>
          val par = if λ.`type`.isDefined then id else arg

          * :+= sg._2

          code match
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), "!"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                     Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                          Term.ArgClause(\(s"π-$dir") :: Nil)),
                                               Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `* <- *`(par -> Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), "!"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                          Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                               Term.ArgClause(\(s"π-$dir") :: Nil)))

          λ.`type` match
            case Some((tpe, Some(refined))) =>
              * :+= `* = *: * …`(arg, par, tpe, refined)
            case Some((tpe, _)) =>
              * :+= `* = *: *`(arg, par, tpe)
            case _ =>

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(it @ π(dir, λ(Symbol(ch)), λ(arg: Term), None, r, code)), sum) =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: \("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$dir") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: \("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$dir") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: \("}{") :: Nil))),
                                                   Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                        Term.ArgClause(\(s"π-$dir") :: Nil)))

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(it @ π(dir, λ(Symbol(ch)), arg, None, r, code)), sum) =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), "!"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: \("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$dir") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), "!"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: \("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$dir") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "π"), "!"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: \("}{") :: Nil))),
                                                   Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                        Term.ArgClause(\(s"π-$dir") :: Nil)))

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(it @ τ(r, code)), sum) =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                        Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)))
              * = * ::: `Stream.eval(…)`(enums)
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                   Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                        Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)))

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(it @ ζ(cap, name, _, r, code)), sum) =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(name, "ζ"), "!"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$cap") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(name, "ζ"), "!"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                              Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                   Term.ArgClause(\(s"π-$cap") :: Nil)),
                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * :+= `_ <- *`(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(name, "ζ"), "!"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                   Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                        Term.ArgClause(\(s"π-$cap") :: Nil)))

          * = * ::: sum.emit()

        case !(parallelism, given Option[(Long, String)], Some(it @ π(dir, λ(Symbol(ch)), λ(Symbol(par)), Some(nu @ "ν"), r, code)), sum) if parallelism < -1 =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = * ::: `* <- +`(par, -parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), nu), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                  Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                       Term.ArgClause(\(s"π-$dir") :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 sum.emit())
            case Some((Right(term), _)) =>
              val expr = term
              * = * ::: `* <- +`(par, -parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), nu), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                  Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                       Term.ArgClause(\(s"π-$dir") :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 sum.emit())
            case _ =>
              * = * ::: `* <- +`(par, -parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), nu), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                       Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                            Term.ArgClause(\(s"π-$dir") :: Nil)),
                                 sum.emit())

        case !(parallelism, given Option[(Long, String)], Some(it @ π(dir, λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code)), sum) if parallelism < -1 =>
          val par = if λ.`type`.isDefined then id else arg

          val `val` =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `* = *: * …`(arg, par, tpe, refined) :: Nil
              case Some((tpe, _)) =>
                `* = *: *`(arg, par, tpe) :: Nil
              case _ =>
                Nil

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = * ::: `* <- +`(par, -parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                  Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                       Term.ArgClause(\(s"π-$dir") :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 `val` ::: sum.emit())
            case Some((Right(term), _)) =>
              val expr = term
              * = * ::: `* <- +`(par, -parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                  Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                       Term.ArgClause(\(s"π-$dir") :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 `val` ::: sum.emit())
            case _ =>
              * = * ::: `* <- +`(par, -parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                       Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                            Term.ArgClause(\(s"π-$dir") :: Nil)),
                                 `val` ::: sum.emit())

        case !(parallelism, given Option[(Long, String)], Some(it @ π(dir, λ(Symbol(ch)), λ(arg: Term), None, r, code)), sum) if parallelism < -1 =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: \("}{") :: Nil))),
                                                                  Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                       Term.ArgClause(\(s"π-$dir") :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 sum.emit())
            case Some((Right(term), _)) =>
              val expr = term
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: \("}{") :: Nil))),
                                                                  Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                       Term.ArgClause(\(s"π-$dir") :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 sum.emit())
            case _ =>
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), "*"), Term.ArgClause(rate(r.get) :: pace(arg :: \("}{") :: Nil))),
                                                       Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                            Term.ArgClause(\(s"π-$dir") :: Nil)),
                                 sum.emit())

        case !(parallelism, given Option[(Long, String)], Some(it @ π(dir, λ(Symbol(ch)), arg, None, r, code)), sum) if parallelism < -1 =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: \("}{") :: Nil))),
                                                                  Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                       Term.ArgClause(\(s"π-$dir") :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 sum.emit())
            case Some((Right(term), _)) =>
              val expr = term
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: \("}{") :: Nil))),
                                                                  Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                       Term.ArgClause(\(s"π-$dir") :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 sum.emit())
            case _ =>
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(ch, "π"), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(arg.toTerm :: \("}{") :: Nil))),
                                                       Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                            Term.ArgClause(\(s"π-$dir") :: Nil)),
                                 sum.emit())

        case !(parallelism, given Option[(Long, String)], Some(it @ τ(r, code)), sum) if parallelism < -1 =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                            Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                 `Stream.eval(…)`(enums) ::: sum.emit())
            case Some((Right(term), _)) =>
              val expr = term
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                       Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 sum.emit())
            case _ =>
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                            Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                 sum.emit())

        case !(parallelism, given Option[(Long, String)], Some(it @ ζ(cap, name, _, r, code)), sum) if parallelism < -1 =>

          * :+= sg._2

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(name, "ζ"), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                  Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                       Term.ArgClause(\(s"π-$cap") :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 sum.emit())
            case Some((Right(term), _)) =>
              val expr = term
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(name, "ζ"), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                                  Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                                       Term.ArgClause(\(s"π-$cap") :: Nil)),
                                            Term.ArgClause(expr :: Nil)),
                                 sum.emit())
            case _ =>
              * = * ::: `_ <- +`(-parallelism,
                                 Term.Apply(Term.Apply(Term.Apply(Term.Select(Term.Select(Term.Select(name, "ζ"), "!"), "+"), Term.ArgClause(rate(r.get) :: pace(\("}{") :: Nil))),
                                                       Term.ArgClause(Lit.String(it.υidυ) :: sg._3 :: Nil)),
                                            Term.ArgClause(\(s"π-$cap") :: Nil)),
                                 sum.emit())

        case !(parallelism, given Option[(Long, String)], Some(π @ π(_, _, λ @ λ(Symbol(arg)), Some(_), _, _)), sum) if λ.`type`.isDefined =>
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

          val sem = if parallelism < 0 then null else id

          val `!⋯` =
            if parallelism < 0
            then
              Term.Block(`val` :+
                         `List( *, … ).parSequence`(
                           sum.emit(),
                           `!.π⋯`
                         ))
            else
              `!.π⋯` = `_ <- *.acquire`(sem) :: `!.π⋯`
              Term.Block(`val` :+
                         `List( *, … ).parSequence`(
                           sum.emit() :+ `_ <- *.release`(sem),
                           `!.π⋯`
                         ))

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `\\.emit { def *(*: ()[F]): String => \\[F, Unit] = { implicit ^ => … }; * }`(υidυ -> par, `!⋯`)) :: `!.π⋯`
          else
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `\\.emit { def *(*: ()[F]): String => \\[F, Unit] = { implicit ^ => … }; * }`(υidυ -> par, `!⋯`)) :: `!.π⋯`

        case !(parallelism, given Option[(Long, String)], Some(π @ π(_, _, λ(Symbol(par)), Some(_), _, _)), sum) =>
          val υidυ = id

          var `!.π⋯` = π.emit :+ ^._1 :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(par :: Nil)),
                                                             Term.ArgClause(^._2 :: Nil)))

          val sem = if parallelism < 0 then null else id

          val `!⋯` =
            if parallelism < 0
            then
              `List( *, … ).parSequence`(
                sum.emit(),
                `!.π⋯`
              )
            else
              `!.π⋯` = `_ <- *.acquire`(sem) :: `!.π⋯`
              `List( *, … ).parSequence`(
                sum.emit() :+ `_ <- *.release`(sem),
                `!.π⋯`
              )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `\\.emit { def *(*: ()[F]): String => \\[F, Unit] = { implicit ^ => … }; * }`(υidυ -> par, `!⋯`)) :: `!.π⋯`
          else
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `\\.emit { def *(*: ()[F]): String => \\[F, Unit] = { implicit ^ => … }; * }`(υidυ -> par, `!⋯`)) :: `!.π⋯`

        case !(parallelism, given Option[(Long, String)], Some(μ), sum) =>
          val υidυ = id

          var `!.μ⋯` = μ.emit :+ ^._1 :+ `_ <- *`(Term.Apply(\(υidυ), Term.ArgClause(^._2 :: Nil)))

          val sem = if parallelism < 0 then null else id

          val `!⋯` =
            if parallelism < 0
            then
              `List( *, … ).parSequence`(
                sum.emit(),
                `!.μ⋯`
              )
            else
              `!.μ⋯` = `_ <- *.acquire`(sem) :: `!.μ⋯`
              `List( *, … ).parSequence`(
                sum.emit() :+ `_ <- *.release`(sem),
                `!.μ⋯`
              )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `\\.emit { lazy val *: String => \\[F, Unit] = { implicit ^ => … }; * }`(υidυ, `!⋯`)) :: `!.μ⋯`
          else
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `\\.emit { lazy val *: String => \\[F, Unit] = { implicit ^ => … }; * }`(υidυ, `!⋯`)) :: `!.μ⋯`

        case _ : ! => ??? // caught by 'parse'

        ///////////////////////////////////////////////////////// replication //


        // AMBIENT /////////////////////////////////////////////////////////////

        case `[]`(label, sum) =>
          val labelʹ = label
            .map { it => Term.Apply(\("Some"), Term.ArgClause(Lit.String(it) :: Nil)) }
            .getOrElse(\("None"))

          * :+= sg._2
          * :+= `* <- Stream.eval(*)`(sg._3.value -> Term.Apply(Term.Select(\("}{"), \("}{")), Term.ArgClause(sg._3 :: labelʹ :: Nil)))
          * :+= sg._1

          * = `_ <- *`(`List( *, … ).parSequence`(* ::: sum.emit))

        ///////////////////////////////////////////////////////////// ambient //


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

          * = ^._1 :: `_ <- *`(Term.Apply(Term.Apply(Term.Apply(\(identifier), Term.ArgClause(\(")(") :: \("}{") :: Nil)), Term.ArgClause(args)),
                                          Term.ArgClause(^._2 :: Nil, Some(Mod.Using()))))

        ////////////////////////////////////////////////////////// invocation //

      *


  final class Main(threadlocal: String):

    implicit private def `*[F]`(* : List[Enumerator]): Term =
      if *.nonEmpty then `for *[F] yield ()`(* *)
      else \(`_ <- \\.unit`)

    def apply(prog: List[Bind]): List[Stat] =
      val id = new helper.υidυ

      given Type = Type.Apply(\\(threadlocal), Type.ArgClause(\\(")(") :: Nil))

      val `^-υidυ` = id()
      val sg_υidυ = id()

      given ((Enumerator.Generator, Term.Name), (Enumerator.Generator, Enumerator.Generator, Term.Name)) =
        (`* <- Stream.eval(*)`(`^-υidυ` -> Term.ApplyType(\("π-uuid"), Type.ArgClause(\\("F") :: Nil))), \(`^-υidυ`)) ->
        (`_ <- Stream.eval(*)`(Term.ApplyType(Term.Select(Term.Apply(Term.Select(")(", "set"), Term.ArgClause(\(sg_υidυ) :: Nil)), "asInstanceOf"), Type.ArgClause(Type.Apply(\\("F"), Type.ArgClause(\\("Unit") :: Nil)) :: Nil)))
        ,`* <- Stream.eval(*)`(sg_υidυ -> Term.ApplyType(Term.Select(Term.Select(")(", "get"), "asInstanceOf"), Type.ArgClause(Type.Apply(\\("F"), Type.ArgClause(Type.Select("sΠ", \\(")(")) :: Nil)) :: Nil)))
        ,\(sg_υidυ))

      prog
        .drop(3)
        .map(_ -> _.emit(using id()))
        .map(_.swap)
        .map(defn(_)(_))
