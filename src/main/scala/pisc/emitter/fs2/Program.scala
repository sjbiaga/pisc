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

    def emitʹ(implicit semaphore: Option[String]): List[Enumerator] =

      self match

        case ∥(_, operand) =>
          operand.emitʹ

        case it @ `.`(?:(_, _, None)) =>
          def cases(sum: +): Term =
            sum match
              case +(_, ∥(_, `.`(?:(((lhs, rhs), mismatch), t, None)))) =>
                if mismatch
                then
                  `if * then … else …`(====(lhs, rhs), Nil, cases(t))
                else
                  `if * then … else …`(====(lhs, rhs), cases(t), Nil)
              case _ =>
                val υidυ = id
                `* <- Stream.eval(*)`(υidυ -> Term.Select(semaphore.get, "tryAcquire")) ::
                Enumerator.Generator(`* <- …`(), `if * then … else …`(υidυ, sum.emit, Nil))

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
          implicit val sem = Some(id)

          val sfs = it.choices.foldRight(List[Term]())(_.emitʹ :: _)

          * = List(
            `* <- Semaphore[F](…)`(sem.get),
            `_ <- *`(`List( *, … ).parJoin`(sfs*))
          )

        case it: + =>
          val sfs = it.choices.foldRight(List[Term]())(_.emit :: _)

          val sem = id

          * = List(
            `* <- Semaphore[F](…)`(sem),
            `_ <- *`(`List( *, … ).parJoin(…)`(sfs*)(sem))
          )

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(_, operand) =>
          * = operand.emit

        case it: ∥ =>
          val sfs = it.components.foldRight(List[Term]())(_.emit :: _)

          * = `_ <- *`(`List( *, … ).parJoin`(sfs*))

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, it*) =>
          * = (it :+ end).foldLeft(*)(_ ::: _.emit)

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> `*[F]`("ν")) }.toList

        case τ(Some((Left(enums), _))) =>
          * = `_ <- *`(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(Nil)))
          * = * ::: `Stream.eval(…)`(enums)

        case τ(Some((Right(term), _))) =>
          val expr = term
          * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(Nil)), Term.ArgClause(expr :: Nil)))

        case τ(_) =>
          * = `_ <- *`(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(Nil)))


        case π(λ(Symbol(ch)), Some(nu @ "ν"), code, params*) =>

          val args = params.map(_.asSymbol.name)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(ch, nu), Term.ArgClause(Lit.Int(args.size) :: Nil)),
                                                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(ch, nu), Term.ArgClause(Lit.Int(args.size) :: Nil)),
                                                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Select(ch, nu), Term.ArgClause(Lit.Int(args.size) :: Nil)))

        case π(λ(Symbol(ch)), None, code, args*) if args.forall { case λ(Lit.Null()) => true case _ => false } =>

          val arity = Lit.Int(args.size)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "null"), Term.ArgClause(arity :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "null"), Term.ArgClause(arity :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(ch, "null"), Term.ArgClause(arity :: Nil)))

        case π(λ(Symbol(ch)), None, code, args*) if args.forall { case λ(_: Term) => true case _ => false } =>

          val argc = Term.ArgClause(args.map(_.toTerm).toList)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "*"), argc),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "*"), argc),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(ch, "*"), argc))

        case π(λ(Symbol(ch)), None, code, args*) =>

          val argc = Term.ArgClause(args.map(_.toTerm).toList)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(\(ch), argc),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(\(ch), argc),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(\(ch), argc))

        case π(λ(Symbol(ch)), Some(""), code, params*) =>
          val args = params.map {
            case λ @ λ(Symbol(_)) if λ.`type`.isDefined => id
            case λ(Symbol(par)) => par
          }

          code match
            case Some((Right(term), _)) =>
              val expr = term
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(\(ch), Term.ArgClause(Nil)),
                                                                        Term.ArgClause(expr :: Nil)))
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

        case π(λ(Symbol(ch)), Some(cons), code, params*) =>
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
          * = f.fold(Nil)(_.emit)

          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), *, t.emit))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), t.emit, *))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(_, Some((time, unit)), Some(π(λ(Symbol(ch)), Some(nu @ "ν"), code, params*)), sum) =>
          val pace = Term.Select(Lit.Long(time), unit)

          val args = params.map(_.asSymbol.name)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(pace :: Nil)),
                                                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(pace :: Nil)),
                                                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(pace :: Nil)))

          * = * ::: sum.emit

        case !(_, None, Some(π(λ(Symbol(ch)), Some(nu @ "ν"), code, params*)), sum) =>

          val args = params.map(_.asSymbol.name)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(Nil)),
                                                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(Nil)),
                                                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(Nil)))

          * = * ::: sum.emit

        case !(_, Some((time, unit)), Some(π(λ(Symbol(ch)), Some(_), code, params*)), sum) =>
          val pace = Term.Select(Lit.Long(time), unit)

          val args = params.map {
            case λ @ λ(Symbol(_)) if λ.`type`.isDefined => id
            case λ(Symbol(par)) => par
          }

          code match
            case Some((Right(term), _)) =>
              val expr = term
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(pace :: Nil)),
                                                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Select(ch, "!"), Term.ArgClause(pace :: Nil)))

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

          * = * ::: sum.emit

        case !(_, None, Some(π(λ(Symbol(ch)), Some(_), code, params*)), sum) =>

          val args = params.map {
            case λ @ λ(Symbol(_)) if λ.`type`.isDefined => id
            case λ(Symbol(par)) => par
          }

          code match
            case Some((Right(term), _)) =>
              val expr = term
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(Nil)),
                                                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Select(ch, "!"), Term.ArgClause(Nil)))

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

          * = * ::: sum.emit

        case !(_, Some((time, unit)), Some(π(λ(Symbol(ch)), None, code, args*)), sum) if args.forall { case λ(Lit.Null()) => true case _ => false } =>
          val pace = Term.Select(Lit.Long(time), unit)

          val arity = Lit.Int(args.size)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "null"), Term.ArgClause(pace :: arity :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "null"), Term.ArgClause(pace :: arity :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Select(ch, "!"), "null"), Term.ArgClause(pace :: arity :: Nil)))

          * = * ::: sum.emit

        case !(_, None, Some(π(λ(Symbol(ch)), None, code, args*)), sum) if args.forall { case λ(Lit.Null()) => true case _ => false } =>

          val arity = Lit.Int(args.size)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "null"), Term.ArgClause(arity :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "null"), Term.ArgClause(arity :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Select(ch, "!"), "null"), Term.ArgClause(arity :: Nil)))

          * = * ::: sum.emit

        case !(_, Some((time, unit)), Some(π(λ(Symbol(ch)), None, code, args*)), sum) if args.forall { case λ(_: Term) => true case _ => false } =>
          val pace = Term.Select(Lit.Long(time), unit)

          val argc = Term.ArgClause(pace :: args.map(_.toTerm).toList)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), argc),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), argc),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), argc))

          * = * ::: sum.emit

        case !(_, None, Some(π(λ(Symbol(ch)), None, code, args*)), sum) if args.forall { case λ(_: Term) => true case _ => false } =>
          val argc = Term.ArgClause(args.map(_.toTerm).toList)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), argc),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), argc),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Select(ch, "!"), "*"), argc))

          * = * ::: sum.emit

        case !(_, Some((time, unit)), Some(π(λ(Symbol(ch)), None, code, args*)), sum) =>
          val pace = Term.Select(Lit.Long(time), unit)

          val argc = Term.ArgClause(pace :: args.map(_.toTerm).toList)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "!"), argc),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "!"), argc),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(ch, "!"), argc))

          * = * ::: sum.emit

        case !(_, None, Some(π(λ(Symbol(ch)), None, code, args*)), sum) =>

          val argc = Term.ArgClause(args.map(_.toTerm).toList)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "!"), argc),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "!"), argc),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(ch, "!"), argc))

          * = * ::: sum.emit

        case !(_, Some((time, unit)), Some(τ(code)), sum) =>
          val pace = Term.Select(Lit.Long(time), unit)

          code match
            case Some((Left(enums), _)) =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(pace :: Nil)))
              * = * ::: `Stream.eval(…)`(enums)
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(pace :: Nil)),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(pace :: Nil)))

          * = * ::: sum.emit

        case !(_, None, Some(τ(code)), sum) =>

          code match
            case Some((Left(enums), _)) =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"),
                                      Term.ArgClause(Nil)))
              * = * ::: `Stream.eval(…)`(enums)
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"),
                                      Term.ArgClause(Nil)))

          * = * ::: sum.emit

        case !(_, Some((time, unit)), _, sum) =>
          val pace = Term.Select(Lit.Long(time), unit)

          * = `_ <- *`(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(pace :: Nil)))

          * = * ::: sum.emit

        case !(_, None, _, sum) =>

          * = `_ <- *`(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(Nil)))

          * = * ::: sum.emit

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

        case `(*)`(identifier, qual, params*) =>
          val args = params.map(_.toTerm).toList

          val term = qual match
            case h :: t => (t.map(\(_)) :+ \("π") :+ \(identifier)).foldLeft(h: Term)(Term.Select(_, _))
            case _ => \(identifier)

          * = `_ <- *`(Term.Apply(Term.ApplyType(term, Type.ArgClause(\\("F") :: Nil)), Term.ArgClause(args)))

        ////////////////////////////////////////////////////////// invocation //

      *


  final class Main(F: String):

    implicit private def `*[F]`(* : List[Enumerator]): Term =
      if *.nonEmpty then `for *[F] yield ()`(* *)
      else \(`_ <- \\.unit`)

    def apply(prog: List[Bind]): List[String] =
      val id = new helper.υidυ

      given Set[String] =
        prog.head match
          case (`(*)`(_, _, λ(_: Lit.Null)), _) => Set("Temporal")
          case (`(*)`(_, _, λ(typeclasses: Term.Tuple)), _) =>
            typeclasses.args.map { case Term.Name(it) => it }.toSet

      val Array(tpe, _path*) = F.split('.').reverse
      val path = _path.reverse.map(\(_)).foldLeft("_root_": Term)(Term.Select(_, _))

      Defn.Type(Nil, \\("F"), Type.ParamClause(Nil),
                Type.Select(path.asInstanceOf[Term.Select], \\(tpe)),
                Type.Bounds(None, None, Nil, Nil)).toString
      ::
      prog
        .drop(1)
        .map(_ -> _.emit(using id()))
        .map(_.swap)
        .map(defn(_)(_).toString)
