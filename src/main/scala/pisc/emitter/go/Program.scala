/*
 * Copyright (c) 2023-2026 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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
package go

import scala.meta.Term

import parser.Calculus.*
import parser.Encoding.*
import go.Gala.*
import go.Gala.given


object Program:

  given (Int => (Int, Int)) = { _ => (0, 0) }

  extension (self: Pre | AST)(using id: => String)

    def emitʹ(implicit semaphore: Option[String]): body =

      self match

        case ∥(_, operand) =>
          operand.emitʹ

        case it @ `.`(?:(_, _, None)) =>
          def cases(sum: +): body =
            sum match
              case +(_, ∥(_, `.`(?:(cond, t, None)))) =>
                elvis(cond, cases(t), Nil: body) :: Nil
              case _ =>
                Gala.cases(semaphore.get, tryacquire = true) :: sum.emit

          cases(`+`(-1, ∥(-1, it)))

        case _ => ???

    def emit: body =

      var * = List[Gala]()

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>
          * = void()

        case +(_, operand) =>
          * = operand.emit

        case it: + if it.choices.forall { case ∥(-1, `.`(?:(_, _, None))) => true case _ => false } =>
          implicit val sem = Some(id)

          val ops = it.choices.foldRight(List[body]())(_.emitʹ :: _)

          * = cases(sem.get) :: par(ops)

        case it: + =>
          val ops = it.choices.foldRight(List[body]())(_.emit :: _)

          * = sum(ops)

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(_, operand) =>
          * = operand.emit

        case it: ∥ =>
          val ops = it.components.foldRight(List[body]())(_.emit :: _)

          * = par(ops)

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, it*) =>
          * = ((it :+ end): Seq[Pre | AST]).foldLeft(*)(_ ::: _.emit)

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = nu(names*)


        case τ(Some((Right(term), _))) =>
          * = tau() :: code(term) :: Nil

        case τ(_) =>
          * = tau()


        case π(λ(Symbol(ch)), arg @ λ(Symbol(par)), Some("ν"), opt) =>

          opt match
            case Some((Right(term), _)) =>
              * = nu(par) :: bind(ch, arg, None) :: code(term) :: Nil
            case _ =>
              * = nu(par) :: bind(ch, arg, None) :: Nil

        case π(λ(Symbol(ch)), arg, None, opt) =>

          opt match
            case Some((Right(term), _)) =>
              * = bind(ch, arg, None) :: code(term) :: Nil
            case _ =>
              * = bind(ch, arg, None) :: Nil

        case π(λ(Symbol(ch)), par @ λ(Symbol(_)), Some(""), opt) =>

          opt match
            case Some((Right(term: Term.Name), _)) =>
              * = bind(ch, par, Some(term)) :: Nil
            case Some((Right(term), _)) =>
              * = bind(ch, par, Some(null)) :: code(term) :: Nil
            case _ =>
              * = bind(ch, par, Some(null)) :: Nil

        case _: π => ???

        ////////////////////////////////////////////// restriction | prefixes //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(cond, t, f) =>

          * = elvis(cond, t.emit, f.fold(Nil: body)(_.emit))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(parallelism, paceʹ, Some(π @ π(_, _, Some("ν"), _)), sum) =>

          * = rep(parallelism, paceʹ.map(pace(_, _)).getOrElse(noop()) :: π.emit, Some(false), sum.emit)

        case !(parallelism, paceʹ, Some(π @ π(_, _, Some(""), _)), sum) =>

          * = rep(parallelism, paceʹ.map(pace(_, _)).getOrElse(noop()) :: π.emit, Some(true), sum.emit)

        case !(parallelism, paceʹ, Some(μ), sum) =>

          * = rep(parallelism, paceʹ.map(pace(_, _)).getOrElse(noop()) :: μ.emit, Some(false), sum.emit)

        case !(parallelism, paceʹ, _, sum) =>

          * = rep(parallelism, paceʹ.map(pace(_, _)).getOrElse(noop()), None, sum.emit)

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(Definition(_, _, _, variables, _), sum, _, pointers) =>

          * = init((variables zip pointers).map(_.name -> _.name).toList) :: sum.emit

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, Nil, params*) =>

          * = call(identifier, params)

        case _: `(*)` => ???

        ////////////////////////////////////////////////////////// invocation //

      *


  final class Main:

    def apply(prog: List[Bind]): List[func] =
      val id = new helper.υidυ
      prog
        .map(_ -> _.emit(using id()))
        .map(_.swap)
        .map(defn(_)(using id())(_))
