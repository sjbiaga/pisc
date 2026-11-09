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

import scala.meta.{ Lit, Term }

import parser.Calculus.*
import parser.Encoding.*
import go.Gala.*
import go.Gala.given


object Program:

  given (Int => (Int, Int)) = { _ => (0, 0) }

  extension (self: Pre | AST)

    def emitʹ: body =

      self match

        case ∥(operand) =>
          operand.emitʹ

        case it @ `.`(?:(_, _, None)) =>
          def cases(sum: +): body =
            sum match
              case +(∥(`.`(?:(cond, t, None)))) =>
                elvis(cond, cases(t), exclude(t.enabled.map(Lit.String(_).toString).toList) :: Nil) :: Nil
              case _ =>
                sum.emit

          cases(`+`(∥(it)))

        case _ => ???

    def emit: body =

      var * = List[Gala]()

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>
          * = void()

        case +(operand) =>
          * = operand.emit

        case it: + if it.choices.forall { case ∥(`.`(?:(_, _, None))) => true case _ => false } =>
          val ops = it.choices.foldRight(List[body]())(_.emitʹ :: _)

          * = par(ops)

        case it: + =>
          val ops = it.choices.foldRight(List[body]())(_.emit :: _)

          * = sum(ops)

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(operand) =>
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
          * = nu(names)


        case it @ τ(r, Some((Right(term), _))) =>
          * = tau(rate(r.get), Lit.String(it.υidυ).toString) :: code(term) :: Nil

        case it @ τ(r, _) =>
          * = tau(rate(r.get), Lit.String(it.υidυ).toString) :: noop() :: Nil


        case it @ π(λ(Symbol(ch)), λ(Symbol(arg)), Some("ν"), r, opt) if ch == arg =>

          val par: alloc = alloc()

          opt match
            case Some((Right(term), _)) =>
              * = par :: nu(par) :: pi(ch, λ(par), None, rate(r.get), Lit.String(it.υidυ).toString) :: code(term) :: init(Right(ch -> par)) :: Nil
            case _ =>
              * = par :: nu(par) :: pi(ch, λ(par), None, rate(r.get), Lit.String(it.υidυ).toString) :: noop() :: init(Right(ch -> par)) :: Nil

        case it @ π(λ(Symbol(ch)), arg @ λ(Symbol(par)), Some("ν"), r, opt) =>

          opt match
            case Some((Right(term), _)) =>
              * = nu(Seq(par)) :: pi(ch, arg, None, rate(r.get), Lit.String(it.υidυ).toString) :: code(term) :: Nil
            case _ =>
              * = nu(Seq(par)) :: pi(ch, arg, None, rate(r.get), Lit.String(it.υidυ).toString) :: noop() :: Nil

        case it @ π(λ(Symbol(ch)), arg, None, r, opt) =>

          opt match
            case Some((Right(term), _)) =>
              * = pi(ch, arg, None, rate(r.get), Lit.String(it.υidυ).toString) :: code(term) :: Nil
            case _ =>
              * = pi(ch, arg, None, rate(r.get), Lit.String(it.υidυ).toString) :: noop() :: Nil

        case it @ π(λ(Symbol(ch)), par @ λ(Symbol(_)), Some(""), r, opt) =>

          opt match
            case Some((Right(term: Term.Name), _)) =>
              * = pi(ch, par, Some(term), rate(r.get), Lit.String(it.υidυ).toString) :: noop() :: Nil
            case Some((Right(term), _)) =>
              * = pi(ch, par, Some(null), rate(r.get), Lit.String(it.υidυ).toString) :: code(term) :: Nil
            case _ =>
              * = pi(ch, par, Some(null), rate(r.get), Lit.String(it.υidυ).toString) :: noop() :: Nil

        case _: π => ???

        ////////////////////////////////////////////// restriction | prefixes //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(cond, t, f) =>

          * = elvis(cond, t.emit, f.fold(Nil: body)(_.emit))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(parallelism, paceʹ, Some(π @ π(_, _, Some("ν"), _, _)), sum) =>

          * = rep(parallelism, paceʹ.map(pace(_, _)).getOrElse(noop()) :: π.emit, false, sum.emit)

        case !(parallelism, paceʹ, Some(π @ π(_, _, Some(""), _, _)), sum) =>

          * = rep(parallelism, paceʹ.map(pace(_, _)).getOrElse(noop()) :: π.emit, true, sum.emit)

        case !(parallelism, paceʹ, Some(μ), sum) =>

          * = rep(parallelism, paceʹ.map(pace(_, _)).getOrElse(noop()) :: μ.emit, false, sum.emit)

        case _: ! => ???

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(Definition(_, _, _, variables, _), sum, _, pointers) =>

          * = init(Left((variables zip pointers).map(_.name -> _.name).toList)) :: sum.emit

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, params*) =>

          * = call(identifier, params)

        ////////////////////////////////////////////////////////// invocation //

      *


  final class Main:

    def apply(prog: List[Bind]): List[func] =
      prog
        .map(_ -> _.emit)
        .map(_.swap)
        .map(defn(_)(_))
