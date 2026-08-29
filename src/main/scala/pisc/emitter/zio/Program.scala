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
package zio

import scala.meta.*
import dialects.Scala3

import parser.Calculus.*
import parser.Encoding.*
import zio.Meta.*


object Program:

  private def pace(args: List[Term])(using pace: Option[(Long, String)]) =
    pace match
      case Some((time, unit)) => Term.Select(Lit.Long(time), unit) :: args
      case _ => args

  extension (self: Pre | AST)(using id: => String, ^ : (Enumerator.Generator, Term.Name))

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

          * = `_ <- *`(`List( *, … ).collectAllPar`(ios*))

        case it: + =>
          val ios = it.choices.foldRight(List[Term]())(_.emit :: _)

          * = `_ <- *`(`List( *, … ).collectAllPar`(ios*))

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(_, operand) =>
          * = operand.emit

        case it: ∥ =>
          val ios = it.components.foldRight(List[Term]())(_.emit :: _)

          * = `_ <- *`(`List( *, … ).collectAllPar`(ios*))

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, ps*) =>
          * = (ps :+ end).foldLeft(*)(_ ::: _.emit)

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> "ν") }.toList

        case it @ τ(r, Some((Left(enums), _))) =>
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\("τ"),
                                    Term.ArgClause(rate(r.get)::Nil)),
                         Term.ArgClause(Lit.String(it.υidυ)::Nil)))
          * = * ::: enums

        case it @ τ(r, Some((Right(term), _))) =>
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\("τ"),
                                    Term.ArgClause(rate(r.get)::Nil)),
                         Term.ArgClause(Lit.String(it.υidυ)::Nil)))
          * :+= `_ <- ZIO { * }`(term)

        case it @ τ(r, _) =>
          * = `_ <- *`(Term.Apply(
                         Term.Apply(\("τ"),
                                    Term.ArgClause(rate(r.get)::Nil)),
                         Term.ArgClause(Lit.String(it.υidυ)::Nil)))


        case it @ π(λ(Symbol(ch)), arg @ λ(_: Term), None, r, code) =>

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(Term.Apply(\(ch), Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                          Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                               Term.ArgClause(Lit.String(it.υidυ)::Nil)
                             ),
                             Term.ArgClause(expr::Nil)
                           ))
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- ZIO { * }`(term))
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(Term.Apply(\(ch), Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                          Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                               Term.ArgClause(Lit.String(it.υidυ)::Nil)
                             ),
                             Term.ArgClause(expr::Nil)
                           ))
            case _ =>
              * = `_ <- *`(Term.Apply(
                             Term.Apply(Term.Apply(\(ch), Term.ArgClause(Lit.Boolean(false) :: Nil)),
                                        Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                             Term.ArgClause(Lit.String(it.υidυ)::Nil)
                           ))

        case it @ π(λ(Symbol(ch)), arg, nu @ (None | Some("ν")), r, code) =>
          val argʹ =
            nu match
              case None =>
                arg
              case _ =>
                val λ(Symbol(par)) = arg
                val parʹ = if ch == par then id else par
                * = ν(parʹ).emit
                λ(Symbol(parʹ))

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(
                                 Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: argʹ.toTerm :: Nil)),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)
                               ),
                               Term.ArgClause(expr::Nil)
                             ))
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- ZIO { * }`(term))
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(
                                 Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: argʹ.toTerm :: Nil)),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)
                               ),
                               Term.ArgClause(expr::Nil)
                             ))
            case _ =>
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: argʹ.toTerm :: Nil)),
                               Term.ArgClause(Lit.String(it.υidυ)::Nil)
                             ))

          nu match
            case None =>
            case _ =>
              val λ(Symbol(par)) = arg
              if ch == par
              then
                val λ(Symbol(parʹ)) = argʹ
                * :+= `* <- ZIO.succeed(*)`(par -> parʹ)

        case π(λ(Symbol(ch)), λ(params: List[`λ`]), Some(cons), r, code) =>
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
              * :+= `_ <- ZIO { * }`(term)
            case _ =>

        case it @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code) =>
          val par = if λ.`type`.isDefined then id else arg

          code match
            case Some((Right(term), _)) =>
              * = `* <- …`(Pat.Tuple(List(Pat.Var(par), Pat.Wildcard())),
                           Term.Apply(
                             Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(rate(r.get)::Nil)),
                               Term.ArgClause(Lit.String(it.υidυ)::Nil)
                             ),
                             Term.ArgClause(term::Nil)
                           ))
            case _ =>
              * = `* <- …`(Pat.Tuple(List(Pat.Var(par), Pat.Wildcard())),
                           Term.Apply(
                             Term.Apply(\(ch), Term.ArgClause(rate(r.get)::Nil)),
                             Term.ArgClause(Lit.String(it.υidυ)::Nil)
                           ))

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
          * = f.fold(`_ <- *`(`π-exclude`(t.enabled)): List[Enumerator])(_.emit)

          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), *, t.emit))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), t.emit, *))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // (LINEAR) REPLICATION ////////////////////////////////////////////////

        case !(parallelism, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(nu), r, code)), sum) if parallelism < -1 =>
          val par = if λ.`type`.isDefined then id else arg

          val υidυ = id

          val chʹ =
            nu match
              case "ν" => Term.Apply(\(ch), Term.ArgClause(Lit.String(nu) :: Nil))
              case _   => Term.Apply(\(ch), Term.ArgClause(Lit.Null() :: Nil))

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(
                                   Term.Apply(chʹ, Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                   Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: Nil))),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                               Term.ArgClause(expr :: Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(
                                   Term.Apply(chʹ, Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                   Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: Nil))),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                               Term.ArgClause(expr :: Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(chʹ, Term.ArgClause(Lit.Boolean(false) :: Nil)),
                                 Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: Nil))),
                               Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))

          val `val` =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `val * = *: * …`(arg, par, tpe, refined) :: Nil
              case Some((tpe, _)) =>
                `val * = *: *`(arg, par, tpe) :: Nil
              case _ => Nil

          val wrap = { (body: Term) => Term.Block(`val` :+ body) }

          * ::= `* <- *`(υidυ -> `\\.\\\\ { def *(*: ()): String ?=> UIO[Any] = …; * }`(υidυ -> par, wrap(sum.emit)))

        case !(parallelism, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), arg @ λ(_: Term), None, r, code)), sum) if parallelism < -1 =>
          val υidυ = id

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(
                                   Term.Apply(
                                     Term.Apply(\(ch), Term.ArgClause(Lit.String("*") :: Nil)),
                                     Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                   Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: arg.toTerm :: Nil))),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                               Term.ArgClause(expr :: Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(
                                   Term.Apply(
                                     Term.Apply(\(ch), Term.ArgClause(Lit.String("*") :: Nil)),
                                     Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                   Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: arg.toTerm :: Nil))),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                               Term.ArgClause(expr :: Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(
                                   Term.Apply(\(ch), Term.ArgClause(Lit.String("*") :: Nil)),
                                   Term.ArgClause(Lit.Boolean(false) :: Nil)),
                                 Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: arg.toTerm :: Nil))),
                               Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))

          * ::= `* <- *`(υidυ -> `\\.\\\\ { def *(): String ?=> UIO[Any] = …; * }`(υidυ, sum.emit))

        case !(parallelism, given Option[(Long, String)], Some(it @ π(λ(Symbol(ch)), arg, None, r, code)), sum) if parallelism < -1 =>
          val υidυ = id

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(
                                   Term.Apply(\(ch), Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                   Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: arg.toTerm :: Nil))),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                               Term.ArgClause(expr :: Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(
                                   Term.Apply(\(ch), Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                   Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: arg.toTerm :: Nil))),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                               Term.ArgClause(expr :: Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(\(ch), Term.ArgClause(Lit.Boolean(false) :: Nil)),
                                 Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: arg.toTerm :: Nil))),
                               Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))

          * ::= `* <- *`(υidυ -> `\\.\\\\ { def *(): String ?=> UIO[Any] = …; * }`(υidυ, sum.emit))

        case !(parallelism, given Option[(Long, String)], Some(it @ τ(r, code)), sum) if parallelism < -1 =>
          val υidυ = id

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(
                                   Term.Apply(\("τ"), Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                   Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: Nil))),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                               Term.ArgClause(expr :: Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(
                                   Term.Apply(\("τ"), Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                   Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: Nil))),
                                 Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                               Term.ArgClause(expr :: Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(
                             Term.Apply(
                               Term.Apply(
                                 Term.Apply(\("τ"), Term.ArgClause(Lit.Boolean(false) :: Nil)),
                                 Term.ArgClause(pace(Lit.Int(-(parallelism % Int.MaxValue)) :: rate(r.get) :: Nil))),
                               Term.ArgClause(Lit.String(it.υidυ)::Nil)),
                             Term.ArgClause(\(υidυ) :: Nil)))

          * ::= `* <- *`(υidυ -> `\\.\\\\ { def *(): String ?=> UIO[Any] = …; * }`(υidυ, sum.emit))

        // REPLICATION /////////////////////////////////////////////////////////

        case !(parallelism, pace, Some(π @ π(_, λ @ λ(Symbol(arg)), Some(_), _, _)), sum) =>
          val par = if λ.`type`.isDefined then id else arg

          val υidυ = id

          val πʹ = if λ.`type`.isDefined then π.copy(name = λ.copy()(using None))(π.υidυ) else π

          val `!.π⋯` = πʹ.emit :+ ^._1 :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(arg :: Nil)),
                                                              Term.ArgClause(^._2 :: Nil, Some(Mod.Using()))))

          val `!⋯` = pace.map(`_ <- ZIO.sleep(*.…)`(_, _) :: `!.π⋯`).getOrElse(`!.π⋯`)

          val `val` =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `val * = *: * …`(arg, par, tpe, refined) :: Nil
              case Some((tpe, _)) =>
                `val * = *: *`(arg, par, tpe) :: Nil
              case _ => Nil

          val wrap = { (body: Term) => Term.Block(`val` :+ body) }

          val sem = if parallelism < 0 then null else id

          var body =
            `List( *, … ).collectAllPar`(
              if parallelism < 0
              then sum.emit
              else sum.emit :+ `_ <- *.release`(sem),
              `!⋯`
            )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `\\.\\\\ { def *(*: ()): String ?=> UIO[Any] = …; * }`(υidυ -> par, wrap(body))) :: `!.π⋯`
          else
            body = `_ <- *.acquire`(sem) :: `_ <- *`(body)
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `\\.\\\\ { def *(*: ()): String ?=> UIO[Any] = …; * }`(υidυ -> par, wrap(body))) :: `!.π⋯`

        case !(parallelism, pace, Some(μ), sum) =>
          val υidυ = id

          val `!.μ⋯` = μ.emit :+ ^._1 :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(Nil)),
                                                             Term.ArgClause(^._2 :: Nil, Some(Mod.Using()))))

          val `!⋯` = pace.map(`_ <- ZIO.sleep(*.…)`(_, _) :: `!.μ⋯`).getOrElse(`!.μ⋯`)

          val sem = if parallelism < 0 then null else id

          var body =
            `List( *, … ).collectAllPar`(
              if parallelism < 0
              then sum.emit
              else sum.emit :+ `_ <- *.release`(sem),
              `!⋯`
            )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `\\.\\\\ { def *(): String ?=> UIO[Any] = …; * }`(υidυ, body)) :: `!.μ⋯`
          else
            body = `_ <- *.acquire`(sem) :: `_ <- *`(body)
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `\\.\\\\ { def *(): String ?=> UIO[Any] = …; * }`(υidυ, body)) :: `!.μ⋯`

        case _ : ! => ??? // caught by 'parse'

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(Definition(_, _, _, variables, _), _sum, _, pointers) =>
          * = (variables zip pointers)
            .map(_.name -> _.name)
            .map(Pat.Var(_) -> _)
            .map(Enumerator.Val(_, _))
            .toList

          val n = pointers.size

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
          case (`(*)`(_, λ(parameters: Term)), _) =>
            Defn.Val(Nil, Pat.Var("π-parameters") :: Nil, None, parameters)
      ) ::
      ( prog.tail.tail.head match
          case (`(*)`(_, λ(traces: (Lit.Null | Term))), _) =>
            Term.Assign(\("π-traces"), traces)
      ) ::
      prog
        .drop(1+2)
        .map(_ -> _.emit(using id()))
        .map(_.swap)
        .map(defn(_)(_))
