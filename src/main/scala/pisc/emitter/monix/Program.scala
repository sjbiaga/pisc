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
package monix

import scala.meta.*
import dialects.Scala3

import parser.Calculus.*
import monix.Meta.*


object Program:

  private def pace(args: List[Term])(using pace: Option[(Long, String)]) =
    pace match
      case Some((time, unit)) => Term.Select(Lit.Long(time), unit) :: args
      case _ => args

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
                `* <- Stream.evalF(*)`(υidυ -> Term.Select(semaphore.get, "tryAcquire")) ::
                Enumerator.Generator(`* <- …`(), `if * then … else …`(υidυ, sum.emit, Nil))

          `_ <- *`(cases(`+`(-1, ∥(-1, it))))

        case _ => ???

    def emit(implicit _pace: Option[(Long, String)] = None): List[Enumerator] =

      var * = List[Enumerator]()

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>

        case +(_, operand) =>
          * = operand.emit

        case it: + if it.scaling == -1 && it.choices.forall { case ∥(-1, `.`(?:(_, _, None))) => true case _ => false } =>
          implicit val sem = Some(id)

          val ifs = it.choices.foldRight(List[Term]())(_.emitʹ :: _)

          * = List(
            `* <- Semaphore(…)`(sem.get),
            `_ <- *`(`Observable( *, … ).mapParF`(ifs*))
          )

        case it: + =>
          val ifs = it.choices.foldRight(List[Term]())(_.emit :: _)

          val sem = id

          * = List(
            `* <- Semaphore(…)`(sem),
            `_ <- *`(`Observable( *, … ).mapParF(…)`(ifs*)(sem))
          )

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(_, operand) =>
          * = operand.emit

        case it: ∥ =>
          val ifs = it.components.foldRight(List[Term]())(_.emit :: _)

          * = `_ <- *`(`Observable( *, … ).mapParF`(ifs*))

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, it*) =>
          * = (it :+ end).foldLeft(*)(_ ::: _.emit)

        //////////////////////////////////////////////////////////// sequence //


        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> `*[F]`("ν")) }.toList

        case τ(Some((Left(enums), _))) =>
          * = `_ <- *`(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(pace(Nil))))
          * = * ::: `Stream.evalF(…)`(enums)

        case τ(Some((Right(term), _))) =>
          val expr = term
          * = `_ <- *`(Term.Apply(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(pace(Nil))), Term.ArgClause(expr :: Nil)))

        case τ(_) =>
          * = `_ <- *`(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(pace(Nil))))


        case π(λ(Symbol(ch)), Some(nu @ "ν"), code, params*) =>

          val args = params.map(_.asSymbol.name)
          val arity = Lit.Int(args.size)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(ch, nu), Term.ArgClause(arity :: pace(Nil))),
                                                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(ch, nu), Term.ArgClause(arity :: pace(Nil))),
                                                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Select(ch, nu), Term.ArgClause(arity :: pace(Nil))))

        case π(λ(Symbol(ch)), None, code, args*) if args.forall { case λ(Lit.Null()) => true case _ => false } =>

          val arity = Lit.Int(args.size)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "null"), Term.ArgClause(arity :: pace(Nil))),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(ch, "null"), Term.ArgClause(arity :: pace(Nil))),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(ch, "null"), Term.ArgClause(arity :: pace(Nil))))

        case π(λ(Symbol(ch)), None, code, args*) if args.forall { case λ(_: Term) => true case _ => false } =>

          val argc = Term.ArgClause(pace(args.map(_.toTerm).toList))

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

          val argc = Term.ArgClause(pace(args.map(_.toTerm).toList))

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
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(\(ch), Term.ArgClause(pace(Nil))),
                                                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(\(ch), Term.ArgClause(pace(Nil))))

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
              * :+= `_ <- Stream.evalF(*)`(term)
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

        case !(1, given Option[(Long, String)], Some(π(λ(Symbol(ch)), Some(nu @ "ν"), code, params*)), sum) =>

          val args = params.map(_.asSymbol.name)
          val arity = Lit.Int(args.size)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(arity :: pace(Nil))),
                                                                        Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(arity :: pace(Nil))),
                                                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Select(Term.Select(ch, "!"), nu), Term.ArgClause(arity :: pace(Nil))))

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(π(λ(Symbol(ch)), Some(_), code, params*)), sum) =>

          val args = params.map {
            case λ @ λ(Symbol(_)) if λ.`type`.isDefined => id
            case λ(Symbol(par)) => par
          }

          code match
            case Some((Right(term), _)) =>
              val expr = term
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Apply(Term.Select(ch, "!"), Term.ArgClause(pace(Nil))),
                                                                        Term.ArgClause(expr :: Nil)))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*), Term.Apply(Term.Select(ch, "!"), Term.ArgClause(pace(Nil))))

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

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(π(λ(Symbol(ch)), None, code, args*)), sum) if args.forall { case λ(Lit.Null()) => true case _ => false } =>

          val arity = Lit.Int(args.size)

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "null"), Term.ArgClause(arity :: pace(Nil))),
                                      Term.ArgClause(expr :: Nil)))
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Select(ch, "!"), "null"), Term.ArgClause(arity :: pace(Nil))),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Select(ch, "!"), "null"), Term.ArgClause(arity :: pace(Nil))))

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(π(λ(Symbol(ch)), None, code, args*)), sum) if args.forall { case λ(_: Term) => true case _ => false } =>

          val argc = Term.ArgClause(pace(args.map(_.toTerm).toList))

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

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(π(λ(Symbol(ch)), None, code, args*)), sum) =>

          val argc = Term.ArgClause(pace(args.map(_.toTerm).toList))

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

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], Some(τ(code)), sum) =>

          code match
            case Some((Left(enums), _)) =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(pace(Nil))))
              * = * ::: `Stream.evalF(…)`(enums)
            case Some((Right(term), _)) =>
              val expr = term
              * = `_ <- *`(Term.Apply(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(pace(Nil))),
                                      Term.ArgClause(expr :: Nil)))
            case _ =>
              * = `_ <- *`(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(pace(Nil))))

          * = * ::: sum.emit()

        case !(1, given Option[(Long, String)], _, sum) =>

          * = `_ <- *`(Term.Apply(Term.Select(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), "!"), Term.ArgClause(pace(Nil))))

          * = * ::: sum.emit()

        case !(parallelism, given Option[(Long, String)], Some(π(λ(Symbol(ch)), Some(nu), code, params*)), sum) =>
          val args = params.map {
            case λ @ λ(Symbol(_)) if λ.`type`.isDefined => id
            case λ(Symbol(par)) => par
          }

          val υidυ = id

          val πʹ = π(λ(Symbol(ch)), Some(nu), code, params.map(_.copy()(using None))*)

          var `!.π⋯` = πʹ.emit :+ `_ <- *`(Term.Apply(\(υidυ), Term.ArgClause(pace(params.map(_.asSymbol.name).map(\(_)).toList))))

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

          val `!⋯` =
            if parallelism < 0
            then
              Term.Block(`val` :+
                         `Observable( *, … ).mapParF`(
                           sum.emit(),
                           `!.π⋯`
                         ))
            else
              `!.π⋯` = `_ <- *.acquire`(sem) :: `!.π⋯`
              Term.Block(`val` :+
                         `Observable( *, … ).mapParF`(
                           sum.emit() :+ `_ <- *.release`(sem),
                           `!.π⋯`
                         ))

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `\\.\\\\\\ { def *(*: ()[F], ⋯): \\[F, Unit] = …; * }`(υidυ, `!⋯`, args*)) :: `!.π⋯`
          else
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `\\.\\\\\\ { def *(*: ()[F], ⋯): \\[F, Unit] = …; * }`(υidυ, `!⋯`, args*)) :: `!.π⋯`

        case !(parallelism, given Option[(Long, String)], Some(μ), sum) =>
          val υidυ = id

          var `!.μ⋯` = μ.emit :+ `_ <- *`(υidυ)

          val sem = if parallelism < 0 then null else id

          val `!⋯` =
            if parallelism < 0
            then
              `Observable( *, … ).mapParF`(
                sum.emit(),
                `!.μ⋯`
              )
            else
              `!.μ⋯` = `_ <- *.acquire`(sem) :: `!.μ⋯`
              `Observable( *, … ).mapParF`(
                sum.emit() :+ `_ <- *.release`(sem),
                `!.μ⋯`
              )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `\\.\\\\\\ { lazy val *: \\[F, Unit] = …; * }`(υidυ, `!⋯`)) :: `!.μ⋯`
          else
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `\\.\\\\\\ { lazy val *: \\[F, Unit] = …; * }`(υidυ, `!⋯`)) :: `!.μ⋯`

        case !(parallelism, given Option[(Long, String)], _, sum) =>
          val υidυ = id

          var `!.⋯` = `_ <- *`(Term.Apply(Term.Apply(`*[F]`("τ"), Term.ArgClause(Nil)), Term.ArgClause(pace(Nil)))) :: `_ <- *`(υidυ)

          val sem = if parallelism < 0 then null else id

          val `!⋯` =
            if parallelism < 0
            then
              `Observable( *, … ).mapParF`(
                sum.emit(),
                `!.⋯`
              )
            else
              `!.⋯` = `_ <- *.acquire`(sem) :: `!.⋯`
              `Observable( *, … ).mapParF`(
                sum.emit() :+ `_ <- *.release`(sem),
                `!.⋯`
              )

          if parallelism < 0
          then
            * = `* <- *`(υidυ -> `\\.\\\\\\ { lazy val *: \\[F, Unit] = …; * }`(υidυ, `!⋯`)) :: `!.⋯`
          else
            * = `* <- Semaphore(…)`(sem, parallelism) ::
                `* <- *`(υidυ -> `\\.\\\\\\ { lazy val *: \\[F, Unit] = …; * }`(υidυ, `!⋯`)) :: `!.⋯`

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

    def apply(prog: List[Bind]): List[Stat] =
      val id = new helper.υidυ

      given Set[String] =
        prog.head match
          case (`(*)`(_, _, λ(_: Lit.Null)), _) => Set("Concurrent", "ContextShift", "Timer", "TaskLift", "TaskLike")
          case (`(*)`(_, _, λ(typeclasses: Term.Tuple)), _) =>
            typeclasses.args.map { case Term.Name(it) => it }.toSet

      val Array(tpe, _path*) = F.split('.').reverse
      val path = _path.reverse.map(\(_)).foldLeft("_root_": Term)(Term.Select(_, _))

      Defn.Type(Nil, \\("F"), Type.ParamClause(Nil),
                Type.Select(path.asInstanceOf[Term.Select], \\(tpe)),
                Type.Bounds(None, None, Nil, Nil))
      ::
      prog
        .drop(1)
        .map(_ -> _.emit(using id()))
        .map(_.swap)
        .map(defn(_)(_))
