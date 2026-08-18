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

package basc
package emitter
package ziof

import scala.meta.*
import dialects.Scala3

import parser.BioAmbients.Actions
import parser.Calculus.*
import parser.Encoding.*
import parser.μ
import zio.Program.{ emit => zioemit }
import ziof.Meta.*


object Program:

  /**
    * Phantoms help to avoid `flatMap`s, for example:
    *
    * `!.a<0>. a<1>.a<2>. (a<3>.(a<4>. | a<5>.) + a<6>.)`
    *
    * here, 1, 4 or 5 dont need to become `flatMap`s.
    *
    * Phantoms only exist in this emitter's lifetime.
    */
  private lazy val phantom: τ = τ(Some(null), None)(null)

  extension (self: Pre)(using id: => String)

    def emit(* : List[Enumerator]): Term =

      implicit val υidυ = id

      self match

        // PREFIXES ////////////////////////////////////////////////////////////

        case it @ τ(r, Some((Left(enums), _))) =>
          `*.flatMap { null else … }`(Term.Apply(
                                        Term.Apply(\("τ"),
                                                   Term.ArgClause(rate(r.get) :: Nil)),
                                        Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: Nil)),
                                      enums ::: *)

        case it @ τ(r, Some((Right(term), _))) =>
          `*.flatMap { null else … }`(Term.Apply(
                                        Term.Apply(\("τ"),
                                                   Term.ArgClause(rate(r.get) :: Nil)),
                                        Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: Nil)),
                                      `_ <- ZIO { * }`(term) :: *)

        case it @ τ(r, _) =>
          `*.flatMap { null else … }`(Term.Apply(
                                        Term.Apply(\("τ"),
                                                   Term.ArgClause(rate(r.get) :: Nil)),
                                        Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: Nil)),
                                      *)


        case it @ π(dir, λ(Symbol(ch)), arg @ λ(_: Term), None, r, code) =>
          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              `*.flatMap { null else … }`(Term.Apply(
                                            Term.Apply(
                                              Term.Apply(
                                                Term.Apply(\(ch), Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                                Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                              Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$dir") :: Nil)
                                            ),
                                            Term.ArgClause(expr::Nil)),
                                          *)
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- ZIO { * }`(term))
              `*.flatMap { null else … }`(Term.Apply(
                                            Term.Apply(
                                              Term.Apply(
                                                Term.Apply(\(ch), Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                                Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                              Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$dir") :: Nil)
                                            ),
                                            Term.ArgClause(expr::Nil)),
                                          *)
            case _ =>
              `*.flatMap { null else … }`(Term.Apply(
                                            Term.Apply(
                                              Term.Apply(\(ch), Term.ArgClause(Lit.Boolean(true) :: Nil)),
                                              Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                            Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$dir") :: Nil)),
                                          *)

        case it @ π(dir, λ(Symbol(ch)), arg, None, r, code) =>
          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              `*.flatMap { null else … }`(Term.Apply(
                                            Term.Apply(
                                              Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                              Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$dir") :: Nil)
                                            ),
                                            Term.ArgClause(expr::Nil)),
                                          *)
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- ZIO { * }`(term))
              `*.flatMap { null else … }`(Term.Apply(
                                            Term.Apply(
                                              Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                              Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$dir") :: Nil)
                                            ),
                                            Term.ArgClause(expr::Nil)),
                                          *)
            case _ =>
              `*.flatMap { null else … }`(Term.Apply(
                                            Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: arg.toTerm :: Nil)),
                                            Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$dir") :: Nil)),
                                          *)

        case it @ π(_, λ(Symbol(ch)), λ(Symbol(par)), Some("ν"), _, _) =>
          val parʹ = if ch == par then id else par
          val ** = if ch == par then `* <- ZIO.succeed(*)`(par -> parʹ) else `_ <- \\.unit`
          `for * yield ()`(
            `* <- *`(parʹ -> "ν"),
            `_ <- *`(it.copy(name = λ(Symbol(parʹ)), polarity = None)(it.υidυ).emit(** :: *))
          )

        case it @ π(dir, λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), r, code) =>
          val par = if λ.`type`.isDefined then id else arg

          val ** =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `* = *: * …`(arg, par, tpe, refined) :: *
              case Some((tpe, _)) =>
                `* = *: *`(arg, par, tpe) :: *
              case _ =>
                *

          code match
            case Some((Right(term), _)) =>
              `*.flatMap { null else … }`(Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: Nil)),
                                                                Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$dir") :: Nil)), Term.ArgClause(term::Nil)),
                                          **)(par)

            case _ =>
              `*.flatMap { null else … }`(Term.Apply(Term.Apply(\(ch), Term.ArgClause(rate(r.get) :: Nil)),
                                                     Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$dir") :: Nil)),
                                          **)(par)

        case it @ ζ(cap, name, _, r, code) =>
          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              `*.flatMap { null else … }`(Term.Apply(
                                            Term.Apply(
                                              Term.Apply(\(name), Term.ArgClause(rate(r.get) :: Nil)),
                                              Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$cap") :: Nil)),
                                            Term.ArgClause(expr::Nil)),
                                          *)
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- ZIO { * }`(term))
              `*.flatMap { null else … }`(Term.Apply(
                                            Term.Apply(
                                              Term.Apply(\(name), Term.ArgClause(rate(r.get) :: Nil)),
                                              Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$cap") :: Nil)),
                                            Term.ArgClause(expr::Nil)),
                                          *)
            case _ =>
              `*.flatMap { null else … }`(Term.Apply(
                                            Term.Apply(\(name), Term.ArgClause(rate(r.get) :: Nil)),
                                            Term.ArgClause(Lit.String(it.υidυ) :: \(")(") :: \(s"π-$cap") :: Nil)),
                                          *)

        case _ => ??? // caught by parser

        //////////////////////////////////////////////////////////// prefixes //


  extension (self: AST)(using id: => String, ^ : (Enumerator.Generator, Term.Name))

    /** Called on behalf of a guarded replication definitely not discarded:
      * emulate the replication guard with a phantom τ in each composition,
      * eventually dropping head phantoms from these sequences of prefixes;
      * obviously, it does not apply to sums with zero or multiple choices.
      * Note: also used while emitting a leaf, otherwise `false` is a noop.
      */
    def emit0: List[Enumerator] =

      self match

        case +(-1, ∥(-1, ss*)) =>

          `+`(-1, ∥(-1, ss.map(it => it.copy(prefixes = phantom +: it.prefixes))*)).emit(false)

        case _ =>
          self.emit(false)

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
                sum.emit()

          `_ <- *`(cases(`+`(-1, ∥(-1, it))))

        case _ => ???

    /**
      * @param flatMap whether to emit the guard with a "`flatMap`" or not,
      * if the proximal leaf is a replication; it is meant to fall through
      * an AST node like `+(_, ∥(_, .(!)))`, otherwise being reset to `true`.
      */
    def emit(flatMap: Boolean = true): List[Enumerator] =

      var * = List[Enumerator]()

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>

        case +(_, operand) =>
          * = operand.emit(flatMap)

        case it: + if it.scaling == -1 && it.choices.forall { case ∥(-1, `.`(?:(_, _, None))) => true case _ => false } =>
          val ios = it.choices.foldRight(List[Term]())(_.emitʹ :: _)

          * = `_ <- *`(`List( *, … ).collectAllPar`(ios*))

        case it: + =>
          val ios = it.choices.foldRight(List[Term]())(_.emit() :: _)

          * = `_ <- *`(`List( *, … ).collectAllPar`(ios*))

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(_, operand) =>
          * = operand.emit(flatMap)

        case it: ∥ =>
          val ios = it.components.foldRight(List[Term]())(_.emit(flatMap) :: _)

          * = `_ <- *`(`List( *, … ).collectAllPar`(ios*))

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, ps*) =>

          val υidυ = Actions(ps*).headOption

          val endʹ =
            υidυ match
              case None => end.emit(flatMap)
              case _    => end.emit0

          * = ps.foldRight(endʹ) {

            case (ν(names*), ios) =>
              names.map { it => `* <- *`(it -> "ν") }.toList ::: ios

            case (π(dir, λ(Symbol(ch)), λ(params: List[`λ`]), Some(cons), _, code), ios) =>
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

              * ::: ios

            case (it: τ, uios) if it eq phantom => // drop it
              uios

            case (it: μ, ios) if υidυ.get eq it.υidυ =>
              `_ <- *`(it.emit(ios))

            case (it, ios) =>
              it.zioemit ::: ios

          }

        //////////////////////////////////////////////////////////// sequence //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((lhs, rhs), mismatch), t, f) =>
          * = f.fold(`_ <- *`(`π-exclude`(t.enabled)): List[Enumerator])(_.emit())

          if mismatch
          then
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), *, t.emit()))
          else
            * = `_ <- *`(`if * then … else …`(====(lhs, rhs), t.emit(), *))

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(parallelism, pace, Some(π @ π(_, _, λ @ λ(Symbol(arg)), Some(_), _, _)), sum) =>
          val par = if λ.`type`.isDefined then id else arg

          val υidυ = id

          val πʹ = if λ.`type`.isDefined then π.copy(name = λ.copy()(using None))(π.υidυ) else π

          val `!.π⋯` =
            if flatMap
            then
              `_ <- *` { πʹ.emit { ^._1 :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(arg :: Nil)),
                                                               Term.ArgClause(^._2 :: Nil, Some(Mod.Using()))))
                                 }
                       } :: Nil
            else
              πʹ.zioemit :+ ^._1 :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(par :: Nil)),
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
              then sum.emit0
              else sum.emit0 :+ `_ <- *.release`(sem),
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

          val `!.μ⋯` =
            if flatMap
            then
              `_ <- *` { μ.emit { ^._1 :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(Nil)),
                                                              Term.ArgClause(^._2 :: Nil, Some(Mod.Using()))))
                                }
                       } :: Nil
            else
              μ.zioemit :+ ^._1 :+ `_ <- *`(Term.Apply(Term.Apply(\(υidυ), Term.ArgClause(Nil)),
                                                       Term.ArgClause(^._2 :: Nil, Some(Mod.Using()))))

          val `!⋯` = pace.map(`_ <- ZIO.sleep(*.…)`(_, _) :: `!.μ⋯`).getOrElse(`!.μ⋯`)

          val sem = if parallelism < 0 then null else id

          var body =
            `List( *, … ).collectAllPar`(
              if parallelism < 0
              then sum.emit0
              else sum.emit0 :+ `_ <- *.release`(sem),
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


        // AMBIENT /////////////////////////////////////////////////////////////

        case `[]`(label, sum) =>
          val labelʹ = label
            .map { it => Term.Apply(\("Some"), Term.ArgClause(Lit.String(it) :: Nil)) }
            .getOrElse(\("None"))

          * = `_ <- *`(Term.Apply(Term.Select(\("}{"), \("}{")), Term.ArgClause(\(")(") :: labelʹ :: Nil)))

          * = `_ <- *`(`List( *, … ).collectAllPar`(* ::: sum.emit()))

        ///////////////////////////////////////////////////////////// ambient //


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

          * = * ::: sum.emit()

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, params*) =>
          val args = params.map(_.toTerm).toList

          * = ^._1 :: `_ <- *`(Term.Apply(Term.Apply(Term.Apply(\(identifier), Term.ArgClause(\(")(") :: Nil)), Term.ArgClause(args)),
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
          case (`(*)`(_, λ(parallelism: Lit.Int)), _) =>
            Defn.Val(Nil, Pat.Var("π-parallelism") :: Nil, None, parallelism)
      ) ::
      ( prog.tail.tail.head match
          case (`(*)`(_, λ(Term.Tuple(List(threshold: Lit.Int, _)))), _) =>
            Defn.Val(Nil, Pat.Var("π-batch-threshold") :: Nil, None, threshold)
      ) ::
      ( prog.tail.tail.head match
          case (`(*)`(_, λ(Term.Tuple(List(_, timeout: Lit.Int)))), _) =>
            Defn.Val(Nil, Pat.Var("π-batch-timeout") :: Nil, None, timeout)
      ) ::
      ( prog.tail.tail.tail.head match
          case (`(*)`(_, λ(snapshot: Lit.Boolean)), _) =>
            Defn.Val(Nil, Pat.Var("π-snapshot") :: Nil, None, snapshot)
      ) ::
      prog
        .drop(1+3)
        .map(_ -> _.emit(using id())())
        .map(_.swap)
        .map(defn(_)(_))
