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
package akka

import scala.collection.mutable.{ ListBuffer => Listʹ, HashMap => Mapʹ, HashSet => Setʹ }

import scala.meta.*
import dialects.Scala3

import parser.Calculus.*
import akka.Meta.*


object Program:

  import Optimizer.Ref1

  extension (self: AST)(using id: => String)

    def generateʹ(using opt1: Mapʹ[String, List[String] | Ref1], opt2: Setʹ[String]): (Stat, Defn.Def) =

      self match

        case ∥(_, operand) =>
          operand.generateʹ

        case it @ `.`(?:(_, _, None)) =>
          def cases(sum: +): Either[Stat, (Term, Stat)] =
            sum match
              case +(-1, ∥(-1, `.`(?:(((lhs, rhs), mismatch), t, None)))) =>

                Right {
                  cases(t) match

                    case Left(Lit.Unit()) =>

                      `if * then … else …`(====(lhs, rhs), `Behaviors.ignore`, `Behaviors.ignore`) -> Lit.Unit()

                    case Left(defn: Defn.Def) =>

                      val term = Term.Apply(defn.name, Term.ArgClause(Nil))

                      ( if mismatch
                        then
                          `if * then … else …`(====(lhs, rhs), `Behaviors.ignore`, Term.Block(term :: Nil))
                        else
                          `if * then … else …`(====(lhs, rhs), Term.Block(term :: Nil), `Behaviors.ignore`)
                      ) -> defn

                    case Right((term, stat)) =>

                      ( if mismatch
                        then
                          `if * then … else …`(====(lhs, rhs), `Behaviors.ignore`, Term.Block(term :: Nil))
                        else
                          `if * then … else …`(====(lhs, rhs), Term.Block(term :: Nil), `Behaviors.ignore`)
                      ) -> stat
                }

              case _ =>
                given Listʹ[String]()

                sum.generate()._1 match

                  case Some(defn) =>

                    val name = defn.name.value

                    opt1.get(name) match

                      case Some(Ref1(nameʹ: String, _)) => (opt1 -= name) -= nameʹ
                      case _ =>

                    opt2 += name

                    Left(defn)

                  case _ =>

                    Left(Lit.Unit())

          val (recv, stat) = cases(`+`(-1, ∥(-1, it))).right.get

          val name = "cases" + id

          stat -> dfn(name, recv :: Nil)

    def generate(using opt1: Mapʹ[String, List[String] | Ref1], opt2: Setʹ[String])
                (using collect1: Listʹ[String])
                (implicit semaphore: Option[String] = None): (Option[Defn.Def], Int) =

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>
          None -> -1

        case it: + if it.scaling == -1 && it.choices.forall { case ∥(-1, `.`(?:(_, _, None))) => true case _ => false } =>
          val defs = it.choices.foldRight(List[(Stat, Defn.Def)]())(_.generateʹ :: _)

          val name = "sum_cases" + id

          val names = defs.map(_._2.name.value)

          opt2 ++= names

          val υidυs = names.map(_ => id)

          val stats = (υidυs zip names).map(`* = gACΠ.spawnAnonymous(…)`(_, _))

          val recv = stats :+ `List( *, … ).foreach`(υidυs*)(true)

          val defsʹ = defs.flatMap(_.productIterator.toList).asInstanceOf[List[Stat]]

          Some(dfn(name, defsʹ :+ `Behaviors.receive { case Left(it) => if it *; empty else stopped } (release?)`(recv))) -> -1

        case +(-1|1, operand) =>
          operand.generate

        case it: + =>
          given Listʹ[String]()

          val defs = it.choices.foldRight(List[Defn.Def]())(_.generate()._1.get :: _)

          val name = "sum" + id

          var names = defs.map(_.name.value)

          opt1 += name -> given_Listʹ_String.toList
          opt2 ++= names

          it.scaling match
            case -1|1 =>
            case _ =>
              names = List.fill(it.scaling)(names).reduce(_ ::: _)

          val υidυs = names.map(_ => id)

          val stats = (υidυs zip names).map(`* = gACΠ.spawnAnonymous(…)`(_, _))

          val recv = stats :+ `List( *, … ).foreach`(υidυs*)(true)

          Some(dfn(name, defs :+ `Behaviors.receive { case Left(it) => if it *; empty else stopped } (release?)`(recv))) -> -1

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(-1|1, operand) =>
          given Listʹ[String]()

          operand.generate match

            case it @ (_, -1) =>
              collect1 ++= given_Listʹ_String
              it

            case (Some(it: Defn.Def), parallelism) =>
              val name = "par" + id

              opt1 += name -> given_Listʹ_String.toList

              val υidυ = id

              val sem = "∥1" + id

              val stats = `* = Semaphore(…)`(sem, parallelism) :: `* = gACΠ.spawnAnonymous(…)`(υidυ, it.name.value, sem) :: Nil

              val recv = stats :+ `List( *, … ).foreach`(υidυ)()

              Some(dfn(name, it :: `Behaviors.receive { case Left(it) => if it *; empty else stopped }`(recv) :: Nil)) -> -1

        case it: ∥ =>
          given Listʹ[String]()

          val defs = it.components.foldRight(List[(Option[Defn.Def], Int)]())(_.generate() :: _)

          val name = "par2" + id

          opt1 += name -> given_Listʹ_String.toList

          val args = defs.flatMap {
            case (_, -1) =>
              None
            case (Some(Defn.Def(_, Term.Name(name), _, _, _, _)), parallelism) =>
              Some(name -> ("∥2" + id -> parallelism))
          }.toMap

          var names = defs.map(_._1.get.name.value)

          it.scaling match
            case -1|1 =>
            case _ =>
              names = List.fill(it.scaling)(names).reduce(_ ::: _)

          val υidυs = names.map(_ => id)

          val stats = (υidυs zip names).flatMap { (υidυ, name) =>
            args.get(name) match
              case Some((sem, parallelism)) =>
                `* = Semaphore(…)`(sem, parallelism) :: `* = gACΠ.spawnAnonymous(…)`(υidυ, name, sem) :: Nil
              case _ =>
                `* = gACΠ.spawnAnonymous(…)`(υidυ, name) :: Nil
          }

          val recv = stats :+ `List( *, … ).foreach`(υidυs*)()

          Some(dfn(name, defs.map(_._1.get) :+ `Behaviors.receive { case Left(it) => if it *; empty else stopped } (release?)`(recv))) -> -1

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, ps*) =>
          val name = "nest" + id

          var sem: String = null

          val ts = ps.foldRight(List[(Seq[String], List[Enumerator])]())(_.emit :: _)

          given Seq[String] = ts.flatMap(_._1)

          var code = ts.flatMap(_._2)
          val nsʹ =
            end match {
              case `⟦⟧`(_, _, _, _, assignment) =>
                code = code ::: assignment
                  .map(_.name -> _.name)
                  .map(Pat.Var(_) -> _)
                  .map(Enumerator.Val(_, _))
                  .toList
                assignment.map(_._1.name).toSeq
              case _ => Nil
            }
          val ns = nsʹ ++ ps.flatMap {
            case ν(names*) => names
            case π(_, λ(Symbol(arg)), Some(_), _) => Some(arg)
            case π(_, λ(params: List[`λ`]), Some(_), _) => params.map(_.asSymbol.name).filter(_.nonEmpty)
            case _ => Nil
          }
          val block =
            given Listʹ[String]()
            given List[Stat] = `Behaviors.stopped` :: Nil
            val loop = Term.Apply(\(name), Term.ArgClause(ns.map(\(_)).toList))
            end.emit(loop) { `null` ?=> (body, term) =>
              opt1 += name -> given_Listʹ_String.toList
              val recv =
                if term ne null
                then
                  val υidυ = id
                  `Behaviors.receive { case Left(it) => if it *; empty else stopped }`(`* = gACΠ.spawnAnonymous(…)`(υidυ, term) :: `* ! Left(None)`(υidυ) :: Nil)
                else
                  `null` match
                    case  (it @ Term.Apply(Term.Select(Term.Name("Behaviors"), Term.Name("receive")), _)) :: Term.Select(Term.Name("Behaviors"), _) :: Nil =>
                      it
                    case it =>
                      `Behaviors.receive { case Left(it) => if it * else stopped }`(it)
              val bodyʹ = dfn(name, Term.Block(body :+ recv), ns*)
              val recvʹ =
                if code.isEmpty
                then
                  loop
                else
                  `Behaviors.receive { case Right(it) => it case _ => pipeToSelf(*); same }`(code, (name +: ns)*)()
              Term.Block(bodyʹ :: recvʹ :: Nil)
            } match
              case Term.Tuple(Term.Name(semʹ) :: it :: Nil) => sem = semʹ; it
              case it                                       => it

          val recv = if semaphore.isDefined then Optimizer.release(using semaphore.get)(block) else block

          val defn = dfn(id, recv :: Nil)

          end match {

            case !(parallelism, _, _, _) if parallelism >= 0 =>

              val defnʹ = defn.copy(paramss = Term.ParamClause(Term.Param(Nil, sem,
                                                                          Some(Type.Name("πSem")),
                                                                          None) :: Nil) :: Nil)
              Some(defnʹ) -> parallelism

            case it: `(*)` =>

              collect1 += defn.name.value

              defn.body.asInstanceOf[Term.Block].stats.last match

                case Term.Apply(Term.Name(name), _) =>

                  opt1 += defn.name.value -> Ref1(name, code.isEmpty)
                  opt1 += name -> Ref1(it, code.isEmpty)

                case _ =>

                  opt1 += defn.name.value -> Ref1(it, code.isEmpty)

              Some(defn) -> -1

            case _ =>

              Some(defn) -> -1

          }

        //////////////////////////////////////////////////////////// sequence //

    def emit(using Mapʹ[String, List[String] | Ref1], Setʹ[String])
            (using Listʹ[String], List[Stat])
            (loop: Term)
            (make: List[Stat] ?=> (List[Stat], Term) => Term): Term =

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>

          make(Lit.Unit() :: Nil, null)

        case it: + =>

          val defn = it.generate._1.get

          make(defn :: Nil, Term.Apply(\(defn.name.value), Term.ArgClause(Nil)))

        /////////////////////////////////////////////////////////// summation //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((lhs, rhs), mismatch), t, f) =>
          val (defn, defnʹ) = t.generate._1 -> f.flatMap(_.generate._1)

          val υidυ = id

          val tʹ = defn.fold(`Behaviors.stopped` :: Nil) { it =>
            `* = gACΠ.spawnAnonymous(…)`(υidυ, Term.Apply(\(it.name.value), Term.ArgClause(Nil))) :: `* ! Left(None)`(υidυ) :: `Behaviors.empty` :: Nil
          }

          val fʹ = defnʹ.fold(`Behaviors.stopped` :: Nil) { it =>
            `* = gACΠ.spawnAnonymous(…)`(υidυ, Term.Apply(\(it.name.value), Term.ArgClause(Nil))) :: `* ! Left(None)`(υidυ) :: `Behaviors.empty` :: Nil
          }

          val `if` =
            if mismatch
            then
              `if * then … else …`(====(lhs, rhs), Term.Block(fʹ), Term.Block(tʹ))
            else
              `if * then … else …`(====(lhs, rhs), Term.Block(tʹ), Term.Block(fʹ))

          given List[Stat] = `if` :: Nil

          make(defn.getOrElse(Lit.Unit()) :: defnʹ.getOrElse(Lit.Unit()) :: Nil, null)

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(parallelism, pace, Some(π @ π(_, λ(Symbol(par)), Some("ν"), _)), sum) =>

          implicit val sem = if parallelism < 0 then None else Some(id)

          val defn = sum.generate._1.map(_.copy(paramss = List(Term.Param(Nil, \(par), Some(Type.Name("()")), None) :: Nil)))

          var `!⋯` = defn.fold(Nil) { it =>
            val υidυ = id
            `* = gACΠ.spawnAnonymous(…)`(υidυ, Term.Apply(\(it.name.value), Term.ArgClause(\(par) :: Nil))) :: `* ! Left(None)`(υidυ) :: Nil
          }

          `!⋯` :+= `self ! Left(None)`

          if pace.isDefined
          then
            `!⋯` = pace.map(`sleep(*.…)`(_, _) :: `!⋯`).get

          if parallelism >= 0
          then
            if defn.isDefined
            then
              `!⋯` = `*.acquire`(sem.get) :: `!⋯`
            else
              `!⋯` = `*.acquire`(sem.get) :: `*.release`(sem.get) :: `!⋯`

          val υidυ = id
          val υidυʹ = "emit" + id

          π.emit match

            case (given Seq[String], code) =>

              val `π.emit` = code match
                case hd :: (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
                  hd :: it.copy(pat = Pat.Var(υidυ)) :: tl

              val `yield`: Term => Term = `if * then … else …`(Term.ApplyInfix(\(υidυ), \("eq"),
                                                                               Type.ArgClause(Nil),
                                                                               Term.ArgClause(\("None") :: Nil)),
                                                               `Behaviors.stopped`,
                                                               _)

              val recvʹ = `Behaviors.receive { case _ => * }`(`!⋯` :+ loop)
              val body = dfn(υidυʹ, Term.Block(recvʹ :: Nil), par)
              val recv = `Behaviors.receive { case Right(it) => it case _ => pipeToSelf(*); same }`(`π.emit`, υidυʹ, par)(Some(`yield`))

              given List[Stat] = recv :: `Behaviors.same` :: Nil

              val term = make(defn.getOrElse(Lit.Unit()) :: body :: Nil, null)

              if parallelism < 0
              then
                term
              else
                Term.Tuple(List(\(sem.get), term))

        case !(parallelism, pace, Some(π @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), _)), sum) =>

          implicit val sem = if parallelism < 0 then None else Some(id)

          val defn = sum.generate._1.map(_.copy(paramss = List(Term.Param(Nil, \(arg), Some(Type.Name("()")), None) :: Nil)))

          var `!⋯` = defn.fold(Nil) { it =>
            val υidυ = id
            `* = gACΠ.spawnAnonymous(…)`(υidυ, Term.Apply(\(it.name.value), Term.ArgClause(\(arg) :: Nil))) :: `* ! Left(None)`(υidυ) :: Nil
          }

          `!⋯` :+= `self ! Left(None)`

          if pace.isDefined
          then
            `!⋯` = pace.map(`sleep(*.…)`(_, _) :: `!⋯`).get

          if parallelism >= 0
          then
            if defn.isDefined
            then
              `!⋯` = `*.acquire`(sem.get) :: `!⋯`
            else
              `!⋯` = `*.acquire`(sem.get) :: `*.release`(sem.get) :: `!⋯`

          val υidυ = id
          val υidυʹ = "emit" + id

          val par = if λ.`type`.isDefined then id else arg

          val πʹ = π.copy(name = λ.copy()(using None))

          val `πʹ.emit` = πʹ.emit._2

          val `val` =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `val * = *: * …`(arg, par, tpe, refined) :: Nil
              case Some((tpe, _)) =>
                `val * = *: *`(arg, par, tpe) :: Nil
              case _ => Nil

          val `yield`: Term => Term = { it => `if * then … else …`(Term.ApplyUnary("!", par),
                                                                   `Behaviors.stopped`,
                                                                   Term.Block(`val` :+ it))
                                      }

          val recvʹ = `Behaviors.receive { case _ => * }`(`!⋯` :+ loop)
          val body = dfn(υidυʹ, Term.Block(recvʹ :: Nil), par)
          val recv = `Behaviors.receive { case Right(it) => it case _ => pipeToSelf(*); same }`(`πʹ.emit`, υidυʹ, arg)(Some(`yield`))(using Nil)

          given List[Stat] = recv :: `Behaviors.same` :: Nil

          val term = make(defn.getOrElse(Lit.Unit()) :: body :: Nil, null)

          if parallelism < 0
          then
            term
          else
            Term.Tuple(List(\(sem.get), term))

        case !(parallelism, pace, Some(μ), sum) =>
          implicit val sem = if parallelism < 0 then None else Some(id)

          val defn = sum.generate._1

          var `!⋯` = defn.fold(Nil) { it =>
            val υidυ = id
            `* = gACΠ.spawnAnonymous(…)`(υidυ, Term.Apply(\(it.name.value), Term.ArgClause(Nil))) :: `* ! Left(None)`(υidυ) :: Nil
          }

          `!⋯` :+= `self ! Left(None)`

          if pace.isDefined
          then
            `!⋯` = pace.map(`sleep(*.…)`(_, _) :: `!⋯`).get

          if parallelism >= 0
          then
            if defn.isDefined
            then
              `!⋯` = `*.acquire`(sem.get) :: `!⋯`
            else
              `!⋯` = `*.acquire`(sem.get) :: `*.release`(sem.get) :: `!⋯`

          val υidυ = id
          val υidυʹ = "emit2" + id

          val `μ.emit` = μ.emit._2 match
            case (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
              it.copy(pat = Pat.Var(υidυ)) :: tl

          val `yield`: Term => Term = `if * then … else …`(Term.ApplyInfix(\(υidυ), \("eq"),
                                                                           Type.ArgClause(Nil),
                                                                           Term.ArgClause(\("None") :: Nil)),
                                                           `Behaviors.stopped`,
                                                           _)

          val recvʹ = `Behaviors.receive { case _ => * }`(`!⋯` :+ loop)
          val body = dfn(υidυʹ, Term.Block(recvʹ :: Nil))
          val recv = `Behaviors.receive { case Right(it) => it case _ => pipeToSelf(*); same }`(`μ.emit`, υidυʹ)(Some(`yield`))(using Nil)

          given List[Stat] = recv :: `Behaviors.same` :: Nil

          val term = make(defn.getOrElse(Lit.Unit()) :: body :: Nil, null)

          if parallelism < 0
          then
            term
          else
            Term.Tuple(List(\(sem.get), term))

        case !(parallelism, pace, _, sum) =>

          implicit val sem = if parallelism < 0 then None else Some(id)

          val defn = sum.generate._1

          var `!⋯` = defn.fold(Nil) { it =>
            val υidυ = id
            `* = gACΠ.spawnAnonymous(…)`(υidυ, Term.Apply(\(it.name.value), Term.ArgClause(Nil))) :: `* ! Left(None)`(υidυ) :: Nil
          }

          `!⋯` :+= `self ! Left(None)`

          if pace.isDefined
          then
            `!⋯` = pace.map(`sleep(*.…)`(_, _) :: `!⋯`).get

          if sem.isDefined
          then
            if defn.isDefined
            then
              `!⋯` = `*.acquire`(sem.get) :: `!⋯`
            else
              `!⋯` = `*.acquire`(sem.get) :: `*.release`(sem.get) :: `!⋯`

          given List[Stat] = `!⋯` :+ `Behaviors.same`

          val term = make(defn.getOrElse(Lit.Unit()) :: Nil, null)

          if parallelism < 0
          then
            term
          else
            Term.Tuple(List(\(sem.get), term))

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(_, variables, _sum, _, assignment) =>
          val n = assignment.size

          val sum = if (variables.size == n)
                    then
                      _sum
                    else
                      `+`(-1, ∥(-1, `.`(_sum, ν(variables.drop(n).map(_.name).toSeq*))))

          sum.emit(null)(make)

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //

        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, qual, params*) =>
          val args = params.map(_.toTerm).toList

          val term = qual match
            case h :: t => (t.map(\(_)) :+ \("π") :+ \(identifier)).foldLeft(h: Term)(Term.Select(_, _))
            case _ => \(identifier)

          make(Lit.Unit() :: Nil, Term.Apply(term, Term.ArgClause(args)))

        ////////////////////////////////////////////////////////// invocation //

  extension (self: Pre)(using id: => String)

    def emit: (Seq[String], List[Enumerator]) =

      var ** : Seq[String] = Nil

      var * = List[Enumerator]()

      self match

        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          ** = names.map(_ => id)
          * = names.zipWithIndex.map { (it, i) => `* = *`(it -> **(i)) }.toList

        case τ(Some((Left(enums), _))) =>
          * :+= `_ <- *`("τ")
          * :::= enums

        case τ(Some((Right(term), _))) =>
          * :+= `_ <- *`("τ")
          * :+= `_ <- Future { * }`(term)

        case τ(_) =>
          * = `_ <- *`("τ")


        case π(λ(Symbol(ch)), arg, nu @ (None | Some("ν")), code) =>
          nu match
            case None =>
            case _ =>
              val λ(Symbol(par)) = arg
              val (ns, ls) = ν(par).emit
              ** = ns
              * = ls

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(arg.toTerm::Nil)),
                               Term.ArgClause(expr::Nil)
                             ))
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- Future { * }`(term))
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
              * :+= `_ <- Future { * }`(term)
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

      (**, *)


  final class Main(opt: Int):
    require(0 <= opt && opt <= 2)

    import Optimizer.*

    def apply(prog: List[Bind]): List[String] =
      val id = new helper.υidυ
      given opt1: Mapʹ[String, List[String] | Ref1] = Mapʹ()
      given opt2: Setʹ[String] = Setʹ()
      prog
        .map {
          case (bind, ∅()) =>
            opt2 += bind.identifier
            dfn(Nil, Nil)(bind)
          case (bind, sum) =>
            given Listʹ[String]()
            val defn = sum.generate(using id())._1.get
            val υidυ = id()
            val recv =
              `* = gACΠ.spawnAnonymous(…)`(υidυ, defn.name.value) ::
              `* ! Left(None)`(υidυ) :: Nil
            opt1(bind.identifier) = given_Listʹ_String.toList
            opt2 += bind.identifier
            dfn(defn :: Nil, recv)(bind)
        }
        .flatMap { it => if opt > 0 then it.optimize1._1 else Some(it) }
        .map { it => if opt > 1 then it.optimize2._1 else it }
        .map(_.toString)
