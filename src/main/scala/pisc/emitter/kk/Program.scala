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
package kk

import scala.collection.mutable.{ ListBuffer => Listʹ, LinkedHashMap => Mapʹ, HashSet => Setʹ }

import scala.meta.*
import dialects.Scala3

import parser.-
import parser.Calculus.*
import kk.Meta.*


object Program:

  import Optimize.Opt

  extension (self: AST)(using id: => String)

    def generateʹ(using opt: Opt): (Stat, Defn.Def) =

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

                    opt._1.get(name) match

                      case Some(nameʹ: String) => (opt._1 -= name) -= nameʹ
                      case _ =>

                    opt._2 += name

                    Left(defn)

                  case _ =>

                    Left(Lit.Unit())

          val (recv, stat) = cases(`+`(-1, ∥(-1, it))).right.get

          val name = "cases" + id

          stat -> dfn(name, recv :: Nil)

    def generate(using opt: Opt, collect1: Listʹ[String])
                (implicit semaphore: Option[String] = None): (Option[Defn.Def], Int) =

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>
          None -> -1

        case +(-1, it*) if it.forall { case ∥(-1, `.`(?:(_, _, None))) => true case _ => false } =>
          val defs = it.foldRight(List[(Stat, Defn.Def)]())(_.generateʹ :: _)

          val name = "sum_cases" + id

          val names = defs.map(_._2.name.value)

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

          opt._1 += name -> given_Listʹ_String.toList
          opt._2 ++= names

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

              opt._1 += name -> given_Listʹ_String.toList

              val υidυ = id

              val sem = "sem1" + id

              val stats = `* = Semaphore(…)`(sem, parallelism) :: `* = gACΠ.spawnAnonymous(…)`(υidυ, it.name.value, sem) :: Nil

              val recv = stats :+ `List( *, … ).foreach`(υidυ)()

              Some(dfn(name, it :: `Behaviors.receive { case Left(it) => if it *; empty else stopped }`(recv) :: Nil)) -> -1

        case it: ∥ =>
          given Listʹ[String]()

          val defs = it.components.foldRight(List[(Option[Defn.Def], Int)]())(_.generate() :: _)

          val name = "par2" + id

          opt._1 += name -> given_Listʹ_String.toList

          val args = defs.flatMap {
            case (_, -1) =>
              None
            case (Some(Defn.Def(_, Term.Name(name), _, _, _, _)), parallelism) =>
              Some(name -> ("sem2" + id -> parallelism))
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
          val ts = ps.foldRight(List[(Seq[String], List[Enumerator])]())(_.emit :: _)

          given Seq[String] = ts.flatMap(_._1)

          var code = ts.flatMap(_._2)
          var ns = ps.flatMap {
            case ν(names*) => names
            case π(_, Some(_), _, params*) => params.filter(_.isSymbol).map(_.asSymbol.name).filterNot(_.isEmpty)
            case _ => Nil
          }.toSet.toSeq

          end match {
            case `⟦⟧`(_, _, _, _, assignment) =>
              code ++= assignment
                .map(_.name -> _.name)
                .map(Pat.Var(_) -> _)
                .map(Enumerator.Val(_, _))
              ns ++= assignment.map(_._1.name)
            case _ =>
          }

          var block: Term.Block = null

          implicit var sem: Option[String] = None

          given Listʹ[String]()

          val name = "scheme_" + end.ordinal + "_" + code.size + id

          val behavior =
            end match {
              case !(_, _, Some(_), _) => "thunk" + id
              case _ => "_"
            }

          end.scheme(behavior) { (stats, |) => semʹ ?=>
            require(stats.nonEmpty)

            val recv =
              | match
                case spawn: Term.Apply =>
                  val υidυ = id
                  val body = `* = gACΠ.spawnAnonymous(…)`(υidυ, spawn) :: `* ! Left(None)`(υidυ) :: Nil
                  `Behaviors.receive { case Left(it) => if it *; empty else stopped }`(body)
                case (it @ Term.Apply(Term.Select(Term.Name("Behaviors"), Term.Name("receive")), _)) :: Nil => it
                case body: List[Stat] =>
                  `Behaviors.receive { case Left(it) => if it * else stopped }`(body)

            val thunk = Term.Apply(\(name), Term.ArgClause(ns.map(\(_)).toList))

            val statsʹ =
              end match {
                case !(_, _, Some(_), _) =>
                  dfn(name, Term.Block(`def * = …`(behavior, thunk) :: (stats :+ recv)), ns*) :: Nil
                case _ =>
                  dfn(name, Term.Block(stats :+ recv), ns*) :: Nil
              }

            val recvʹ =
              if code.isEmpty
              then
                thunk
              else
                `Behaviors.receive { case Right(it) => it case Left(it) => if it pipeToSelf(*); same else stopped }`(code, thunk)

            block = Term.Block(statsʹ :+ recvʹ)
            sem = semʹ
          }

          opt._1 += name -> given_Listʹ_String.toList

          val recv = if semaphore.isDefined then release(using semaphore.get)(block) else block

          val defn = dfn("seq" + id, recv :: Nil)

          end match {

            case !(parallelism, _, _, _) if sem.isDefined =>

              val defnʹ = defn.copy(paramss = Term.ParamClause(Term.Param(Nil, sem.get,
                                                                          Some(Type.Name("πSem")),
                                                                          None) :: Nil) :: Nil)
              Some(defnʹ) -> parallelism

            case it: `(*)` if code.isEmpty =>

              collect1 += defn.name.value

              (opt._1 += defn.name.value -> name) += name -> it

              Some(defn) -> -1

            case _ =>

              Some(defn) -> -1

          }

        //////////////////////////////////////////////////////////// sequence //

  extension (self: + | -)(using id: => String)

    def scheme(using Opt, Option[String], Listʹ[String])
              (behavior: Term.Name)
              (callback: (List[Stat], Term.Apply | List[Stat]) => Option[String] ?=> Unit): Unit =

      extension (defn: Option[Defn.Def])

        private def spawn(stats: Stat*)(param: Term.Name*)(statsʹ: Stat*) =
          defn
            .map(_.name.value).map(\(_))
            .map(Term.Apply(_, Term.ArgClause(param.toList)))
            .fold(stats.toList) { it =>
              val υidυ = id
              `* = gACΠ.spawnAnonymous(…)`(υidυ, it) :: `* ! Left(None)`(υidυ) :: statsʹ.toList
            }

        def spawning(param: Term.Name*)(implicit sem: Option[String]) =
          self match
            case !(_, pace, Some(_), _) =>
              var `!⋯` = spawn()(param*)() :+ `self ! Left(None)` :+ behavior

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

              `!⋯`

            case !(_, pace, _, _) =>
              var `!⋯` = spawn()()() :+ `self ! Left(None)` :+ `Behaviors.same`

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

              `!⋯`

            case _ =>
              spawn(`Behaviors.stopped`)()(`Behaviors.empty`)

      extension (self: List[Enumerator])

        def pipeToSelf(defnʹ: Option[Defn.Def], param: Term.Name*)
                      (`yield`: Term => Term)
                      (using Seq[String])
                      (using Option[String]) =

          val recvʹ = `Behaviors.receive { case _ => * }`(defnʹ.spawning(param*))
          val υidυ = "pipe" + id
          val defn = dfn(υidυ, Term.Block(recvʹ :: Nil), param.map(_.value)*)
          val thunk = Term.Apply(\(υidυ), Term.ArgClause(param.toList))
          val recv = `Behaviors.receive { case Right(it) => it case Left(it) => if it pipeToSelf(*); same else stopped }`(self, `yield`(thunk))

          callback(defnʹ.getOrElse(Lit.Unit()) :: defn :: Nil, recv :: Nil)

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>
          callback(Lit.Unit() :: Nil, `Behaviors.stopped` :: Nil)

        case it: + =>
          val defn = it.generate._1.get

          callback(defn :: Nil, Term.Apply(\(defn.name.value), Term.ArgClause(Nil)))

        /////////////////////////////////////////////////////////// summation //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((lhs, rhs), mismatch), t, f) =>
          val (tʹ, fʹ) = t.generate._1 -> f.flatMap(_.generate._1)

          val (tʹʹ, fʹʹ) = tʹ.spawning() -> fʹ.spawning()

          val `if` =
            if mismatch
            then
              `if * then … else …`(====(lhs, rhs), Term.Block(fʹʹ), Term.Block(tʹʹ))
            else
              `if * then … else …`(====(lhs, rhs), Term.Block(tʹʹ), Term.Block(fʹʹ))

          val defs = (tʹ zip fʹ).map(_ :: _ :: Nil).orElse(tʹ.orElse(fʹ).map(_ :: Nil))

          callback(defs.getOrElse(Lit.Unit() :: Nil), `if` :: Nil)

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(parallelism, _, Some(π @ π(λ(Symbol(ch)), Some("ν"), _, params*)), sum) =>
          implicit val sem = if parallelism < 0 then None else Some(id)

          val paramsʹ = params.filter(_.isSymbol).map(_.asSymbol.name).map(\(_))

          val defn = sum.generate._1.map(_.copy(paramss = List(paramsʹ.map(Term.Param(Nil, _, Some(Type.Name("()")), None)).toList)))

          π.emit match

            case (given Seq[String], enums) =>
              val υidυ = id

              val `π.emit` = enums match
                case hd :: (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
                  hd :: it.copy(pat = Pat.Var(υidυ)) :: tl

              `π.emit`.pipeToSelf(defn, paramsʹ*)(`if * then … else …`(Term.ApplyInfix(\(υidυ), \("eq"),
                                                                                       Type.ArgClause(Nil),
                                                                                       Term.ArgClause("None" :: Nil)),
                                                                       `Behaviors.stopped`,
                                                                       _))

        case !(parallelism, _, Some(π @ π(λ(Symbol(ch)), Some(_), code, params*)), sum) =>
          val args = params.map {
            case λ @ λ(Symbol(_)) if λ.`type`.isDefined => id
            case λ(Symbol(par)) => par
          }

          implicit val sem = if parallelism < 0 then None else Some(id)

          val defn = sum.generate._1.map {
            case it @ Defn.Def(_, _, _, _, _, Term.Block(stats)) =>

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

              val argsʹ = args.map(Term.Param(Nil, _, Some(Type.Name("()")), None))

              it.copy(paramss = List(argsʹ.toList), body = Term.Block(`val` ::: stats))
          }

          val πʹ = Pre.π(λ(Symbol(ch)), Some(""), code, args.map(Symbol(_)).map(λ(_))*)

          val `πʹ.emit` = πʹ.emit._2

          `πʹ.emit`.pipeToSelf(defn, args.map(\(_))*)(`if * then … else …`(Term.ApplyUnary("!", args.head),
                                                                           `Behaviors.stopped`,
                                                                           _)
                                                     )(using Nil)

        case !(parallelism, _, Some(μ), sum) =>
          implicit val sem = if parallelism < 0 then None else Some(id)

          val defn = sum.generate._1

          val υidυ = id

          val `μ.emit` = μ.emit._2 match
            case (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
              it.copy(pat = Pat.Var(υidυ)) :: tl

          `μ.emit`.pipeToSelf(defn)(`if * then … else …`(Term.ApplyInfix(\(υidυ), \("eq"),
                                                                         Type.ArgClause(Nil),
                                                                         Term.ArgClause("None" :: Nil)),
                                                         `Behaviors.stopped`,
                                                         _)
                                   )(using Nil)

        case !(parallelism, _, _, sum) =>
          implicit val sem = if parallelism < 0 then None else Some(id)

          val defn = sum.generate._1

          callback(defn.getOrElse(Lit.Unit()) :: Nil, defn.spawning())

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(_, variables, _sum, _, assignment) =>
          val n = assignment.size

          val sum: + = if (variables.size == n)
                       then
                         _sum
                       else
                         `+`(-1, ∥(-1, `.`(_sum, ν(variables.drop(n).map(_.name).toSeq*))))

          sum.scheme(null)(callback)

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, qual, params*) =>
          val args = params.map(_.toTerm).toList

          val term = qual match
            case h :: t => (t.map(\(_)) :+ \("π") :+ \(identifier)).foldLeft(h: Term)(Term.Select(_, _))
            case _ => \(identifier)

          callback(Lit.Unit() :: Nil, Term.Apply(term, Term.ArgClause(args)))

        ////////////////////////////////////////////////////////// invocation //

  extension (self: Pre)(using id: => String)

    def emit: (Seq[String], List[Enumerator]) =

      var ** : Seq[String] = Nil

      var * = List[Enumerator]()

      self match

        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          ** = names.map(_ => id)
          * = names.zipWithIndex.map { (it, i) =>  `* <- Future.successful(*)`(it -> **(i)) }.toList

        case τ(Some((Left(enums), _))) =>
          * = `_ <- *`("τ")
          * = * ::: enums

        case τ(Some((Right(term), _))) =>
          * = `_ <- *`("τ")
          * :+= `_ <- Future { * }`(term)

        case τ(_) =>
          * = `_ <- *`("τ")


        case π(λ(Symbol(ch)), nu @ (None | Some("ν")), code, params*) =>
          val paramsʹ =
            nu match
              case None =>
                params
              case _ =>
                params.map {
                  case λ(Symbol(`ch`)) => λ(Symbol(id))
                  case it => it
                }

          nu match
            case None =>
            case _ =>
              val (ns, ls) = ν(paramsʹ.filter(_.isSymbol).map(_.asSymbol.name)*).emit
              ** = ns
              * = ls

          val args = paramsʹ.map(_.toTerm).toList

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(args)),
                               Term.ArgClause(expr::Nil)
                             ))
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- Future { * }`(term))
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(args)),
                               Term.ArgClause(expr::Nil)
                             ))
            case _ =>
              * :+= `_ <- *`(Term.Apply(\(ch), Term.ArgClause(args)))

          nu match
            case None =>
            case _ =>
              params.zipWithIndex.foreach {
                case (λ(Symbol(`ch`)), i) =>
                  * :+= `* <- Future.successful(*)`(ch -> paramsʹ(i).asSymbol.name)
              }

        case π(λ(Symbol(ch)), Some(""), code, params*) =>
          val args = params.map {
            case λ @ λ(Symbol(_)) if λ.`type`.isDefined => id
            case λ(Symbol(par)) => par
          }

          code match
            case Some((Right(term), _)) =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*),
                                       Term.Apply(Term.Apply(Term.Apply(\(ch), Term.ArgClause(Nil)),
                                                             Term.ArgClause(Lit.Int(args.size) :: Nil)),
                                                  Term.ArgClause(term::Nil)
                                       ))
            case _ =>
              * = Enumerator.Generator(`Seq(*) <- …`(args*),
                                       Term.Apply(Term.Apply(\(ch), Term.ArgClause(Nil)),
                                                  Term.ArgClause(Lit.Int(args.size) :: Nil)))

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
              * :+= `_ <- Future { * }`(term)
            case _ =>

        case _: π => ??? // caught by parser

        ////////////////////////////////////////////// restriction | prefixes //

      (**, *)


  final class Main(optLevel: Int):
    require(0 <= optLevel && optLevel <= 2)

    import Optimize.*

    def apply(prog: List[Bind]): List[String] =
      val id = new helper.υidυ
      given opt: Opt = Opt(Mapʹ(), Setʹ())
      prog
        .drop(1)
        .zipWithIndex
        .map {
          case ((bind, ∅()), k) =>
            opt.__1 += (bind.identifier + k) -> Mapʹ()
            opt._2 += bind.identifier
            k -> dfn(Nil, Nil)(bind)
          case ((bind, sum), k) =>
            opt.__1 += (bind.identifier + k) -> Mapʹ()
            given Listʹ[String]()
            val defn = sum.generate(using id())._1.get
            val υidυ = id()
            val recv =
              `* = gACΠ.spawnAnonymous(…)`(υidυ, defn.name.value) ::
              `* ! Left(None)`(υidυ) :: Nil
            opt._1(bind.identifier) = given_Listʹ_String.toList
            opt._2 += bind.identifier
            k -> dfn(defn :: Nil, recv)(bind)
        }
        .flatMap { (k, it) => if optLevel > 0 then it.optimize1(using opt.__1(it.name.value + k))._1 else Some(it) }
        .map { it => if optLevel > 1 then it.optimize2(using opt._2)._1 else it }
        .map(_.toString)
