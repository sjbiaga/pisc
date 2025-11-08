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
package ca

import scala.meta.*
import dialects.Scala3

import parser.Calculus.*
import ca.Meta.*


object Program:

  extension (self: AST)(using id: => String)

    def generateʹ(implicit semaphore: Option[String]): Defn.Class =

      self match

        case ∥(_, operand) =>
          operand.generateʹ

        case it @ `.`(?:(_, _, None)) =>
          def cases(sum: +): Either[Defn.Class, Term] =
            sum match
              case +(-1, ∥(-1, `.`(?:(((lhs, rhs), mismatch), t, None)))) =>

                Right {
                  cases(t) match

                    case Left(clasz) =>

                      val name = clasz.name.value

                      val υidυ = id

                      val code = `* <- sys.actorOf(…)`(υidυ, name) :: `_ <- * ! null`(υidυ) :: `_ <- self.stop`

                      if mismatch
                      then
                        `if * then … else …`(====(lhs, rhs), `_ <- self.stop`, Term.Block(clasz :: (code: Term) :: Nil))
                      else
                        `if * then … else …`(====(lhs, rhs), Term.Block(clasz :: (code: Term) :: Nil), `_ <- self.stop`)

                    case Right(term) =>

                      if mismatch
                      then
                        `if * then … else …`(====(lhs, rhs), `_ <- self.stop`, term)
                      else
                        `if * then … else …`(====(lhs, rhs), term, `_ <- self.stop`)
                }

              case _ =>

                Left {
                  val clasz = sum.generate()._1.get

                  val name = clasz.name.value

                  val υidυ = id

                  val code = `* <- sys.actorOf(…)`(υidυ, name) :: `_ <- * ! null`(υidυ) :: Nil

                  cls(id, clasz :: Nil, `_ <- *.tryAcquire.ifM`(semaphore.get, code) :: `_ <- self.stop` :: Nil, true)
                }

          val arg = Term.ParamClause(Term.Param(Nil, semaphore.get,
                                                Some(Type.Apply(Type.Name("Semaphore"),
                                                                Type.ArgClause(Type.Name("IO") :: Nil))),
                                                None) :: Nil)

          cls(id, Nil, cases(`+`(-1, ∥(-1, it))).right.get, true, arg)

    def generate(implicit semaphore: Option[String] = None): (Option[Defn.Class], Int) =

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>
          None -> -1

        case +(-1|1, operand) =>
          operand.generate

        case it: + if it.scaling == -1 && it.choices.size > 1 && it.choices.forall { case ∥(-1, `.`(?:(_, _, None))) => true case _ => false } =>
          implicit val sem = Some(id)

          val clazs = it.choices.foldRight(List[Defn.Class]())(_.generateʹ :: _)

          val names = clazs.map(_.name.value)

          val υidυs = names.map(_ => id)

          val ios = (υidυs zip names).map(`* <- sys.actorOf(…)`(_, _, sem.get))

          var recv = `* <- Semaphore[IO](…)`(sem.get) +: ios :+ `_ <- *`(`List( *, … ).parTraverse`(υidυs*)())
          semaphore.foreach(recv :+= `_ <- *.release`(_))
          recv :+= `_ <- self.stop`

          Some(cls(id, clazs, recv, true)) -> -1

        case it: + =>
          val clazs = it.choices.foldRight(List[Defn.Class]())(_.generate()._1.get :: _)

          val names =
            it.scaling match
              case -1|1 =>
                clazs.map(_.name.value)
              case _ =>
                List.fill(it.scaling)(clazs.map(_.name.value)).reduce(_ ::: _)

          val υidυs = names.map(_ => id)

          val ios = (υidυs zip names).map(`* <- sys.actorOf(…)`(_, _))

          implicit val sem = Some(id)

          var recv = ios :+ `* <- Semaphore[IO](…)`(sem.get) :+ `_ <- *`(`List( *, … ).parTraverse`(υidυs*))
          semaphore.foreach(recv :+= `_ <- *.release`(_))
          recv :+= `_ <- self.stop`

          Some(cls(id, clazs, recv, true)) -> -1

        /////////////////////////////////////////////////////////// summation //


        // COMPOSITION /////////////////////////////////////////////////////////

        case ∥(-1|1, operand) =>

          operand.generate match

            case (Some(clasz @ Defn.Class(_, Type.Name(name), _, Ctor.Primary(_, _, List(List(Term.Param(_, Term.Name(sem), _, _)))), _)), parallelism) =>

              val υidυ = id

              val ios = `* <- Semaphore[IO](…)`(sem, parallelism) :: `* <- sys.actorOf(…)`(υidυ, name, sem) :: Nil
              val recv = ios :+ `_ <- * ! null`(υidυ) :+ `_ <- self.stop`

              Some(cls(id, clasz :: Nil, recv, true)) -> -1

            case it => it


        case it: ∥ =>
          val clazs = it.components.foldRight(List[(Option[Defn.Class], Int)]())(_.generate() :: _)

          val args = clazs.flatMap {
            case (Some(Defn.Class(_, Type.Name(name), _, Ctor.Primary(_, _, List(List(Term.Param(_, Term.Name(sem), _, _)))), _)), parallelism) =>
              Some(name -> (sem -> parallelism))
            case _ =>
              None
          }.toMap

          val names =
            it.scaling match
              case -1|1 =>
                clazs.map(_._1.get.name.value)
              case _ =>
                List.fill(it.scaling)(clazs.map(_._1.get.name.value)).reduce(_ ::: _)

          val υidυs = names.map(_ => id)

          val ios = (υidυs zip names).flatMap { (υidυ, name) =>
            args.get(name) match
              case Some((sem, parallelism)) =>
                `* <- Semaphore[IO](…)`(sem, parallelism) :: `* <- sys.actorOf(…)`(υidυ, name, sem) :: Nil
              case _ =>
                `* <- sys.actorOf(…)`(υidυ, name) :: Nil
          }

          var recv = ios :+ `_ <- *`(`List( *, … ).parTraverse`(υidυs*))
          semaphore.foreach(recv :+= `_ <- *.release`(_))
          recv :+= `_ <- self.stop`

          Some(cls(id, clazs.map(_._1.get), recv, true)) -> -1

        ///////////////////////////////////////////////////////// composition //


        // SEQUENCE ////////////////////////////////////////////////////////////

        case `.`(end, ps*) =>
          var i = 0
          var j = -1
          var * = Seq.empty[(String, String, Seq[String], String, List[Enumerator])]

          while {
            j = ps.indexWhere({ case π(_, _, Some(""), _) => true case _ => false }, i)
            j >= 0
          } do
            val π(_, λ(Symbol(arg)), Some(""), _) = ps(j) : @unchecked
            val psʹ = ps.drop(i).take(j - i + 1)
            val ns = psʹ.flatMap { case ν(names*) => names case _ => Nil }
            * :+= (id, id, ns, arg, psʹ.foldLeft(List[Enumerator]())(_ ::: _.emit))
            i = j + 1

          var sem: String = null

          def nest(k: Int, arg: Option[String], par: Option[String], υidυʹʹ: Option[String], patch: Option[Enumerator.Val]): Term =
            if k == *.size
            then
              given Enumerator.Generator = `_ <- self.stop`
              end.emit { done ?=> (body, term) =>
                val υidυ = id
                val psʹ = ps.drop(i)
                var code = psʹ.foldLeft(List[Enumerator]())(_ ::: _.emit)
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
                var start = code.size
                val spawn =
                  if term ne null
                  then `* <- sys.actorOf(…)`(υidυ, term) :: `_ <- * ! null`(υidυ) :: done :: Nil
                  else `_ <- self.stop` :: Nil
                if k == 0
                then
                  (nsʹ ++ psʹ.flatMap { case ν(names*) => names case _ => Nil }) match
                    case Nil =>
                      if term ne null
                      then
                        code = code ::: spawn
                      else
                        code :+= done
                        if code.size == 1 then code ::= `_ <- IO.unit`
                      (start, Term.Block(body :+ (code: Term)))
                    case ns =>
                      val υidυ = id
                      code = code :+ `_ <- * ! null`("self") :+ `_ <- ctx.become(*)`(υidυ, ns*)
                      val bodyʹ = dfn(υidυ, None, Term.Block(body :+ (spawn: Term)), ns*) :: Nil
                      (start, Term.Block(bodyʹ :+ (code: Term)))
                else
                  code = `* <- *.get`(par, υidυʹʹ, patch, code)
                  start = code.size
                  if term ne null
                  then
                    val υidυ = id
                    val ns = par.get +: (nsʹ ++ psʹ.flatMap { case ν(names*) => names case _ => Nil })
                    code = code :+ `_ <- * ! null`("self") :+ `_ <- ctx.become(*)`(υidυ, ns*)
                    val bodyʹ = dfn(υidυ, None, Term.Block(body :+ (spawn: Term)), ns*) :: Nil
                    (start, Term.Block(bodyʹ :+ (code: Term)))
                  else
                    code :+= done
                    (start, Term.Block(body :+ (code: Term)))
              } match
                case Term.Tuple(Term.Name(semʹ) :: it :: Nil) => sem = semʹ; it
                case it                                       => it
            else
              val (υidυ, υidυʹ, ns, argʹ, code) = *(k)
              val (codeʹ, patchʹ, parʹ) =
                code.reverse match
                  case (v @ Enumerator.Val(_, _)) :: Enumerator.Generator(Pat.Var(Term.Name(p)), it) :: _ =>
                    (code.init.init :+ Enumerator.Generator(Pat.Wildcard(), it), Some(v), Some(p))
                  case Enumerator.Generator(Pat.Var(_), it) :: _ =>
                    (code.init :+ Enumerator.Generator(Pat.Wildcard(), it), None, Some(argʹ))
              val args = arg.fold(ns)(_ +: ns)
              val f_y: Term = `* <- *.get`(par, υidυʹʹ, patch, codeʹ) :+ `_ <- ctx.become(*)`(υidυ, args*)
              Term.Block(dfn(υidυ, Some(υidυʹ), nest(k + 1, Some(argʹ), parʹ, Some(υidυʹ), patchʹ), args*) :: f_y :: Nil)

          def wrap: Term => Term =

            case Term.Block(stat :: Term.ForYield(code, _) :: Nil) =>

              val codeʹ = code.last match

                case it @ Enumerator.Generator(_, Term.Apply(Term.Select(Term.Name("context"), Term.Name("become")), _)
                                                | Term.Select(Term.Name("self"), Term.Name("stop"))) =>

                  code.init :+ `_ <- *.release`(semaphore.get) :+ it

                case _ =>

                  code :+ `_ <- *.release`(semaphore.get)

              Term.Block(stat :: (codeʹ: Term) :: Nil)

            case Term.Block(stat :: (it @ Term.Select(Term.Name("self"), Term.Name("stop"))) :: Nil) =>

              val codeʹ = `_ <- *.release`(semaphore.get) :: `_ <- *`(it) :: Nil

              Term.Block(stat :: (codeʹ: Term) :: Nil)

          val recv: Term = nest(0, None, None, None, None) match

            case it if !semaphore.isDefined => it

            case Term.If(cond, t, f) => Term.If(cond, wrap(t), wrap(f))

            case it => wrap(it)

          val clasz = cls(id, Nil, recv, false)

          end match {

            case !(parallelism, _, _, _) if parallelism >= 0 =>

              val ctor = clasz.ctor.copy(paramss = Term.ParamClause(Term.Param(Nil, sem,
                                                                               Some(Type.Apply(Type.Name("Semaphore"),
                                                                                               Type.ArgClause(Type.Name("IO") :: Nil))),
                                                                               None) :: Nil) :: Nil)
              Some(clasz.copy(ctor = ctor)) -> parallelism

            case _ =>

              Some(clasz) -> -1

          }

        //////////////////////////////////////////////////////////// sequence //

    private def emit(parallelism: Int,
                     pace: Option[(Long, String)],
                     υidυ: String,
                     guard: Option[List[Enumerator]])
                    (using sem: Option[String]): ((Int, Term)) => Term =

      case (_, Term.Block((df @ Defn.Def(_, _, _, _, _, Term.PartialFunction(List(it @ Case(_, _, Term.Block(stat :: Term.ForYield(spawn, _) :: Nil)))))) :: tl)) =>

        var `!⋯` =
          if guard.isDefined
          then
            guard.get :+ `_ <- *` { `if * then … else …`(Term.ApplyInfix(\(υidυ), \("eq"),
                                                                         Type.ArgClause(Nil),
                                                                         Term.ArgClause(\("None") :: Nil)),
                                                         `_ <- self.stop`,
                                                         spawn)
                                  }
          else
             spawn

        if pace.isDefined
        then
          `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!⋯`).get

        if parallelism >= 0
        then
          `!⋯` = `_ <- *.acquire`(sem.get) :: `!⋯`

        val term = Term.Block(df.copy(body = Term.PartialFunction(List(it.copy(body = Term.Block(stat :: (`!⋯`: Term) :: Nil))))) :: tl)

        if parallelism < 0
        then
          term
        else
          Term.Tuple(List(\(sem.get), term))

      case (start, Term.Block(stat :: Term.ForYield(code, _) :: _)) =>

        val υidυʹ = id

        val spawn = code.drop(start)

        var `!⋯` =
          if guard.isDefined
          then
            guard.get :+ `_ <- *` { `if * then … else …`(Term.ApplyInfix(\(υidυ), \("eq"),
                                                                         Type.ArgClause(Nil),
                                                                         Term.ArgClause(\("None") :: Nil)),
                                                         `_ <- self.stop`,
                                                         spawn)
                                  }
          else
             spawn

        if pace.isDefined
        then
          `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!⋯`).get

        if parallelism >= 0
        then
          `!⋯` = `_ <- *.acquire`(sem.get) :: `!⋯`

        val ns = stat match
          case Defn.Def(_, _, _, List(ps), _, _) => ps.map(_.name.value).toSeq
          case _ => Nil

        val statʹ = dfn(υidυʹ, None, Term.Block(stat :: (`!⋯`: Term) :: Nil), ns*)

        val codeʹ = code.take(start) :+ `_ <- * ! null`("self") :+ `_ <- ctx.become(*)`(υidυʹ, ns*)

        val term = Term.Block(statʹ :: (codeʹ: Term) :: Nil)

        if parallelism < 0
        then
          term
        else
          Term.Tuple(List(\(sem.get), term))

    def emit(mk: Enumerator.Generator ?=> (List[Stat], Term) => (Int, Term))
            (using Enumerator.Generator): Term =

      self match

        // SUMMATION ///////////////////////////////////////////////////////////

        case ∅() =>

          mk(Lit.Unit() :: Nil, null)._2

        case it: + =>

          val clasz = it.generate._1.get

          mk(clasz :: Nil, Term.Apply(\(clasz.name.value), Term.ArgClause(Nil)))._2

        /////////////////////////////////////////////////////////// summation //


        // (MIS)MATCH | IF THEN ELSE | ELVIS OPERATOR //////////////////////////

        case ?:(((lhs, rhs), mismatch), t, f) =>

          t.generate._1 -> f.flatMap(_.generate._1) match

            case (clasz, claszʹ) =>

              (clasz.fold(mk(Lit.Unit() :: Nil, null)) { it => mk(it :: Nil, Term.Apply(\(it.name.value), Term.ArgClause(Nil))) }
              ,claszʹ.fold(mk(Lit.Unit() :: Nil, null)) { it => mk(it :: Nil, Term.Apply(\(it.name.value), Term.ArgClause(Nil))) }) match

                case ((start, Term.Block(stat :: Term.ForYield(code, _) :: _))
                     ,(_, Term.Block(statʹ :: Term.ForYield(codeʹ, _) :: _))) =>

                  val t = Term.Block(stat :: (code.drop(start): Term) :: Nil)
                  val f = Term.Block(statʹ :: (codeʹ.drop(start): Term) :: Nil)

                  code.take(start) :+ `_ <- *`(
                    if mismatch
                    then
                      `if * then … else …`(====(lhs, rhs), f, t)
                    else
                      `if * then … else …`(====(lhs, rhs), t, f)
                  )

                case ((_, t), (_, f)) =>

                  if mismatch
                  then
                    `if * then … else …`(====(lhs, rhs), f, t)
                  else
                    `if * then … else …`(====(lhs, rhs), t, f)

        ////////////////////////// (mis)match | if then else | elvis operator //


        // REPLICATION /////////////////////////////////////////////////////////

        case !(parallelism, pace, Some(π @ π(_, λ(Symbol(par)), Some("ν"), _)), sum) =>
          val υidυ = id

          val `π.emit` = π.emit match
            case hd :: (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
              hd :: it.copy(pat = Pat.Var(υidυ)) :: tl

          implicit val sem = if parallelism < 0 then None else Some(id)

          emit(parallelism, pace, υidυ, Some(`π.emit`)) {
            sum.generate._1 match
              case Some(clasz) =>
                given Enumerator.Generator = `_ <- * ! null`("self")
                val ctor = clasz.ctor.copy(paramss = Term.ParamClause(Term.Param(Nil, par,
                                                                                 Some(Type.Name("()")),
                                                                                 None) :: Nil) :: Nil)
                mk(clasz.copy(ctor = ctor) :: Nil, Term.Apply(\(clasz.name.value), Term.ArgClause(par :: Nil)))
              case _ =>
                given Enumerator.Generator = `_ <- *`(sem.fold(`_ <- * ! null`("self") :: Nil)(`_ <- *.release`(_) :: `_ <- * ! null`("self") :: Nil))
                mk(Lit.Unit() :: Nil, null)
          }

        case !(parallelism, pace, Some(π @ π(λ(Symbol(ch)), λ @ λ(Symbol(arg)), Some(_), _)), sum) =>
          val par = if λ.`type`.isDefined then id else arg

          val πʹ = π.copy(name = λ.copy()(using None))

          val `πʹ.emit` = πʹ.emit match
            case Enumerator.Generator(Pat.Var(_), it) :: tl =>
               Enumerator.Generator(Pat.Wildcard(), it) :: tl

          val `val` =
            λ.`type` match
              case Some((tpe, Some(refined))) =>
                `* = *: * …`(arg, par, tpe, refined) :: Nil
              case Some((tpe, _)) =>
                `* = *: *`(arg, par, tpe) :: Nil
              case _ => Nil

          implicit val sem = if parallelism < 0 then None else Some(id)

          sum.generate._1 match {
            case Some(clasz) =>
              given Enumerator.Generator = `_ <- * ! null`("self")
              val ctor = clasz.ctor.copy(paramss = Term.ParamClause(Term.Param(Nil, arg,
                                                                               Some(Type.Name("()")),
                                                                               None) :: Nil) :: Nil)
              mk(clasz.copy(ctor = ctor) :: Nil, Term.Apply(\(clasz.name.value), Term.ArgClause(arg :: Nil)))
            case _ =>
              given Enumerator.Generator = `_ <- *`(sem.fold(`_ <- * ! null`("self") :: Nil)(`_ <- *.release`(_) :: `_ <- * ! null`("self") :: Nil))
              mk(Lit.Unit() :: Nil, null)
          } match

            case (_, Term.Block((df @ Defn.Def(_, _, _, _, _, Term.PartialFunction(List(it @ Case(_, _, Term.Block(stat :: Term.ForYield(spawn, _) :: Nil)))))) :: tl)) =>

              val υidυʹ = id

              var `!⋯` = `πʹ.emit`

              if pace.isDefined
              then
                `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!⋯`).get

              if parallelism >= 0
              then
                `!⋯` = `_ <- *.acquire`(sem.get) :: `!⋯`

              val spawnʹ = spawn :+ `_ <- ctx.become(*)`(υidυʹ)

              val codeʹ = `* <- *`(par, Term.Select(par, "get")) ::
                          `_ <- *` { `if * then … else …`(Term.ApplyUnary("!", par),
                                                          `_ <- self.stop`,
                                                          `val` ::: spawnʹ)
                                   } :: Nil

              val statʹ = dfn(υidυʹ, Some(par), Term.Block(stat :: (codeʹ: Term) :: Nil))

              `!⋯` :+= `_ <- ctx.become(*)`(υidυʹ)

              val term = Term.Block(df.copy(body = Term.PartialFunction(List(it.copy(body = Term.Block(statʹ :: (`!⋯`: Term) :: Nil))))) :: tl)

              if parallelism < 0
              then
                term
              else
                Term.Tuple(List(\(sem.get), term))

            case (start, Term.Block(stat :: Term.ForYield(code, _) :: _)) =>

              val υidυʹ = id
              val υidυʹʹ = id

              val spawn = code.drop(start)

              var `!⋯` = `πʹ.emit`

              if pace.isDefined
              then
                `!⋯` = pace.map(`_ <- IO.sleep(*.…)`(_, _) :: `!⋯`).get

              if parallelism >= 0
              then
                `!⋯` = `_ <- *.acquire`(sem.get) :: `!⋯`

              val spawnʹ = spawn :+ `_ <- ctx.become(*)`(υidυʹʹ)

              val codeʹ = `* <- *`(par, Term.Select(par, "get")) ::
                          `_ <- *` { `if * then … else …`(Term.ApplyUnary("!", par),
                                                          `_ <- self.stop`,
                                                          `val` ::: spawnʹ)
                                   } :: Nil

              val statʹ = dfn(υidυʹ, Some(par), Term.Block(stat :: (codeʹ: Term) :: Nil))

              `!⋯` :+= `_ <- ctx.become(*)`(υidυʹ)

              val ns = stat match
                case Defn.Def(_, _, _, List(ps), _, _) => ps.map(_.name.value).toSeq
                case _ => Nil

              val statʹʹ = dfn(υidυʹʹ, None, Term.Block(statʹ :: (`!⋯`: Term) :: Nil), ns*)

              val codeʹʹ = code.take(start) :+ `_ <- * ! null`("self") :+ `_ <- ctx.become(*)`(υidυʹʹ, ns*)

              val term = Term.Block(statʹʹ :: (codeʹʹ: Term) :: Nil)

              if parallelism < 0
              then
                term
              else
                Term.Tuple(List(\(sem.get), term))

        case !(parallelism, pace, Some(μ), sum) =>
          val υidυ = id

          val `μ.emit` = μ.emit match
            case (it @ Enumerator.Generator(Pat.Wildcard(), _)) :: tl =>
              it.copy(pat = Pat.Var(υidυ)) :: tl

          implicit val sem = if parallelism < 0 then None else Some(id)

          emit(parallelism, pace, υidυ, Some(`μ.emit`)) {
            sum.generate._1 match
              case Some(clasz) =>
                given Enumerator.Generator = `_ <- * ! null`("self")
                mk(clasz :: Nil, Term.Apply(\(clasz.name.value), Term.ArgClause(Nil)))
              case _ =>
                given Enumerator.Generator = `_ <- *`(sem.fold(`_ <- * ! null`("self") :: Nil)(`_ <- *.release`(_) :: `_ <- * ! null`("self") :: Nil))
                mk(Lit.Unit() :: Nil, null)
          }

        case !(parallelism, pace, _, sum) =>

          implicit val sem = if parallelism < 0 then None else Some(id)

          emit(parallelism, pace, null, None) {
            sum.generate._1 match
              case Some(clasz) =>
                given Enumerator.Generator = `_ <- * ! null`("self")
                mk(clasz :: Nil, Term.Apply(\(clasz.name.value), Term.ArgClause(Nil)))
              case _ =>
                given Enumerator.Generator = `_ <- *`(sem.fold(`_ <- * ! null`("self") :: Nil)(`_ <- *.release`(_) :: `_ <- * ! null`("self") :: Nil))
                mk(Lit.Unit() :: Nil, null)
          }

        ///////////////////////////////////////////////////////// replication //


        // INSTANTIATION ///////////////////////////////////////////////////////

        case `⟦⟧`(_, variables, _sum, _, assignment) =>
          val n = assignment.size

          val sum = if (variables.size == n)
                    then
                      _sum
                    else
                      `+`(-1, ∥(-1, `.`(_sum, ν(variables.drop(n).map(_.name).toSeq*))))

          sum.emit(mk)

        case _: `{}` => ???

        /////////////////////////////////////////////////////// instantiation //


        // INVOCATION //////////////////////////////////////////////////////////

        case `(*)`(identifier, qual, params*) =>
          val identifierʹ = identifier + params.size

          val args = params.map(_.toTerm).toList

          val term = qual match
            case h :: t => (t.map(\(_)) :+ \("π") :+ \(identifierʹ)).foldLeft(h: Term)(Term.Select(_, _))
            case _ => \(identifierʹ)

          mk(Lit.Unit() :: Nil, Term.Apply(term, Term.ArgClause(args)))._2

        ////////////////////////////////////////////////////////// invocation //

  extension (self: Pre)(using id: => String)

    def emit: List[Enumerator] =

      var * = List[Enumerator]()

      self match

        // RESTRICTION | PREFIXES //////////////////////////////////////////////

        case ν(names*) =>
          * = names.map { it => `* <- *`(it -> "ν") }.toList

        case τ(Some((Left(enums), _))) =>
          * :+= `_ <- *`("τ")
          * :::= enums

        case τ(Some((Right(term), _))) =>
          * :+= `_ <- *`("τ")
          * :+= `_ <- IO { * }`(term)

        case τ(_) =>
          * = `_ <- *`("τ")


        case π(λ(Symbol(ch)), arg, nu @ (None | Some("ν")), code) =>
          nu match
            case None =>
            case _ =>
              val λ(Symbol(par)) = arg
              * = ν(par).emit

          code match
            case Some((Left(enums), _)) =>
              val expr = `for * yield ()`(enums*)
              * :+= `_ <- *`(Term.Apply(
                               Term.Apply(\(ch), Term.ArgClause(arg.toTerm::Nil)),
                               Term.ArgClause(expr::Nil)
                             ))
            case Some((Right(term), _)) =>
              val expr = `for * yield ()`(`_ <- IO { * }`(term))
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
              * :+= `_ <- IO { * }`(term)
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

      *

  final class Main:

    def apply(prog: List[Bind]): List[String] =
      val id = new helper.υidυ
      prog
        .map {
          case (bind, ∅()) =>
            cls(Nil, `IO.cede`)(bind)
          case (bind, sum) =>
            val clasz = sum.generate(using id())._1.get
            val υidυ = id()
            val recv: Term =
              `* <- sys.actorOf(…)`(υidυ, clasz.name.value) ::
              `_ <- * ! null`(υidυ) ::
              `_ <- self.stop` :: Nil
            cls(clasz :: Nil, recv)(bind)
        }
        .map(_.toString)
