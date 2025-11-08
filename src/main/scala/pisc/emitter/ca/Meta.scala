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

import parser.Calculus.{ λ, `(*)` }


object Meta extends emitter.shared.effects.Meta:

  def cls(body: List[Stat], recv: Term): `(*)` => Defn.Class =
    case `(*)`("Main", _) =>
      Defn.Class(Mod.Final() :: Nil,
                 Type.Name("Main"),
                 Type.ParamClause(Nil),
                 Ctor.Primary(Nil, Name.Anonymous(),
                              Term.ParamClause(Term.Param(Nil, "args", `:`("List", "String"), None) :: Nil) :: Nil),
                 `extends Actor[IO, Null]`(body, recv))
    case `(*)`(identifier, _, params*) =>
      Defn.Class(Mod.Final() :: Nil,
                 Type.Name(identifier + params.size),
                 Type.ParamClause(Nil),
                 Ctor.Primary(Nil, Name.Anonymous(),
                              Term.ParamClause(params
                                                 .map(_.asSymbol.name)
                                                 .map(\(_))
                                                 .map(Term.Param(Nil, _, Some(Type.Name("()")), None))
                                                 .toList) :: Nil),
                 `extends Actor[IO, Null]`(body, recv))

  def cls(υidυ: String, body: List[Stat], recv: Term, init: Boolean, args: Term.ParamClause*): Defn.Class =
    Defn.Class(Mod.Final() :: Nil,
               Type.Name(υidυ),
               Type.ParamClause(Nil),
               Ctor.Primary(Nil, Name.Anonymous(), args),
               (if init then `extends Actor[IO, Null]` else `extends Actor[IO, …]`)(body, recv))

  def dfn(υidυ: String, υidυʹ: Option[String], body: Term, args: String*) =
    Defn.Def(Nil, υidυ, `(…)`(args*), `: Receive[IO, …]`,
             Term.PartialFunction(Case(υidυʹ.fold(Pat.Wildcard())(Pat.Var(_)), None, body) :: Nil))

  private def `(…)`(* : String*) =
    Member.ParamClauseGroup(
      Type.ParamClause(Nil),
      Term.ParamClause(*
                        .map(\(_))
                        .map(Term.Param(Nil, _, Some(Type.Name("()")), None))
                        .toList,
                       None) :: Nil
    ) :: Nil

  private def `extends Actor[IO, Null]`(body: List[Stat], recv: Term) =
    Template(None,
             Init(Type.Apply(Type.Name("Actor"),
                             Type.ArgClause(Type.Name("IO") :: Type.Name("Null") :: Nil)),
                  Name.Anonymous(),
                  Seq.empty) :: Nil,
             Template.Body(None, `override def receive`(recv, true) :: body),
             Nil)

  private def `extends Actor[IO, …]`(body: List[Stat], recv: Term) =
    Template(None,
             Init(Type.Apply(Type.Name("Actor"),
                             Type.ArgClause(Type.Name("IO") ::
                                            Type.Apply(Type.Name("Deferred"),
                                                       Type.ArgClause(Type.Name("IO") :: Type.Name("()") :: Nil)) :: Nil)),
                  Name.Anonymous(),
                  Seq.empty) :: Nil,
             Template.Body(None, `override def receive`(recv, false) :: body),
             Nil)

  private def `override def receive`(body: Term, init: Boolean) =
    Defn.Def(Mod.Override() :: Nil, "receive", Nil,
             if init then `: Receive[IO, Null]` else `: Receive[IO, …]`,
             Term.PartialFunction(Case(Pat.Wildcard(), None, body) :: Nil))

  private val `: Receive[IO, Null]` =
    Some(Type.Apply(Type.Name("Receive"),
                    Type.ArgClause(Type.Name("IO") :: Type.Name("Null"):: Nil)))

  private val `: Receive[IO, …]` =
    Some(Type.Apply(Type.Name("Receive"),
                    Type.ArgClause(Type.Name("IO") ::
                                   Type.Apply(Type.Name("Deferred"),
                                              Type.ArgClause(Type.Name("IO") :: Type.Name("()") :: Nil)) :: Nil)))

  def `* <- *.get`(arg: Option[String], υidυ: Option[String], patch: Option[Enumerator.Val], code: List[Enumerator]) =
    if patch.isDefined
    then
      arg.fold(code)(`* <- *`(_, Term.Select(υidυ.get, "get")) :: patch.get :: code)
    else
      arg.fold(code)(`* <- *`(_, Term.Select(υidυ.get, "get")) :: code)

  def `* <- sys.actorOf(…)`(* : String, `…`: Term): Enumerator.Generator =
    `* <- *`(*, Term.Apply(Term.Select(Term.Select("context", "system"), "actorOf"), Term.ArgClause(`…` :: Nil)))

  def `* <- sys.actorOf(…)`(* : String, ** : String, `…`: String*): Enumerator.Generator =
    `* <- sys.actorOf(…)`(*, Term.Apply(\(**), Term.ArgClause(`…`.map(\(_)).toList)))

  def `_ <- ctx.become(*)`(υidυ: String, args: String*) =
    `_ <- *`(Term.Apply(Term.Select("context", "become"),
                        Term.ArgClause(Term.Apply(\(υidυ),
                                                  Term.ArgClause(args.map(\(_)).toList)) :: Nil)))

  def `_ <- * ! null`(* : String) =
    `_ <- *`(Term.ApplyInfix(\(*), \("!"), Type.ArgClause(Nil), Term.ArgClause(Lit.Null() :: Nil)))

  def `_ <- *.stop`(* : String) = `_ <- *`(Term.Select(*, "stop"))

  val `_ <- self.stop` = `_ <- *.stop`("self")


  def `_ <- *.tryAcquire.ifM`(* : String,`…`: Term): Enumerator.Generator =
    Enumerator.Generator(`* <- …`(), Term.Apply(Term.Select(Term.Select(*, "tryAcquire"), "ifM"),
                                                Term.ArgClause(`…` :: `IO.cede` :: Nil)))


  def `List( *, … ).parTraverse`(* : String*)(implicit `…`: Option[String] = None): Term =
    `…` match
      case Some(sem) =>
        Term.Apply(`List( *, … ).parTraverse`(* *)(), Term.ArgClause(sem :: Nil))
      case _ =>
        Term.Select(Term.Apply(\("πLs"), Term.ArgClause(*.map(\(_)).toList)), "πparTraverse")

  def `List( *, … ).parSequence`(* : Term*): Term =
    *.flatMap {
      case Term.Select(Term.Name("IO"), Term.Name("unit" | "cede")) => None
      case it => Some(it)
    } match
      case Nil => `IO.cede`
      case it => Term.Select(Term.Apply(\("πLs"), Term.ArgClause(it.toList)), "πparSequence")

/*
  a<x>. ν(y). c(z). d<w>. e(u). P(y, z, u)
  override def receive =
    case _ =>
      def _u1u(y: `()`) =
        case _u2u =>
          def _u3u(z: `()`) =
            case _u4u =>
              for
                u    <- _u4u.get
                _u5u <- system.actorOf(_u5u(y, z, u))
                _    <- _u5u ! null
              yield
                ()
          for
            z <- _u2u.get
            _ <- d(w)
            _ <- e()
            _ <- context.become(_u3u(z), true)
          yield
            ()
      for
        _ <- a(x)
        y <- ν
        _ <- c()
        _ <- context.become(_u1u(y), true)
      yield
        ()
*/
