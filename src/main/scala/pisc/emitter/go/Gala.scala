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

import parser.Calculus.{ `(*)`, λ }
import Gala.*


enum Gala(using id: => String):

  case bind(channel: String, params: Seq[λ], polarity: Option[Term.Name])
           (using => String)

  case body(lines: List[Gala])
           (using => String)

  case call(agent: String, names: Seq[λ])
           (using => String)

  case cases(semaphore: String, tryacquire: Boolean)
           (using => String)

  case code(term: Term)
           (using => String)

  case elvis(cond: ((λ, λ), Boolean), t: Gala.body, f: Gala.body)
           (using => String)

  case func(agent: String, names: Seq[String], body: Gala.body)
           (using => String)

  case init(assignment: List[(String, String)])
           (using => String)

  case noop()(using => String)

  case nu(names: String*)
         (using => String)

  case pace(amount: Long, paceunit: String)
           (using => String)

  case par(operands: List[Gala.body])
          (using => String)

  case rep(parallelism: Int, guard: List[Gala], polarity: Option[Boolean], operand: Gala.body)
          (using => String)

  case sum(operands: List[Gala.body])
          (using => String)

  case tau()(using => String)

  case void()(using => String)

  override def toString: String = issue("").mkString("\n")

  protected def issue(indent: String): List[String] = issueʹ(indent)._1._1

  protected def issueʹ(indent: String): ((List[String], String), List[String]) =
    val indentʹ = indent + " "*4

    given context = none

    given Conversion[String, ((List[String], String), List[String])] = List(_) -> indent -> Nil

    given Conversion[List[String], ((List[String], String), List[String])] = _ -> indent -> Nil

    this match

      case bind(channel, params @ Seq(λ(Lit.Null()), _*), None) =>
        val nulls = "ListOf(" + (", Go_NothingPi()"*params.size).substring(2) + ")"
        s"${indent}bind _ = " + s"$channel match {" ::
        s"${indentʹ}case Go_QueuePi(it) => it.Go_EnqueuePi($nulls)" ::
        s"${indentʹ}case it => Panic(it)" ::
        s"${indent}}" ::
        Nil

      case bind(channel, params, None) =>
        val items = params.map(_.toName).mkString("ListOf(", ", ", ")")
        s"${indent}bind _ = " + s"$channel match {" ::
        s"${indentʹ}case Go_QueuePi(it) => it.Go_EnqueuePi($items)" ::
        s"${indentʹ}case it => Panic(it)" ::
        s"${indent}}" ::
        Nil

      case bind(channel, params, Some(term)) =>
        val indentʹʹ = indentʹ + " "*4
        val indentʹʹʹ = indentʹʹ + " "*4
        val indentʹʹʹʹ = indentʹʹʹ + " "*4
        val υidυ = id
        val dequeue = term match
          case null => "Go_DequeuePi()"
          case Term.Name(fun) => s"Go_DequeuePi_($fun)"
        val conses = params.map(_.asSymbol.name).foldRight("Nil()") {
          case (s"_$_", tail) => s"Cons(_, $tail)"
          case (head, tail) => s"Cons($head, $tail)"
        }
        val pre = s"${indent}$channel match {" ::
                  s"${indentʹ}case Go_QueuePi($υidυ) =>" ::
                  s"${indentʹʹ}$υidυ.$dequeue match {" ::
                  s"${indentʹʹʹ}case Some($conses) => {" ::
                  Nil
        val post = s"${indentʹʹʹ}}" ::
                   s"${indentʹʹʹ}case Some(it) => Panic(it)" ::
                   s"${indentʹʹʹ}case _ => None()" ::
                   s"${indentʹʹ}}" ::
                   s"${indentʹ}case it => Panic(it)" ::
                   s"${indent}}" ::
                   Nil
        pre -> indentʹʹʹʹ -> post

      case it: body =>
        val (pre, _, post) = it.body.foldLeft((List.empty[List[String]], indent, List.empty[List[String]])) {
          case ((pre, indent, post), it) =>
            val ((preʹ, indentʹ), postʹ) = it.issueʹ(indent)
            (preʹ :: pre, indentʹ, postʹ :: post)
        }
        pre.reverse.flatten ::: post.flatten

      case call(agent, names) =>
        indent + "bind _ = " + agent + '_' + names.size + names.map(_.toName).mkString("(go_context, ", ", ", ")") ::
        s"${indent}Some(true)" ::
        Nil

      case cases(sem: String, false) =>
        indent + s"bind $sem = Some(Go_NewSemaphorePi(1))"

      case cases(sem: String, true) =>
        indent + s"bind _ = $sem.TryAcquire1()"

      case code(lit @ (Lit.Int(_) | Lit.Long(_) | Lit.Double(_))) =>
        indent + s"""bind _ = Some(Go_NumberPi(big.NewFloat(""" + lit.value + ")))"

      case code(lit @ Lit.Boolean(_)) =>
        indent + s"""bind _ = Some(Go_BooleanPi(""" + lit.value + "))"

      case code(lit @ Lit.String(_)) =>
        indent + s"""bind _ = Some(Go_StringPi(""" + lit.value + "))"

      case code(Term.Name(str)) =>
        indent + str.replaceAll("[']([a-zA-Z_]['a-zA-Z0-9_\"]*)", "$1")

      case code(_) => ???

      case elvis(((lhs, rhs), mismatch), t, body(Nil)) =>
        given context = context(lhs, rhs)
        var cond = s"${lhs.toName}.Equal(${rhs.toName})"
        if mismatch
        then
          cond = s"!$cond"
        s"${indent}bind _ = if (${lhs.toName}.Equal(${rhs.toName})) {" ::
        t.issue(indentʹ) :::
        s"${indent}} else Some(false)" ::
        s"${indent}Some(true)" :: // LEAF
        Nil

      case elvis(((lhs, rhs), mismatch), t, f) =>
        val tʹ = t.issue(indentʹ)
        val fʹ = f.issue(indentʹ)
        given context = context(lhs, rhs)
        var cond = s"${lhs.toName}.Equal(${rhs.toName})"
        if mismatch
        then
          cond = s"!$cond"
        s"${indent}bind _ = if ($cond) {" :: tʹ :::
        s"${indent}} else {" :: fʹ :::
        s"${indent}}" ::
        s"${indent}Some(true)" :: // LEAF
        Nil

      case func("Main", Nil, body) =>
        indent + s"func Main(go_context context.Context, args List[string]) Option[bool] {" ::
        body.issue(indentʹ) :::
        s"${indent}}" ::
        Nil

      case func(agent, names, body) =>
        val params = names.map(_ + " Go_NamePi").mkString("(go_context context.Context, ", ", ", ")")
        indent + "func " + agent + '_' + names.size + params + " Option[bool] {" ::
        body.issue(indentʹ) :::
        s"${indent}}" ::
        Nil

      case init(it) if it.isEmpty =>
        Nil

      case init(it) =>
        indent + it.map(_._1).mkString("val ", ", ", "") + it.map(_._2).mkString(" = ", ", ", "")

      case noop() =>
        Nil

      case nu(names*) =>
        names.map(s"${indent}bind " + _ + " = " + "Some(Go_QueuePi(Go_NewBlockingQueuePi()))").toList

      case pace(amount, paceunit) =>
        indent + "bind _ = Go_PacePi(" + amount + ", time." + paceunit + ")"

      case par(ops) =>
        val sem = id
        val acquire = s"${indent}bind _ = $sem.AcquireN(go_context, ${ops.size})"
        val releasable = s"${indentʹ}use $id = Go_ReleasablePi($sem)"
        s"${indent}bind $sem = Some(Go_NewSemaphorePi(${ops.size}))" ::
        acquire ::
        ops.map(_.issue(indentʹ)).flatMap(s"${indent}bind _ = Go_SpawnPi(() => {" :: releasable :: _ ::: s"${indent}})" :: Nil) :::
        acquire ::
        s"${indent}Some(true)" :: // LEAF
        Nil

      case rep(-1, it @ _ :: nu(names*) :: _, Some(false), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sig = (", Go_NamePi"*names.size).substring(2)
        val sigʹ = names.map(_ + " Go_NamePi").mkString(", ")
        s"${indent}var $υidυ func ($sig) Option[bool]" ::
        s"${indent}$υidυ = ($sigʹ) Option[bool] => {" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}$υidυ(${names.mkString(", ")})" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}$υidυ(${names.mkString(", ")})" :: // LEAF, tailcall
        Nil

      case rep(parallelism, it @ _ :: nu(names*) :: _, Some(false), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sig = (", Go_NamePi"*names.size).substring(2)
        val sigʹ = names.map(_ + " Go_NamePi").mkString(", ")
        val sem = id
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)" ::
        s"${indent}var $υidυ func ($sig) Option[bool]" ::
        s"${indent}$υidυ = ($sigʹ) Option[bool] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_context)" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        s"${indentʹʹ}use $id = Go_ReleasablePi($sem)" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}$υidυ(${names.mkString(", ")})" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}$υidυ(${names.mkString(", ")})" :: // LEAF, tailcall
        Nil

      case rep(-1, it, Some(false), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func () Option[bool]" ::
        s"${indent}$υidυ = () Option[bool] => {" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}$υidυ()" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}$υidυ()" :: // LEAF, tailcall
        Nil

      case rep(parallelism, it, Some(false), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)" ::
        s"${indent}var $υidυ func () Option[bool]" ::
        s"${indent}$υidυ = () Option[bool] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_context)" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        s"${indentʹʹ}use $id = Go_ReleasablePi($sem)" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}$υidυ()" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}$υidυ()" :: // LEAF, tailcall
        Nil

      case rep(-1, hd :: (it @ bind(_, params, _)) :: tl, Some(true), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val names = params.map(_.asSymbol.name).map {
          case s"_$_" => "_"
          case it => it
        }
        val sig = (", Go_NamePi"*names.size).substring(2)
        val sigʹ = names.map(_ + " Go_NamePi").mkString(", ")
        s"${indent}var $υidυ func ($sig) Option[bool]" ::
        s"${indent}$υidυ = ($sigʹ) Option[bool] => {" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        {
          val ((pre, indentʹʹʹ), post) = it.issueʹ(indentʹ)
          hd.issue(indentʹ) :::
          pre :::
          (tl: body).issue(indentʹʹʹ) :::
          s"${indentʹʹʹ}$υidυ(${names.mkString(", ")})" ::
          post
        } :::
        s"${indent}}" ::
        {
          val ((pre, indentʹʹʹ), post) = it.issueʹ(indent)
          pre :::
          (tl: body).issue(indentʹʹʹ) :::
          s"${indentʹʹʹ}$υidυ(${names.mkString(", ")})" ::
          post
        } // LEAF: return in bind

      case rep(parallelism, hd :: (it @ bind(_, params, _)) :: tl, Some(true), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        val names = params.map(_.asSymbol.name).map {
          case s"_$_" => "_"
          case it => it
        }
        val sig = (", Go_NamePi"*names.size).substring(2)
        val sigʹ = names.map(_ + " Go_NamePi").mkString(", ")
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)" ::
        s"${indent}var $υidυ func ($sig) Option[bool]" ::
        s"${indent}$υidυ = ($sigʹ) Option[bool] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_context)" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        s"${indentʹʹ}use $id = Go_ReleasablePi($sem)" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        {
          val ((pre, indentʹʹʹ), post) = it.issueʹ(indentʹ)
          hd.issue(indentʹ) :::
          pre :::
          (tl: body).issue(indentʹʹʹ) :::
          s"${indentʹʹʹ}$υidυ(${names.mkString(", ")})" ::
          post
        } :::
        s"${indent}}" ::
        {
          val ((pre, indentʹʹʹ), post) = it.issueʹ(indent)
          pre :::
          (tl: body).issue(indentʹʹʹ) :::
          s"${indentʹʹʹ}$υidυ(${names.mkString(", ")})" ::
          post
        } // LEAF: return in bind

      case rep(-1, it, None, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func () Option[bool]" ::
        s"${indent}$υidυ = () Option[bool] => {" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}$υidυ()" ::
        s"${indent}}" ::
        s"${indent}$υidυ()" :: // LEAF, tailcall
        Nil

      case rep(parallelism, it, None, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)" ::
        s"${indent}var $υidυ func () Option[bool]" ::
        s"${indent}$υidυ = () Option[bool] => {" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        s"${indentʹʹ}use $id = Go_ReleasablePi($sem)" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}$υidυ()" ::
        s"${indent}}" ::
        s"${indent}$υidυ()" :: // LEAF, tailcall
        Nil

      case _: rep => ???

      case sum(ops) =>
        val sem = id
        val semʹ = id
        val releasable = s"${indentʹ}use $id = Go_ReleasablePi($sem)"
        val tryacquire = s"${indentʹ}bind _ = $semʹ.TryAcquire1()"
        val acquire = s"${indent}bind _ = $sem.AcquireN(go_context, ${ops.size})"
        s"${indent}bind $sem = Some(Go_NewSemaphorePi(${ops.size}))" ::
        s"${indent}bind $semʹ = Some(Go_NewSemaphorePi(1))" ::
        acquire ::
        ops.map(_.issue(indentʹ)).flatMap(s"${indent}bind _ = Go_SpawnPi(() => {" :: releasable :: tryacquire :: _ ::: s"${indentʹ}Some(true)" :: s"${indent}})" :: Nil) :::
        acquire ::
        s"${indent}Some(true)" :: // LEAF
        Nil

      case tau() =>
        Nil

      case void() =>
        s"${indent}Some(true)" // LEAF


object Gala:

  export Gala.*, context.*

  enum context:
    case none, number, boolean, string

  object context:

    def apply(lhs: λ, rhs: λ): context =
      lhs.`val` match
        case _: BigDecimal | _: Lit.Int | _: Lit.Long | _: Lit.Double => number
        case _: Boolean | _: Lit.Boolean => boolean
        case _: String | _: Lit.String => string
        case _ =>
          rhs.`val` match
            case _: BigDecimal | _: Lit.Int | _: Lit.Long | _: Lit.Double => number
            case _: Boolean | _: Lit.Boolean => boolean
            case _: String | _: Lit.String => string
            case _ => none

  extension (self: body)
    def body: List[Gala] =
      self.lines match
        case List(body: body) =>
          body.body
        case _ =>
          self.lines

  extension (self: λ)
    def toName: context ?=> String = context ?=> {
      self.`val` match
        case Symbol(it) => it
        case it: BigDecimal => s"Go_NumberPi(big.NewFloat($it))"
        case it: Boolean => s"Go_BooleanPi(${it.toString.toLowerCase})"
        case it: String => s"Go_StringPi(\"$it\")"
        case Lit.Int(it) => s"Go_IntPi($it)"
        case Lit.Long(it) => s"Go_IntPi($it)"
        case Lit.Double(it) => s"Go_NumberPi(big.NewFloat($it))"
        case Lit.Boolean(it) => s"Go_BooleanPi($it)"
        case Lit.String(it) => s"Go_StringPi($it)"
        case Term.Name(it) =>
          val itʹ = it.replaceAll("[']([a-zA-Z_]['a-zA-Z0-9_\"]*)", "$1")
          context match
            case `number` => s"Go_NumberPi(big.NewFloat($itʹ))"
            case `boolean` => s"Go_BooleanPi($itʹ)"
            case `string` => s"Go_StringPi($itʹ)"
            case _ => itʹ
        case _ => "Panic(\"N/A\")"
    }

  given list2body(using => String): Conversion[List[Gala], body] = body(_)

  given Conversion[Gala, List[Gala]] = {
    case it: body => it.body
    case it => List(it)
  }

  def defn(body: body)(using => String): `(*)` => func =
    case `(*)`("Main", _) =>
      func("Main", Nil, body)
    case `(*)`(identifier, _, params*) =>
      func(identifier, params.map(_.asSymbol.name), body)
