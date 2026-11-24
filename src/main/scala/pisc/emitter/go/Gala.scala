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
import context.*


enum Gala:

  case alloc(var id: String = null)

  case body(lines: List[Gala])

  case call(agent: String, names: Seq[λ])

  case code(term: Term)

  case decl(name: String, value: String)

  case elvis(cond: ((λ, λ), Boolean), t: Gala.body, f: Gala.body)

  case exclude(excluded: List[String])

  case func(agent: String, names: Seq[String], body: Gala.body)

  case init(assignment: Either[List[(String, String)], (String, alloc)])

  case noop()

  case nu(names: alloc | Seq[String])

  case pace(amount: Long, paceunit: String)

  case par(operands: List[Gala.body])

  case pi(channel: String, param: λ, polarity: Option[Term.Name], rate: Any, key: String)

  case rep(parallelism: Int, guard: List[Gala], polarity: Boolean, operand: Gala.body)

  case sum(operands: List[Gala.body])

  case tau(rate: String, key: String)

  case void()

  override def toString: String =
    val id = new helper.υidυ
    issue("")(using id()).mkString("\n")

  protected def issue(indent: String)(using id: => String): List[String] =
    val indentʹ = indent + " "*4

    given context = none

    given Conversion[String, List[String]] = List(_)

    this match

      case it @ alloc(_) =>
        it.id = id
        Nil

      case it: body =>
        it.body.flatMap(_.issue(indent))

      case call(agent, names) =>
        val params = names.map(_.toName) :+ "go_scope" :+ "go_stochastic"
        indent + "bind go_scope = Go_UUIDPi()" ::
        indent + "bind _ = " + agent + '_' + names.size + params.mkString("(", ", ", ")") ::
        s"${indent}Go_returnPi" ::
        Nil

      case code(lit @ (Lit.Int(_) | Lit.Long(_) | Lit.Double(_))) =>
        indent + s"""bind _ = Go_donePi(Go_NumberPi(big.NewFloat(""" + lit.value + ")))"

      case code(lit @ Lit.Boolean(_)) =>
        indent + s"""bind _ = Go_donePi(Go_BoolPi(""" + lit.value + "))"

      case code(lit @ Lit.String(_)) =>
        indent + s"""bind _ = Go_donePi(Go_StringPi(""" + lit.value + "))"

      case code(Term.Name(it)) =>
        indent + it.replaceAll("[']([a-zA-Z_]['a-zA-Z0-9_\"]*)", "$1")

      case code(_) => ???

      case decl(name, value) =>
        indent + s"val $name = $value"

      case elvis(((lhs, rhs), mismatch), t, body(Nil)) =>
        given context = context(lhs, rhs)
        var cond = s"${lhs.toName}.Equal(${rhs.toName})"
        if mismatch
        then
          cond = s"!$cond"
        s"${indent}bind _ = if ($cond) {" ::
        t.issue(indentʹ) :::
        s"${indent}} else Go_returnPi" ::
        s"${indent}Go_returnPi" :: // LEAF
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
        s"${indent}Go_returnPi" :: // LEAF
        Nil

      case exclude(excluded) =>
        indent + excluded.mkString("Go_ExcludePi(HashSetOf(", ", ", "), go_stochastic)") // pseudo-LEAF

      case func("Main", Nil, body) =>
        indent + s"func Main(args List[string], go_scope Go_NamePi, go_stochastic Go_StochasticPi) Go_TrampolinePi[Go_NamePi] {" ::
        body.issue(indentʹ) :::
        s"${indent}}" ::
        Nil

      case func(agent, Nil, body) =>
        val params = "(go_scope Go_NamePi, go_stochastic Go_StochasticPi)"
        indent + "func " + agent + '_' + 0 + params + " Go_TrampolinePi[Go_NamePi] {" ::
        body.issue(indentʹ) :::
        s"${indent}}" ::
        Nil

      case func(agent, names, body) =>
        val params = names.map(_ + " Go_NamePi").mkString("(", ", ", ", go_scope Go_NamePi, go_stochastic Go_StochasticPi)")
        indent + "func " + agent + '_' + names.size + params + " Go_TrampolinePi[Go_NamePi] {" ::
        body.issue(indentʹ) :::
        s"${indent}}" ::
        Nil

      case init(Left(it)) if it.isEmpty =>
        Nil

      case init(Left(it)) =>
        indent + it.map(_._1).mkString("val ", ", ", "") + it.map(_._2).mkString(" = ", ", ", "")

      case init(Right(it)) =>
        indent + s"val ${it._1} = ${it._2.id}"

      case noop() =>
        Nil

      case nu(alloc(name)) =>
        s"${indent}bind $name = Go_NuPi()"

      case nu(Seq(names*)) =>
        names.map(s"${indent}bind " + _ + " = Go_NuPi()").toList

      case pace(amount, paceunit) =>
        indent + "bind _ = Go_PacePi(go_stochastic.Go_ctxPi, " + amount + ", time." + paceunit + ")"

      case par(ops) =>
        val sem = id
        s"${indent}val $sem = Go_NewSemaphorePi(${ops.size})" ::
        s"${indent}bind _ = $sem.TryAcquireN(${ops.size})" ::
        ops.map(_.issue(indentʹ)).flatMap(s"${indent}bind _ = Go_Spawn_ReleasablePi($sem, () => {" :: _ ::: s"${indent}})" :: Nil) :::
        s"${indent}bind _ = $sem.AcquireN(go_stochastic.Go_ctxPi, ${ops.size})" ::
        s"${indent}Go_returnPi" :: // LEAF
        Nil

      case pi(channel, λ(alloc(name)), None, rate, key) =>
        s"${indent}bind _ = $channel.Go_OutputPrefixPi($name, $rate, $key, go_scope.AsString(), go_stochastic)"

      case pi(channel, param, None, rate, key) =>
        s"${indent}bind _ = $channel.Go_OutputPrefixPi(${param.toName}, $rate, $key, go_scope.AsString(), go_stochastic)"

      case pi(channel, λ(Symbol(name)), Some(term), rate, key) =>
        val υidυ = id
        ( term match
          case null =>
            s"${indent}bind $υidυ = $channel.Go_InputPrefixPi($rate, $key, go_scope.AsString(), go_stochastic)"
          case Term.Name(fun) =>
            s"${indent}bind $υidυ = $channel.Go_InputPrefixPi_($rate, $key, go_scope.AsString(), $fun, go_stochastic)"
        ) ::
        s"""${indent}val (${if name.startsWith("_") then "_" else name}, _) = $υidυ.AsTuple()""" ::
        Nil

      case _: pi => ???

      case rep(parallelism, hd :: _ :: nu(alloc(_)) :: pi(channel, _, None, rate, key) :: tl, false, op) if parallelism < -1 =>
        rep(parallelism, hd :: nu(Seq(channel)) :: pi(channel, λ(Symbol(channel)), None, rate, key) :: tl.init, false, op).issue(indent)

      case rep(parallelism, hd :: nu(Seq(name)) :: pi(channel, _, None, rate, key) :: tl, false, op) if parallelism < -1 =>
        val υidυ = id
        val υidυʹ = id
        s"${indent}val $υidυ = ($name Go_NamePi, go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        op.issue(indentʹ) :::
        s"${indent}}" ::
        s"${indent}val $υidυʹ = () Go_TrampolinePi[Go_NamePi] => {" ::
        (hd :: tl).issue(indentʹ) :::
        s"${indentʹ}Go_returnPi" ::
        s"${indent}}" ::
        s"${indent}$channel.Go_BoundOutputPi(${-(parallelism%Int.MaxValue)}, $rate, $key, go_scope, $υidυ, $υidυʹ, go_stochastic)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, hd :: pi(channel, param, None, rate, key) :: tl, false, op) if parallelism < -1 =>
        val υidυ = id
        val υidυʹ = id
        val υidυʹʹ = id
        s"${indent}val $υidυ = (go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        op.issue(indentʹ) :::
        s"${indent}}" ::
        s"${indent}val $υidυʹ = () Go_TrampolinePi[Go_NamePi] => {" ::
        (hd :: tl).issue(indentʹ) :::
        s"${indentʹ}Go_returnPi" ::
        s"${indent}}" ::
        s"${indent}val $υidυʹʹ = () Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}Go_donePi(${param.toName})" ::
        s"${indent}}" ::
        s"${indent}$channel.Go_OutputPi(${-(parallelism%Int.MaxValue)}, $rate, $key, go_scope, $υidυ, $υidυʹ, $υidυʹʹ, go_stochastic)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, hd :: pi(channel, λ(Symbol(name)), Some(null), rate, key) :: tl, true, op) if parallelism < -1 =>
        val υidυ = id
        val υidυʹ = id
        s"${indent}val $υidυ = ($name Go_NamePi, go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        op.issue(indentʹ) :::
        s"${indent}}" ::
        s"${indent}val $υidυʹ = () Go_TrampolinePi[Go_NamePi] => {" ::
        (hd :: tl).issue(indentʹ) :::
        s"${indentʹ}Go_returnPi" ::
        s"${indent}}" ::
        s"${indent}$channel.Go_InputPi(${-(parallelism%Int.MaxValue)}, $rate, $key, go_scope, $υidυ, $υidυʹ, go_stochastic)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, hd :: pi(channel, λ(Symbol(name)), Some(Term.Name(fun)), rate, key) :: tl, true, op) if parallelism < -1 =>
        val υidυ = id
        val υidυʹ = id
        s"${indent}val $υidυ = ($name Go_NamePi, go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        op.issue(indentʹ) :::
        s"${indent}}" ::
        s"${indent}val $υidυʹ = () Go_TrampolinePi[Go_NamePi] => {" ::
        (hd :: tl).issue(indentʹ) :::
        s"${indentʹ}Go_returnPi" ::
        s"${indent}}" ::
        s"${indent}$channel.Go_InputPi_(${-(parallelism%Int.MaxValue)}, $rate, $key, go_scope, $υidυ, $υidυʹ, $fun, go_stochastic)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, hd :: tau(rate, key) :: tl, _, op) if parallelism < -1 =>
        val υidυ = id
        val υidυʹ = id
        s"${indent}val $υidυ = (go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        op.issue(indentʹ) :::
        s"${indent}}" ::
        s"${indent}val $υidυʹ = () Go_TrampolinePi[Go_NamePi] => {" ::
        (hd :: tl).issue(indentʹ) :::
        s"${indentʹ}Go_returnPi" ::
        s"${indent}}" ::
        s"${indent}Go_SilentPi(${-(parallelism%Int.MaxValue)}, $rate, $key, go_scope, $υidυ, $υidυʹ, go_stochastic)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, _, _, _) if parallelism < -1 => ???

      case rep(-1, it @ _ :: _ :: nu(alloc(_)) :: pi(name, _, _, _, _) :: _, false, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = ($name Go_NamePi, go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) ::: // allocate
        s"${indentʹ}bind go_scope = Go_UUIDPi()" ::
        s"${indentʹ}$υidυ($name, go_scope)" ::
        s"${indent}}" ::
        it.tail.tail.issue(indent) ::: // dont reallocate
        s"${indent}bind go_scope = Go_UUIDPi()" ::
        s"${indent}$υidυ($name, go_scope)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, it @ _ :: _ :: nu(alloc(_)) :: pi(name, _, _, _, _) :: _, false, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)" ::
        s"${indent}var $υidυ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = ($name Go_NamePi, go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_stochastic.Go_ctxPi)" ::
        s"${indentʹ}bind _ = Go_Spawn_ReleasablePi($sem, () => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) ::: // allocate
        s"${indentʹ}bind go_scope = Go_UUIDPi()" ::
        s"${indentʹ}$υidυ($name, go_scope)" ::
        s"${indent}}" ::
        it.tail.tail.issue(indent) ::: // dont reallocate
        s"${indent}bind go_scope = Go_UUIDPi()" ::
        s"${indent}$υidυ($name, go_scope)" :: // LEAF, tailcall
        Nil

      case rep(-1, it @ _ :: nu(Seq(name)) :: _, false, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = ($name Go_NamePi, go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}bind go_scope = Go_UUIDPi()" ::
        s"${indentʹ}$υidυ($name, go_scope)" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}bind go_scope = Go_UUIDPi()" ::
        s"${indent}$υidυ($name, go_scope)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, it @ _ :: nu(Seq(name)) :: _, false, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)" ::
        s"${indent}var $υidυ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = ($name Go_NamePi, go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_stochastic.Go_ctxPi)" ::
        s"${indentʹ}bind _ = Go_Spawn_ReleasablePi($sem, () => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}bind go_scope = Go_UUIDPi()" ::
        s"${indentʹ}$υidυ($name, go_scope)" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}bind go_scope = Go_UUIDPi()" ::
        s"${indent}$υidυ($name, go_scope)" :: // LEAF, tailcall
        Nil

      case rep(-1, it, false, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func (string) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = (go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}bind go_scope = Go_UUIDPi()" ::
        s"${indentʹ}$υidυ(go_scope)" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}bind go_scope = Go_UUIDPi()" ::
        s"${indent}$υidυ(go_scope)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, it, false, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)" ::
        s"${indent}var $υidυ func (string) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = (go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_stochastic.Go_ctxPi)" ::
        s"${indentʹ}bind _ = Go_Spawn_ReleasablePi($sem, () => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}bind go_scope = Go_UUIDPi()" ::
        s"${indentʹ}$υidυ(go_scope)" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}bind go_scope = Go_UUIDPi()" ::
        s"${indent}$υidυ(go_scope)" :: // LEAF, tailcall
        Nil

      case rep(-1, it @ _ :: pi(_, λ(Symbol(name)), _, _, _) :: _, true, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = ($name Go_NamePi, go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}bind go_scope = Go_UUIDPi()" ::
        s"${indentʹ}$υidυ($name, go_scope)" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}bind go_scope = Go_UUIDPi()" ::
        s"${indent}$υidυ($name, go_scope)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, it @ _ :: pi(_, λ(Symbol(name)), _, _, _) :: _, true, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)" ::
        s"${indent}var $υidυ func (Go_NamePi, Go_NamePi) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = ($name Go_NamePi, go_scope Go_NamePi) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_stochastic.Go_ctxPi)" ::
        s"${indentʹ}bind _ = Go_Spawn_ReleasablePi($sem, () => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}bind go_scope = Go_UUIDPi()" ::
        s"${indentʹ}$υidυ($name, go_scope)" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}bind go_scope = Go_UUIDPi()" ::
        s"${indent}$υidυ($name, go_scope)" :: // LEAF, tailcall
        Nil

      case _: rep => ???

      case sum(ops) =>
        par(ops).issue(indent)

      case tau(rate, key) =>
        indent + s"bind _ = Go_SilentPrefixPi($rate, $key, go_scope.AsString(), go_stochastic)"

      case void() =>
        s"${indent}Go_returnPi" // LEAF


object Gala:

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
        case it: Boolean => s"Go_BoolPi(${it.toString.toLowerCase})"
        case it: String => s"Go_StringPi(\"$it\")"
        case Lit.Int(it) => s"Go_IntPi($it)"
        case Lit.Long(it) => s"Go_IntPi($it)"
        case Lit.Double(it) => s"Go_NumberPi(big.NewFloat($it))"
        case Lit.Boolean(it) => s"Go_BoolPi($it)"
        case Lit.String(it) => s"Go_StringPi($it)"
        case Term.Name(it) =>
          val itʹ = it.replaceAll("[']([a-zA-Z_]['a-zA-Z0-9_\"]*)", "$1")
          context match
            case `number` => s"Go_NumberPi(big.NewFloat($itʹ))"
            case `boolean` => s"Go_BoolPi($itʹ)"
            case `string` => s"Go_StringPi($itʹ)"
            case _ => itʹ
        case _ => "Panic(\"N/A\")"
    }

  given Conversion[List[Gala], body] = body(_)

  given Conversion[Gala, List[Gala]] = {
    case it: body => it.body
    case it => List(it)
  }

  val rate: Any => String = {
    case w: Long if w < 0 => s"Go_ImmediatePi(${-w})"
    case r: BigDecimal => s"Go_TimedPi(big.NewFloat($r))"
    case w: Long => s"Go_PassivePi($w)"
    case None => rate(1L)
    case _ => ???
  }

  def defn(body: body): `(*)` => func =
    case `(*)`("Main") =>
      func("Main", Nil, body)
    case `(*)`(identifier, params*) =>
      func(identifier, params.map(_.asSymbol.name), body)
