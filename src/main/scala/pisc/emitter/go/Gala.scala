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

  case bind(channel: String, name: λ, polarity: Option[Term.Name])
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

  override def toString: String = toString("")

  protected def toString(indent: String): String =
    val indentʹ = indent + " "*4

    given context = none

    this match

      case bind(channel, λ(Lit.Null()), None) =>
        val enqueue = s"${indentʹ}case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NothingPi())\n${indentʹ}case it => Panic(it)"
        s"${indent}bind _ = " + s"$channel match {\n$enqueue\n${indent}}"

      case bind(channel, name, None) =>
        val enqueue = s"${indentʹ}case Go_QueuePi(it) => it.Go_EnqueuePi(${name.toName})\n${indentʹ}case it => Panic(it)"
        s"${indent}bind _ = " + s"$channel match {\n$enqueue\n${indent}}"

      case bind(channel, λ(Symbol(s"_$_")), Some(null)) =>
        val dequeue = s"${indentʹ}case Go_QueuePi(it) => it.Go_DequeuePi()\n${indentʹ}case it => Panic(it)"
        s"${indent}bind _ = " + s"$channel match {\n$dequeue\n${indent}}"

      case bind(channel, λ(Symbol(s"_$_")), Some(Term.Name(fun))) =>
        val dequeue = s"${indentʹ}case Go_QueuePi(it) => it.Go_DequeuePi_($fun)\n${indentʹ}case it => Panic(it)"
        s"${indent}bind _ = " + s"$channel match {\n$dequeue\n${indent}}"

      case bind(channel, λ(Symbol(name)), Some(null)) =>
        val dequeue = s"${indentʹ}case Go_QueuePi(it) => it.Go_DequeuePi()\n${indentʹ}case it => Panic(it)"
        s"${indent}bind " + name + " = " + s"$channel match {\n$dequeue\n${indent}}"

      case bind(channel, λ(Symbol(name)), Some(Term.Name(fun))) =>
        val dequeue = s"${indentʹ}case Go_QueuePi(it) => it.Go_DequeuePi_($fun)\n${indentʹ}case it => Panic(it)"
        s"${indent}bind " + name + " = " + s"$channel match {\n$dequeue\n${indent}}"

      case bind(_, _, _) => ???

      case it: body =>
        it.body.map(_.toString(indent)).mkString("\n")

      case call(agent, names) =>
        indent + "bind _ = " + agent + '_' + names.size + names.map(_.toName).mkString("(go_context, ", ", ", ")")

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
        if mismatch
        then
          s"${indent}bind _ = if (!${lhs.toName}.Equal(${rhs.toName})) {\n${t.toString(indentʹ)}\n${indentʹ}Some(true)\n${indent}} else Some(false)"
        else
          s"${indent}bind _ = if (${lhs.toName}.Equal(${rhs.toName})) {\n${t.toString(indentʹ)}\n${indentʹ}Some(true)\n${indent}} else Some(false)"

      case elvis(((lhs, rhs), mismatch), t, f) =>
        val tʹ = t.toString(indentʹ)
        val fʹ = f.toString(indentʹ)
        given context = context(lhs, rhs)
        if mismatch
        then
          s"${indent}bind _ = if (${lhs.toName}.Equal(${rhs.toName})) {\n$fʹ\n${indentʹ}Some(true)\n${indent}} else {\n$tʹ\n${indentʹ}Some(false)\n${indent}}"
        else
          s"${indent}bind _ = if (${lhs.toName}.Equal(${rhs.toName})) {\n$tʹ\n${indentʹ}Some(true)\n${indent}} else {\n$fʹ\n${indentʹ}Some(false)\n${indent}}"

      case func("Main", Nil, body) =>
        indent + s"func Main(go_context context.Context, args List[string]) Option[bool] {\n" + body.toString(indentʹ) + s"\n${indentʹ}Some(true)\n${indent}}"

      case func(agent, names, body) =>
        indent + "func " + agent + '_' + names.size + names.map(_ + " Go_NamePi").mkString("(go_context context.Context, ", ", ", ")") + " Option[bool] {\n" + body.toString(indentʹ) + s"\n${indentʹ}Some(true)\n${indent}}"

      case init(it) if it.isEmpty => ""

      case init(it) =>
        indent + it.map(_._1).mkString("val ", ", ", "") + it.map(_._2).mkString(" = ", ", ", "")

      case noop() => ""

      case nu(names*) =>
        names.map(s"${indent}bind " + _ + " = " + "Some(Go_QueuePi(Go_NewBlockingQueuePi()))").mkString("\n")

      case pace(amount, paceunit) =>
        indent + "bind _ = Go_PacePi(" + amount + ", time." + paceunit + ")"

      case par(ops) =>
        val sem = id
        val acquire = s"${indent}bind _ = $sem.AcquireN(go_context, ${ops.size})"
        val releasable = s"${indentʹ}use $id = Go_ReleasablePi($sem)"
        s"${indent}bind $sem = Some(Go_NewSemaphorePi(${ops.size}))\n" +
        ops.map(_.toString(indentʹ)).map(s"${indent}bind _ = Go_SpawnPi(() => {\n$releasable\n" + _ + s"\n${indentʹ}Some(true)\n${indent}})").mkString(acquire + "\n", "\n", "\n" + acquire)

      case rep(-1, it @ _ :: nu(name) :: _, Some(false), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func (Go_NamePi) Option[bool]\n" +
        s"${indent}$υidυ = ($name Go_NamePi) Option[bool] => {\n" +
        s"${indentʹ}bind _ = Go_SpawnPi(() => {\n" + op.toString(indentʹʹ) + s"\n${indentʹʹ}Some(true)\n${indentʹ}})\n" +
        (it: body).toString(indentʹ) + "\n" +
        s"${indentʹ}$υidυ($name)\n${indent}}\n" + (it.tail: body).toString(indent) + s"\n${indent}bind _ = $υidυ($name)"

      case rep(parallelism, it @ _ :: nu(name) :: _, Some(false), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        val releasable = s"${indentʹʹ}use $id = Go_ReleasablePi($sem)"
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)\n" +
        s"${indent}var $υidυ func (Go_NamePi) Option[bool]\n" +
        s"${indent}$υidυ = ($name Go_NamePi) Option[bool] => {\n${indentʹ}bind _ = $sem.Acquire1(go_context)\n" +
        s"${indentʹ}bind _ = Go_SpawnPi(() => {\n$releasable\n" + op.toString(indentʹʹ) + s"\n${indentʹʹ}Some(true)\n${indentʹ}})\n" +
        (it: body).toString(indentʹ) + "\n" +
        s"${indentʹ}$υidυ($name)\n${indent}}\n" + (it.tail: body).toString(indent) + s"\n${indent}bind _ = $υidυ($name)"

      case rep(-1, it, Some(false), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func () Option[bool]\n" +
        s"${indent}$υidυ = () Option[bool] => {\n" +
        s"${indentʹ}bind _ = Go_SpawnPi(() => {\n" + op.toString(indentʹʹ) + s"\n${indentʹʹ}Some(true)\n${indentʹ}})\n" +
        (it: body).toString(indentʹ) + "\n" +
        s"${indentʹ}$υidυ()\n${indent}}\n" + (it.tail: body).toString(indent) + s"\n${indent}bind _ = $υidυ()"

      case rep(parallelism, it, Some(false), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        val releasable = s"${indentʹʹ}use $id = Go_ReleasablePi($sem)"
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)\n" +
        s"${indent}var $υidυ func () Option[bool]\n" +
        s"${indent}$υidυ = () Option[bool] => {\n${indentʹ}bind _ = $sem.Acquire1(go_context)\n" +
        s"${indentʹ}bind _ = Go_SpawnPi(() => {\n$releasable\n" + op.toString(indentʹʹ) + s"\n${indentʹʹ}Some(true)\n${indentʹ}})\n" +
        (it: body).toString(indentʹ) + "\n" +
        s"${indentʹ}$υidυ()\n${indent}}\n" + (it.tail: body).toString(indent) + s"\n${indent}bind _ = $υidυ()"

      case rep(-1, it @ _ :: bind(_, λ(Symbol(name)), _) :: _, Some(true), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func (Go_NamePi) Option[bool]\n" +
        s"${indent}$υidυ = ($name Go_NamePi) Option[bool] => {\n" +
        s"${indentʹ}bind _ = Go_SpawnPi(() => {\n" + op.toString(indentʹʹ) + s"\n${indentʹʹ}Some(true)\n${indentʹ}})\n" +
        (it: body).toString(indentʹ) + "\n" +
        s"${indentʹ}$υidυ($name)\n${indent}}\n" + (it.tail: body).toString(indent) + s"\n${indent}bind _ = $υidυ($name)"

      case rep(parallelism, it @ _ :: bind(_, λ(Symbol(name)), _) :: _, Some(true), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        val releasable = s"${indentʹʹ}use $id = Go_ReleasablePi($sem)"
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)\n" +
        s"${indent}var $υidυ func (Go_NamePi) Option[bool]\n" +
        s"${indent}$υidυ = ($name Go_NamePi) Option[bool] => {\n${indentʹ}bind _ = $sem.Acquire1(go_context)\n" +
        s"${indentʹ}bind _ = Go_SpawnPi(() => {\n$releasable\n" + op.toString(indentʹʹ) + s"\n${indentʹʹ}Some(true)\n${indentʹ}})\n" +
        (it: body).toString(indentʹ) + "\n" +
        s"${indentʹ}$υidυ($name)\n${indent}}\n" + (it.tail: body).toString(indent) + s"\n${indent}bind _ = $υidυ($name)"

      case rep(-1, it, None, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func () Option[bool]\n" +
        s"${indent}$υidυ = () Option[bool] => {\n" +
        s"${indentʹ}bind _ = Go_SpawnPi(() => {\n" + op.toString(indentʹʹ) + s"\n${indentʹʹ}Some(true)\n${indentʹ}})\n" +
        (it: body).toString(indentʹ) + "\n" +
        s"${indentʹ}$υidυ()\n${indent}}\n${indent}bind _ = $υidυ()"

      case rep(parallelism, it, None, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        val releasable = s"${indentʹʹ}use $id = Go_ReleasablePi($sem)"
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)\n" +
        s"${indent}var $υidυ func () Option[bool]\n" +
        s"${indent}$υidυ = () Option[bool] => {\n${indentʹ}bind _ = $sem.Acquire1(go_context)\n" +
        s"${indentʹ}bind _ = Go_SpawnPi(() => {\n$releasable\n" + op.toString(indentʹʹ) + s"\n${indentʹʹ}Some(true)\n${indentʹ}})\n" +
        (it: body).toString(indentʹ) + "\n" +
        s"${indentʹ}$υidυ()\n${indent}}\n${indent}bind _ = $υidυ()"

      case _: rep => ???

      case sum(ops) =>
        val sem = id
        val semʹ = id
        val releasable = s"${indentʹ}use $id = Go_ReleasablePi($sem)"
        val tryacquire = s"${indentʹ}bind _ = $semʹ.TryAcquire1()"
        val acquire = s"${indent}bind _ = $sem.AcquireN(go_context, ${ops.size})"
        s"${indent}bind $sem = Some(Go_NewSemaphorePi(${ops.size}))\n" +
        s"${indent}bind $semʹ = Some(Go_NewSemaphorePi(1))\n" +
        ops.map(_.toString(indentʹ)).map(s"${indent}bind _ = Go_SpawnPi(() => {\n$releasable\n$tryacquire\n" + _ + s"\n${indentʹ}Some(true)\n${indent}})").mkString(acquire + "\n", "\n", "\n" + acquire)

      case tau() => ""


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
