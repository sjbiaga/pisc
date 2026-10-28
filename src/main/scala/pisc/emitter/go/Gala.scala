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


enum Gala:

  case alloc(var id: String = null)

  case body(lines: List[Gala])

  case call(agent: String, names: Seq[λ])

  case cases(semaphore: alloc, tryacquire: Boolean = false)

  case code(term: Term)

  case elvis(cond: ((λ, λ), Boolean), t: Gala.body, f: Gala.body)

  case func(agent: String, names: Seq[String], body: Gala.body)

  case init(assignment: List[(String, String)])

  case noop()

  case nu(names: String*)

  case pace(amount: Long, paceunit: String)

  case par(operands: List[Gala.body])

  case pi(channel: String, params: Seq[λ], polarity: Option[Term.Name])

  case rep(parallelism: Int, guard: List[Gala], polarity: Option[Boolean], operand: Gala.body)

  case sum(operands: List[Gala.body])

  case tau()

  case void()

  override def toString: String =
    val id = new helper.υidυ
    issue("")(using id()).mkString("\n")

  protected def issue(indent: String)(using => String): List[String] = issueʹ(indent)._1._1

  protected def issueʹ(indent: String)(using id: => String): ((List[String], String), List[String]) =
    val indentʹ = indent + " "*4

    given context = none

    given Conversion[String, ((List[String], String), List[String])] = List(_) -> indent -> Nil

    given Conversion[List[String], ((List[String], String), List[String])] = _ -> indent -> Nil

    this match

      case it @ alloc(_) =>
        it.id = id
        Nil

      case it: body =>
        val (pre, _, post) = it.body.foldLeft((List.empty[List[String]], indent, List.empty[List[String]])) {
          case ((preʹ, indentʹ, postʹ), itʹ) =>
            val ((preʹʹ, indentʹʹ), postʹʹ) = itʹ.issueʹ(indentʹ)
            (preʹʹ :: preʹ, indentʹʹ, postʹʹ :: postʹ)
        }
        pre.reverse.flatten ::: post.flatten

      case call(agent, names) =>
        indent + "bind _ = " + agent + '_' + names.size + (names.map(_.toName) :+ "go_context").mkString("(", ", ", ")") ::
        s"${indent}Go_returnPi" ::
        Nil

      case cases(sem, false) =>
        indent + s"val ${sem.id} = Go_NewSemaphorePi(1)"

      case cases(sem, true) =>
        indent + s"bind _ = ${sem.id}.TryAcquire1()"

      case code(lit @ (Lit.Int(_) | Lit.Long(_) | Lit.Double(_))) =>
        indent + s"""bind _ = Go_donePi(Go_NumberPi(big.NewFloat(""" + lit.value + ")))"

      case code(lit @ Lit.Boolean(_)) =>
        indent + s"""bind _ = Go_donePi(Go_BoolPi(""" + lit.value + "))"

      case code(lit @ Lit.String(_)) =>
        indent + s"""bind _ = Go_donePi(Go_StringPi(""" + lit.value + "))"

      case code(Term.Name(it)) =>
        indent + it.replaceAll("[']([a-zA-Z_]['a-zA-Z0-9_\"]*)", "$1")

      case code(_) => ???

      case elvis(((lhs, rhs), mismatch), t, body(Nil)) =>
        given context = context(lhs, rhs)
        ( if given_context == none
          then
            s"${indent}bind _ = if (${lhs.toName} ${if mismatch then '!' else '='}= ${rhs.toName}) {"
          else
            s"${indent}bind _ = if (${if mismatch then '!' else ' '}${lhs.toName}.Equal(${rhs.toName})) {"
        ) ::
        t.issue(indentʹ) :::
        s"${indent}} else Go_returnPi" ::
        s"${indent}Go_returnPi" :: // LEAF
        Nil

      case elvis(((lhs, rhs), mismatch), t, f) =>
        val tʹ = t.issue(indentʹ)
        val fʹ = f.issue(indentʹ)
        given context = context(lhs, rhs)
        ( if given_context == none
          then
            s"${indent}bind _ = if (${lhs.toName} ${if mismatch then '!' else '='}= ${rhs.toName}) {"
          else
            s"${indent}bind _ = if (${if mismatch then '!' else ' '}${lhs.toName}.Equal(${rhs.toName})) {"
        ) :: tʹ :::
        s"${indent}} else {" :: fʹ :::
        s"${indent}}" ::
        s"${indent}Go_returnPi" :: // LEAF
        Nil

      case func("Main", Nil, body) =>
        indent + s"func Main(args List[string], go_context context.Context) Go_TrampolinePi[Go_NamePi] {" ::
        body.issue(indentʹ) :::
        s"${indent}}" ::
        Nil

      case func(agent, names, body) =>
        val params = (names.map(_ + " Go_NamePi") :+ "go_context context.Context").mkString("(", ", ", ")")
        indent + "func " + agent + '_' + names.size + params + " Go_TrampolinePi[Go_NamePi] {" ::
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
        names.map(s"${indent}bind " + _ + " = " + "Go_donePi(Go_QueuePi(Go_NewBlockingQueuePi()))").toList

      case pace(amount, paceunit) =>
        indent + "bind _ = Go_PacePi(" + amount + ", time." + paceunit + ")"

      case par(ops) =>
        val sem = id
        s"${indent}val $sem = Go_NewSemaphorePi(${ops.size})" ::
        s"${indent}bind _ = $sem.TryAcquireN(${ops.size})" ::
        ops.map(_.issue(indentʹ)).flatMap(s"${indent}bind _ = Go_Spawn_ReleasablePi($sem, () => {" :: _ ::: s"${indent}})" :: Nil) :::
        s"${indent}bind _ = $sem.AcquireN(go_context, ${ops.size})" ::
        s"${indent}Go_returnPi" :: // LEAF
        Nil

      case pi(channel, Seq(λ(Lit.Null()), _*), None) =>
        s"${indent}bind _ = " + s"$channel match {" ::
        s"${indentʹ}case Go_QueuePi(it) => it.Go_EnqueuePi(Go_SlicePi())" ::
        s"${indentʹ}case it => Panic(it)" ::
        s"${indent}}" ::
        Nil

      case pi(channel, params, None) =>
        val items = params.map(_.toName).mkString(", ")
        s"${indent}bind _ = " + s"$channel match {" ::
        s"${indentʹ}case Go_QueuePi(it) => it.Go_EnqueuePi(Go_NamesPi($items))" ::
        s"${indentʹ}case it => Panic(it)" ::
        s"${indent}}" ::
        Nil

      case pi(channel, params, Some(term)) =>
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
                  s"${indentʹ}case Go_QueuePi($υidυ) => {" ::
                  s"${indentʹʹ}bind $υidυ = $υidυ.$dequeue" ::
                  s"${indentʹʹ}$υidυ.ToList() match {" ::
                  s"${indentʹʹʹ}case $conses => {" ::
                  Nil
        val post = s"${indentʹʹʹ}}" ::
                   s"${indentʹʹʹ}case it => Panic(it)" ::
                   s"${indentʹʹ}}" ::
                   s"${indentʹ}}" ::
                   s"${indentʹ}case it => Panic(it)" ::
                   s"${indent}}" ::
                   Nil
        pre -> indentʹʹʹʹ -> post

      // MODULO REPLICATION ////////////////////////////////////////////////////

      case rep(parallelism, pace :: nu(names*) :: pi(channel, _, _) :: code :: Nil, Some(false), op) if parallelism < -1 =>
        val indentʹʹ = indentʹ + " "*4
        val indentʹʹʹ = indentʹʹ + " "*4
        val υidυ = id
        val υidυʹ = id
        val υidυʹʹ = id
        val conses = names.foldRight("Nil()")("Cons("  + _ + ", " + _ + ")")
        s"${indent}val $υidυ func (List[Go_NamePi]) Go_TrampolinePi[Go_NamePi] = (($υidυʹʹ) => {" ::
        s"${indentʹ}$υidυʹʹ match {" ::
        s"${indentʹʹ}case $conses => {" ::
        op.issue(indentʹʹʹ) :::
        s"${indentʹʹ}}" ::
        s"${indentʹʹ}case it => Panic(it)" ::
        s"${indentʹ}}" ::
        s"${indent}})" ::
        s"${indent}val $υidυʹ func () Go_TrampolinePi[Go_NamePi] = () => {" ::
        (pace :: code :: Nil).issue(indentʹ) :::
        s"${indentʹ}Go_returnPi" ::
        s"${indent}}" ::
        s"${indent}$channel.Go_BoundOutputPi(go_context, ${names.size}, ${-(parallelism%Int.MaxValue)}, $υidυ, $υidυʹ)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, pace :: pi(channel, params, _) :: code :: Nil, Some(false), op) if parallelism < -1 =>
        val υidυ = id
        val υidυʹ = id
        val υidυʹʹ = id
        s"${indent}val $υidυ func () Go_TrampolinePi[Go_NamePi] = () => {" ::
        op.issue(indentʹ) :::
        s"${indent}}" ::
        s"${indent}val $υidυʹ func () Go_TrampolinePi[Go_NamePi] = () => {" ::
        (pace :: code :: Nil).issue(indentʹ) :::
        s"${indentʹ}Go_returnPi" ::
        s"${indent}}" ::
        s"${indent}val $υidυʹʹ func () List[Go_NamePi] = () => {" ::
        s"""${indentʹ}${params.map(_.toName).mkString("ListOf(", ", ", ")")}""" ::
        s"${indent}}" ::
        s"${indent}$channel.Go_OutputPi(go_context, ${-(parallelism%Int.MaxValue)}, $υidυ, $υidυʹ, $υidυʹʹ)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, pace :: tau() :: code :: Nil, None | Some(false), op) if parallelism < -1 =>
        val υidυ = id
        val υidυʹ = id
        s"${indent}val $υidυ func () Go_TrampolinePi[Go_NamePi] = () => {" ::
        op.issue(indentʹ) :::
        s"${indent}}" ::
        s"${indent}val $υidυʹ func () Go_TrampolinePi[Go_NamePi] = () => {" ::
        (pace :: code :: Nil).issue(indentʹ) :::
        s"${indentʹ}Go_returnPi" ::
        s"${indent}}" ::
        s"${indent}Go_SilentPi(go_context, ${-(parallelism%Int.MaxValue)}, $υidυ, $υidυʹ)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, pace :: pi(channel, params, Some(null)) :: code :: Nil, Some(true), op) if parallelism < -1 =>
        val indentʹʹ = indentʹ + " "*4
        val indentʹʹʹ = indentʹʹ + " "*4
        val υidυ = id
        val υidυʹ = id
        val υidυʹʹ = id
        val conses = params.map(_.asSymbol.name).foldRight("Nil()") {
          case (s"_$_", tail) => s"Cons(_, $tail)"
          case (head, tail) => s"Cons($head, $tail)"
        }
        s"${indent}val $υidυ func (List[Go_NamePi]) Go_TrampolinePi[Go_NamePi] = (($υidυʹʹ) => {" ::
        s"${indentʹ}$υidυʹʹ match {" ::
        s"${indentʹʹ}case $conses => {" ::
        op.issue(indentʹʹʹ) :::
        s"${indentʹʹ}}" ::
        s"${indentʹʹ}case it => Panic(it)" ::
        s"${indentʹ}}" ::
        s"${indent}})" ::
        s"${indent}val $υidυʹ func () Go_TrampolinePi[Go_NamePi] = () => {" ::
        (pace :: code :: Nil).issue(indentʹ) :::
        s"${indentʹ}Go_returnPi" ::
        s"${indent}}" ::
        s"${indent}$channel.Go_InputPi(go_context, ${-(parallelism%Int.MaxValue)}, $υidυ, $υidυʹ)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, pace :: pi(channel, params, Some(Term.Name(fun))) :: code :: Nil, Some(true), op) if parallelism < -1 =>
        val indentʹʹ = indentʹ + " "*4
        val indentʹʹʹ = indentʹʹ + " "*4
        val υidυ = id
        val υidυʹ = id
        val υidυʹʹ = id
        val conses = params.map(_.asSymbol.name).foldRight("Nil()") {
          case (s"_$_", tail) => s"Cons(_, $tail)"
          case (head, tail) => s"Cons($head, $tail)"
        }
        s"${indent}val $υidυ func (List[Go_NamePi]) Go_TrampolinePi[Go_NamePi] = (($υidυʹʹ) => {" ::
        s"${indentʹ}$υidυʹʹ match {" ::
        s"${indentʹʹ}case $conses => {" ::
        op.issue(indentʹʹʹ) :::
        s"${indentʹʹ}}" ::
        s"${indentʹʹ}case it => Panic(it)" ::
        s"${indentʹ}}" ::
        s"${indent}})" ::
        s"${indent}val $υidυʹ func () Go_TrampolinePi[Go_NamePi] = () => {" ::
        (pace :: code :: Nil).issue(indentʹ) :::
        s"${indentʹ}Go_returnPi" ::
        s"${indent}}" ::
        s"${indent}$channel.Go_InputPi_(go_context, ${-(parallelism%Int.MaxValue)}, $fun, $υidυ, $υidυʹ)" :: // LEAF, tailcall
        Nil

      case rep(parallelism, _, _, _) if parallelism < -1 => ???

      // REPLICATION ///////////////////////////////////////////////////////////

      case rep(-1, it @ _ :: nu(names*) :: _, Some(false), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sig = (", Go_NamePi"*names.size).substring(2)
        val sigʹ = names.map(_ + " Go_NamePi").mkString(", ")
        s"${indent}var $υidυ func ($sig) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = ($sigʹ) Go_TrampolinePi[Go_NamePi] => {" ::
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
        s"${indent}var $υidυ func ($sig) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = ($sigʹ) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_context)" ::
        s"${indentʹ}bind _ = Go_Spawn_ReleasablePi($sem, () => {" ::
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
        s"${indent}var $υidυ func () Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = () Go_TrampolinePi[Go_NamePi] => {" ::
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
        s"${indent}var $υidυ func () Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = () Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_context)" ::
        s"${indentʹ}bind _ = Go_Spawn_ReleasablePi($sem, () => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}$υidυ()" ::
        s"${indent}}" ::
        it.tail.issue(indent) :::
        s"${indent}$υidυ()" :: // LEAF, tailcall
        Nil

      case rep(-1, hd :: (it @ pi(_, params, _)) :: tl, Some(true), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val names = params.map(_.asSymbol.name).flatMap {
          case s"_$_" => None
          case it => Some(it)
        }
        val sig = if names.isEmpty then "" else (", Go_NamePi"*names.size).substring(2)
        val sigʹ = if names.isEmpty then "" else names.map(_ + " Go_NamePi").mkString(", ")
        s"${indent}var $υidυ func ($sig) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = ($sigʹ) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = Go_SpawnPi(() => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        {
          val ((pre, indentʹʹʹ), post) = it.issueʹ(indentʹ)
          hd.issue(indentʹ) :::
          pre :::
          tl.issue(indentʹʹʹ) :::
          s"${indentʹʹʹ}$υidυ(${names.mkString(", ")})" ::
          post
        } :::
        s"${indent}}" ::
        {
          val ((pre, indentʹʹʹ), post) = it.issueʹ(indent)
          pre :::
          tl.issue(indentʹʹʹ) :::
          s"${indentʹʹʹ}$υidυ(${names.mkString(", ")})" ::
          post
        } // LEAF: return in bind

      case rep(parallelism, hd :: (it @ pi(_, params, _)) :: tl, Some(true), op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        val sem = id
        val names = params.map(_.asSymbol.name).flatMap {
          case s"_$_" => None
          case it => Some(it)
        }
        val sig = if names.isEmpty then "" else (", Go_NamePi"*names.size).substring(2)
        val sigʹ = if names.isEmpty then "" else names.map(_ + " Go_NamePi").mkString(", ")
        s"${indent}val $sem = Go_NewSemaphorePi($parallelism)" ::
        s"${indent}var $υidυ func ($sig) Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = ($sigʹ) Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_context)" ::
        s"${indentʹ}bind _ = Go_Spawn_ReleasablePi($sem, () => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        {
          val ((pre, indentʹʹʹ), post) = it.issueʹ(indentʹ)
          hd.issue(indentʹ) :::
          pre :::
          tl.issue(indentʹʹʹ) :::
          s"${indentʹʹʹ}$υidυ(${names.mkString(", ")})" ::
          post
        } :::
        s"${indent}}" ::
        {
          val ((pre, indentʹʹʹ), post) = it.issueʹ(indent)
          pre :::
          tl.issue(indentʹʹʹ) :::
          s"${indentʹʹʹ}$υidυ(${names.mkString(", ")})" ::
          post
        } // LEAF: return in bind

      case rep(-1, it, None, op) =>
        val indentʹʹ = indentʹ + " "*4
        val υidυ = id
        s"${indent}var $υidυ func () Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = () Go_TrampolinePi[Go_NamePi] => {" ::
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
        s"${indent}var $υidυ func () Go_TrampolinePi[Go_NamePi]" ::
        s"${indent}$υidυ = () Go_TrampolinePi[Go_NamePi] => {" ::
        s"${indentʹ}bind _ = $sem.Acquire1(go_context)" ::
        s"${indentʹ}bind _ = Go_Spawn_ReleasablePi($sem, () => {" ::
        op.issue(indentʹʹ) :::
        s"${indentʹ}})" ::
        it.issue(indentʹ) :::
        s"${indentʹ}$υidυ()" ::
        s"${indent}}" ::
        s"${indent}$υidυ()" :: // LEAF, tailcall
        Nil

      case _: rep => ???

      /////////////////////////////////////////////////////////// replication //

      case sum(ops) =>
        val sem = id
        val semʹ = id
        val tryacquire = s"${indentʹ}bind _ = $semʹ.TryAcquire1()"
        s"${indent}val $sem = Go_NewSemaphorePi(${ops.size})" ::
        s"${indent}val $semʹ = Go_NewSemaphorePi(1)" ::
        s"${indent}bind _ = $sem.TryAcquireN(${ops.size})" ::
        ops.map(_.issue(indentʹ)).flatMap(s"${indent}bind _ = Go_Spawn_ReleasablePi($sem, () => {" :: tryacquire :: _ ::: s"${indent}})" :: Nil) :::
        s"${indent}bind _ = $sem.AcquireN(go_context, ${ops.size})" ::
        s"${indent}Go_returnPi" :: // LEAF
        Nil

      case tau() =>
        Nil

      case void() =>
        s"${indent}Go_returnPi" // LEAF


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

  def defn(body: body): `(*)` => func =
    case `(*)`("Main", _) =>
      func("Main", Nil, body)
    case `(*)`(identifier, _, params*) =>
      func(identifier, params.map(_.asSymbol.name), body)
