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
package parser
package ast

import scala.meta.Term

import scala.util.parsing.input.{ NoPosition, Position }

import BioAmbients.{ Free, Sum }
import Encoding.Definition
import Pre.*, AST.*


enum AST extends Positional with Free:

  case +(scaling: Int, choices: AST.∥ *)
        (using override val lc: Int => (Int, Int))
      extends AST with Sum

  case ∥(scaling: Int, components: AST.`.`*)
        (using override val lc: Int => (Int, Int))

  case `.`(end: AST.+ | -, prefixes: Pre*)
          (using override val lc: Int => (Int, Int))

  case ?:(cond: ((λ, λ), Boolean), t: AST.+, f: Option[AST.+])
         (using override val lc: Int => (Int, Int))

  case !(parallelism: Int,
         pace: Option[(Long, String)],
         guard: Option[μ | ζ],
         sum: AST.+)
        (using override val lc: Int => (Int, Int))

  case `[]`(label: Option[String], sum: AST.+)
           (using override val lc: Int => (Int, Int))

  case `⟦⟧`(definition: Definition,
            sum: AST.+,
            xid: String = null,
            pointers: List[Symbol] = Nil)
           (using override val lc: Int => (Int, Int))

  case `{}`(identifier: String,
            pointers: List[Symbol],
            agent: Boolean = false,
            params: λ*)
           (using override val lc: Int => (Int, Int))

  case `(*)`(identifier: String,
             params: λ*)
            (using override val lc: Int => (Int, Int))

  override def toString: String = this match
    case ∅() => "()"
    case +(-1, choices*) => choices.mkString(" + ")
    case +(sc, choices*) => "" + sc + " * " + choices.mkString(" + ")

    case ∥(-1, components*) => components.mkString(" | ")
    case ∥(sc, components*) => "" + sc + " * " + components.mkString(" | ")

    case `.`(∅()) => "()"
    case `.`(∅(), prefixes*) => prefixes.mkString(" ") + " ()"
    case `.`(end: +, prefixes*) =>
      prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + "(" + end + ")"
    case `.`(end, prefixes*) =>
      prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + end

    case ?:(cond, t, f) =>
      val test = "" + cond._1._1 + (if cond._2 then " ≠ " else " = ") + cond._1._2
      if f.isEmpty
      then
        "[ " + test + " ] " + t
      else
        "if " + test + " then " + t + " else " + f.get

    case !(-1, _, guard, sum) =>
      "!" + guard.map("." + _).getOrElse("") + sum

    case !(parallelism, _, guard, sum) if parallelism < -1 =>
      s"¡${-(parallelism%Int.MaxValue)}*" + guard.map("." + _).getOrElse("") + sum

    case !(parallelism, _, guard, sum) =>
      s"!$parallelism*" + guard.map("." + _).getOrElse("") + sum

    case `[]`(label, sum) =>
      label.getOrElse("") + "[ " + sum + " ]"

    case `⟦⟧`(_, sum, _, _) =>
      sum.toString

    case `{}`(identifier, pointers, agent, params*) =>
      val ps = if agent then params.mkString("(", ", ", ")") else ""
      s"""$identifier$ps{${pointers.map(_.name).mkString(", ")}}"""

    case `(*)`(identifier, params*) =>
      val args = params.map(_.toTerm).toList
      Term.Apply(Term.Name(identifier), Term.ArgClause(args)).toString


object AST:

  given `_+_`: {} with
    extension (self: +)
      def cc(scaling: Int = self.scaling,
             choices: Seq[∥] = self.choices): + =
        import Cast.given
        val sum: + = `+`(scaling, choices*)(using self.lc).setPos(self.pos).free = self.free
        sum.enabled = self.enabled

  given `_∥_`: {} with
    extension (self: ∥)
      def cc(scaling: Int = self.scaling,
             components: Seq[`.`] = self.components): ∥ =
        import Cast.given
        ∥(scaling, components*)(using self.lc).setPos(self.pos).free = self.free

  given `_._`: {} with
    extension (self: `.`)
      def cc(end: + | - = self.end,
             prefixes: Seq[Pre] = self.prefixes): `.` =
        import Cast.given
        `.`(end, prefixes*)(using self.lc).setPos(self.pos).free = self.free

  given `_?:_`: {} with
    extension (self: ?:)
      def cc(cond: ((λ, λ), Boolean) = self.cond,
             t: AST.+ = self.t,
             f: Option[AST.+] = self.f): ?: =
        import Cast.given
        `?:`(cond, t, f)(using self.lc).setPos(self.pos).free = self.free

  given `_!_`: {} with
    extension (self: !)
      def cc(parallelism: Int = self.parallelism,
             pace: Option[(Long, String)] = self.pace,
             guard: Option[μ | ζ] = self.guard,
             sum: AST.+ = self.sum): ! =
        import Cast.given
        `!`(parallelism, pace, guard, sum)(using self.lc).setPos(self.pos).free = self.free

  given `_[]_`: {} with
    extension (self: `[]`)
      def cc(label: Option[String] = self.label,
             sum: AST.+ = self.sum): `[]` =
        import Cast.given
        `[]`(label, sum)(using self.lc).setPos(self.pos).free = self.free

  given `_⟦⟧_`: {} with
    extension (self: `⟦⟧`)
      def cc(definition: Definition = self.definition,
             sum: AST.+ = self.sum,
             xid: String = self.xid,
             pointers: List[Symbol] = self.pointers): `⟦⟧` =
        import Cast.given
        `⟦⟧`(definition, sum, xid, pointers)(using self.lc).setPos(self.pos).free = self.free

  given `_{}_`: {} with
    extension (self: `{}`)
      def cc(identifier: String = self.identifier,
             pointers: List[Symbol] = self.pointers,
             agent: Boolean = self.agent,
             params: Seq[λ] = self.params): `{}` =
        import Cast.given
        `{}`(identifier, pointers, agent, params*)(using self.lc).setPos(self.pos).free = self.free

  given `_(*)_`: {} with
    extension (self: `(*)`)
      def cc(identifier: String = self.identifier,
             params: Seq[λ] = self.params): `(*)` =
        import Cast.given
        `(*)`(identifier, params*)(using self.lc).setPos(self.pos).free = self.free

  object ∅ :
    def apply(pos: Position = NoPosition)(using Int => (Int, Int)): + = `+`(-1).setPos(pos)
    def unapply(self: AST): Boolean = self match
      case it: (+ | `⟦⟧`) => it.isVoid
      case _ => false

  extension (self: + | `⟦⟧`)
    def isVoid: Boolean = self match
      case +(_) => true
      case `⟦⟧`(_, sum, _, _) => sum.isVoid
      case sum: + => sum.choices.forall(_.components.forall { case `.`(it: (+ | `⟦⟧`)) => it.isVoid case _ => false })


  // functions

  extension [T <: AST](ast: T)

    def foreach(g: AST => Unit): Unit =

      g(ast)

      ast match

        case ∅() =>

        case +(_, choices*) =>
          choices.foreach(_.foreach(g))

        case ∥(_, components*) =>
          components.foreach(_.foreach(g))

        case `.`(end, _*) =>
          end.foreach(g)

        case ?:(_, t, f) =>
          t.foreach(g)
          f.foreach(_.foreach(g))

        case !(_, _, _, sum) =>
          sum.foreach(g)

        case `[]`(_, sum) =>
          sum.foreach(g)

        case `⟦⟧`(_, sum, _, _) =>
          sum.foreach(g)

        case _ =>

    def foreachʹ(g: AST => Unit = { _ => })(h: PartialFunction[AST, Unit] = PartialFunction.empty): Unit =

      h.applyOrElse(ast, {

        case ∅() =>

        case +(_, choices*) =>
          choices.foreach(_.foreachʹ(g)(h))

        case ∥(_, components*) =>
          components.foreach(_.foreachʹ(g)(h))

        case `.`(end, _*) =>
          end.foreachʹ(g)(h)

        case ?:(_, t, f) =>
          t.foreachʹ(g)(h)
          f.foreach(_.foreachʹ(g)(h))

        case !(_, _, _, sum) =>
          sum.foreachʹ(g)(h)

        case `[]`(_, sum) =>
          sum.foreachʹ(g)(h)

        case `⟦⟧`(_, sum, _, _) =>
          sum.foreachʹ(g)(h)

        case _ => g(ast)

      })

    def mapreduce[R](g: AST => R)(h: (R, R) => R): R =

      ast match

        case ∅() => g(ast)

        case +(_, choices*) =>
          choices.map(_.mapreduce(g)(h)).reduce(h)

        case ∥(_, components*) =>
          components.map(_.mapreduce(g)(h)).reduce(h)

        case it @ `.`(end, _*) =>
          h(g(it), end.mapreduce(g)(h))

        case it @ ?:(_, t, Some(f)) =>
          h(h(g(it), t.mapreduce(g)(h)), f.mapreduce(g)(h))

        case it @ ?:(_, t, _) =>
          h(g(it), t.mapreduce(g)(h))

        case it @ !(_, _, _, sum) =>
          h(g(it), sum.mapreduce(g)(h))

        case it @ `[]`(_, sum) =>
          h(g(it), sum.mapreduce(g)(h))

        case it @ `⟦⟧`(_, sum, _, _) =>
          h(g(it), sum.mapreduce(g)(h))

        case _ => g(ast)

    def mapreduceʹ[R](d: AST => R)(g: PartialFunction[AST, R])(h: (R, R) => R): R =

      ast match

        case ∅() => d(ast)

        case it @ +(_, choices*) =>
          h(g.applyOrElse(it, d), choices.map(_.mapreduceʹ(d)(g)(h)).reduce(h))

        case it @ ∥(_, components*) =>
          h(g.applyOrElse(it, d), components.map(_.mapreduceʹ(d)(g)(h)).reduce(h))

        case it @ `.`(end, _*) =>
          h(d(it), end.mapreduceʹ(d)(g)(h))

        case it @ ?:(_, t, Some(f)) =>
          h(h(d(it), t.mapreduceʹ(d)(g)(h)), f.mapreduceʹ(d)(g)(h))

        case it @ ?:(_, t, _) =>
          h(d(it), t.mapreduceʹ(d)(g)(h))

        case it @ !(_, _, _, sum) =>
          h(d(it), sum.mapreduceʹ(d)(g)(h))

        case it @ `[]`(_, sum) =>
          h(d(it), sum.mapreduceʹ(d)(g)(h))

        case it @ `⟦⟧`(_, sum, _, _) =>
          h(d(it), sum.mapreduceʹ(d)(g)(h))

        case _ => d(ast)

    def map(g: AST => AST)(h: AST => AST = identity): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ +(_, choices*) =>
          it.cc(choices = choices.map(_.map(g)(h)))

        case it @ ∥(_, components*) =>
          it.cc(components = components.map(_.map(g)(h)))

        case it @ `.`(end, _*) =>
          h(it.cc(end = end.map(g)(h)))

        case it @ ?:(_, t, f) =>
          h(it.cc(t = t.map(g)(h), f = f.map(_.map(g)(h))))

        case it @ !(_, _, _, sum) =>
          h(it.cc(sum = sum.map(g)(h)))

        case it @ `[]`(_, sum) =>
          h(it.cc(sum = sum.map(g)(h)))

        case it @ `⟦⟧`(_, sum, _, _) =>
          h(it.cc(sum = sum.map(g)(h)))

        case _ => h(ast)

    def mapʹ(g: AST => AST)(h: AST => AST): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ +(_, choices*) =>
          it.cc(choices = choices.map(_.mapʹ(g)(h)))

        case it @ ∥(_, components*) =>
          it.cc(components = components.map(_.mapʹ(g)(h)))

        case it: `.` =>
          val itʹ @ `.`(end, _*) = h(it)
          itʹ.cc(end = end.mapʹ(g)(h))

        case it: ?: =>
          val itʹ @ ?:(_, t, f) = h(it)
          itʹ.cc(t = t.mapʹ(g)(h), f = f.map(_.mapʹ(g)(h)))

        case it: ! =>
          val itʹ @ !(_, _, _, sum) = h(it)
          itʹ.cc(sum = sum.mapʹ(g)(h))

        case it: `[]` =>
          val itʹ @ `[]`(_, sum) = h(it)
          itʹ.cc(sum = sum.mapʹ(g)(h))

        case it: `⟦⟧` =>
          val itʹ @ `⟦⟧`(_, sum, _, _) = h(it)
          itʹ.cc(sum = sum.mapʹ(g)(h))

        case _ => h(ast)

    def mapʹʹ(g: AST => AST)(h: AST => (AST, Boolean)): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ +(_, choices*) =>
          it.cc(choices = choices.map(_.mapʹʹ(g)(h)))

        case it @ ∥(_, components*) =>
          it.cc(components = components.map(_.mapʹʹ(g)(h)))

        case it: `.` =>
          h(it) match
            case (itʹ @ `.`(end, _*), false) =>
              itʹ.cc(end = end.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: ?: =>
          h(it) match
            case (itʹ @ ?:(_, t, f), false) =>
              itʹ.cc(t = t.mapʹʹ(g)(h), f = f.map(_.mapʹʹ(g)(h)))
            case (itʹ, _) => itʹ

        case it: ! =>
          h(it) match
            case (itʹ @ !(_, _, _, sum), false) =>
              itʹ.cc(sum = sum.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: `[]` =>
          h(it) match
            case (itʹ @ `[]`(_, sum), false) =>
              itʹ.cc(sum = sum.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case it: `⟦⟧` =>
          h(it) match
            case (itʹ @ `⟦⟧`(_, sum, _, _), false) =>
              itʹ.cc(sum = sum.mapʹʹ(g)(h))
            case (itʹ, _) => itʹ

        case _ => h(ast)._1
