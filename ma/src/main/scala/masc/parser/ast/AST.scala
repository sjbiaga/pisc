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

package masc
package parser
package ast

import scala.meta.Term

import scala.util.parsing.input.{ NoPosition, Position }

import Expression.Code
import Ambient.Free
import Encoding.Definition
import Pre.*, AST.*


enum AST extends Positional with Free:

  case ∥(scaling: Int, components: AST.`.`*)
        (using override val lc: Int => (Int, Int))

  case `.`(end: AST.∥ | -, prefixes: Pre*)
          (using override val lc: Int => (Int, Int))

  case <>(code: Option[Code], path: Cap*)
         (using override val lc: Int => (Int, Int))

  case !(parallelism: Int,
         pace: Option[(Long, String)],
         guard: Option[String],
         par: AST.∥)
        (using override val lc: Int => (Int, Int))

  case `[]`(amb: String, par: AST.∥)
           (using override val lc: Int => (Int, Int))

  case `go.`(amb: String, par: AST.∥)
            (using override val lc: Int => (Int, Int))

  case `⟦⟧`(definition: Definition,
            par: AST.∥,
            xid: String = null,
            pointers: List[String] = Nil)
           (using override val lc: Int => (Int, Int))

  case `{}`(identifier: String,
            pointers: List[String],
            agent: Boolean = false,
            params: String*)
           (using override val lc: Int => (Int, Int))

  case `(*)`(identifier: String,
             qual: List[String],
             params: String*)
            (using override val lc: Int => (Int, Int))

  override def toString: String = this match
    case ∅() => "()"

    case ∥(-1, components*) => components.mkString(" | ")
    case ∥(sc, it) => sc + " * " + it
    case ∥(sc, components*) => sc + " * " + components.mkString("( ", " | ", " )")

    case `.`(it @ ∅()) => it.toString
    case `.`(it @ ∅(), prefixes*) => prefixes.mkString(" ") + " " + it
    case `.`(end: ∥, prefixes*) =>
      prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + "(" + end + ")"
    case `.`(end, prefixes*) =>
      prefixes.mkString(" ") + (if prefixes.isEmpty then "" else " ") + end

    case <>(_, path*) => path.mkString("<", ". ", ">")

    case !(_, _, guard, par) => "!" + guard.map(".(" + _ + ").").getOrElse("") + par

    case `[]`(amb, ∅()) => amb + " [ ]"
    case `[]`(amb, par) => amb + " [ " + par + " ]"

    case `go.`(amb, par) => "go " + amb + "." + par

    case `⟦⟧`(_, par, _, _) =>
      par.toString

    case `{}`(identifier, pointers, agent, params*) =>
      val ps = if agent then params.mkString("(", ", ", ")") else ""
      s"""$identifier$ps{${pointers.mkString(", ")}}"""

    case `(*)`(identifier, qual, params*) =>
      import emitter.shared.Meta.\
      val args = params.map(\(_)).toList
      val term = qual match
        case h :: t => (t.map(\(_)) :+ \("π") :+ \(identifier)).foldLeft(h: Term)(Term.Select(_, _))
        case _ => \(identifier)
      Term.Apply(term, Term.ArgClause(args)).toString


object AST:

  given `_∥_`: {} with
    extension (self: ∥)
      def cc(scaling: Int = self.scaling,
             components: Seq[`.`] = self.components): ∥ =
        import Cast.given
        ∥(scaling, components*)(using self.lc).setPos(self.pos).free = self.free

  given `_._`: {} with
    extension (self: `.`)
      def cc(end: ∥ | - = self.end,
             prefixes: Seq[Pre] = self.prefixes): `.` =
        import Cast.given
        `.`(end, prefixes*)(using self.lc).setPos(self.pos).free = self.free

  given `_<>_`: {} with
    extension (self: <>)
      def cc(code: Option[Code] = self.code,
             path: Seq[Cap] = self.path): <> =
        import Cast.given
        <>(code, path*)(using self.lc).setPos(self.pos).free = self.free

  given `_!_`: {} with
    extension (self: !)
      def cc(parallelism: Int = self.parallelism,
             pace: Option[(Long, String)] = self.pace,
             guard: Option[String] = self.guard,
             par: ∥ = self.par): ! =
        import Cast.given
        `!`(parallelism, pace, guard, par)(using self.lc).setPos(self.pos).free = self.free

  given `_[]_`: {} with
    extension (self: `[]`)
      def cc(amb: String = self.amb,
             par: ∥ = self.par): `[]` =
        import Cast.given
        `[]`(amb, par)(using self.lc).setPos(self.pos).free = self.free

  given `_go._`: {} with
    extension (self: `go.`)
      def cc(amb: String = self.amb,
             par: ∥ = self.par): `go.` =
        import Cast.given
        `go.`(amb, par)(using self.lc).setPos(self.pos).free = self.free

  given `_⟦⟧_`: {} with
    extension (self: `⟦⟧`)
      def cc(definition: Definition = self.definition,
             par: ∥ = self.par,
             xid: String = self.xid,
             pointers: List[String] = self.pointers): `⟦⟧` =
        import Cast.given
        `⟦⟧`(definition, par, xid, pointers)(using self.lc).setPos(self.pos).free = self.free

  given `_{}_`: {} with
    extension (self: `{}`)
      def cc(identifier: String = self.identifier,
             pointers: List[String] = self.pointers,
             agent: Boolean = self.agent,
             params: Seq[String] = self.params): `{}` =
        import Cast.given
        `{}`(identifier, pointers, agent, params*)(using self.lc).setPos(self.pos).free = self.free

  given `_(*)_`: {} with
    extension (self: `(*)`)
      def cc(identifier: String = self.identifier,
             qual: List[String] = self.qual,
             params: Seq[String] = self.params): `(*)` =
        import Cast.given
        `(*)`(identifier, qual, params*)(using self.lc).setPos(self.pos).free = self.free

  object ∅ :
    def apply(pos: Position = NoPosition)(using Int => (Int, Int)): ∥ = ∥(-1).setPos(pos)
    def unapply(self: AST): Boolean = self match
      case it: ∥ => it.isVoid
      case _ => false


  // functions

  extension (self: ∥ | - | `.`)
    def isVoid: Boolean = self match
      case ∥(_) => true
      case par: ∥ => par.components.forall(_.isVoid)
      case `.`(end, ps*) if ps.forall { case _: ν => true case _ => false } => end.isVoid
      case !(_, _, None, par) => par.isVoid
      case `[]`(_, par) => par.isVoid
      case `go.`(_, par) => par.isVoid
      case `⟦⟧`(_, par, _, _) => par.isVoid
      case _ => false

  extension (par: ∥)
    def isVoid: Boolean = par match
      case ∥(_) => true
      case _ => par.components.forall { case `.`(par: ∥) => par.isVoid case _ => false }

  extension [T <: AST](ast: T)

    def foreach(g: AST => Unit): Unit =

      g(ast)

      ast match

        case ∅() =>

        case ∥(_, components*) =>
          components.foreach(_.foreach(g))

        case `.`(end, _*) =>
          end.foreach(g)

        case !(_, _, _, par) =>
          par.foreach(g)

        case `[]`(_, par) =>
          par.foreach(g)

        case `go.`(_, par) =>
          par.foreach(g)

        case `⟦⟧`(_, par, _, _) =>
          par.foreach(g)

        case _ =>

    def foreachʹ(g: PartialFunction[AST, Unit]): Unit =

      g.applyOrElse(ast, {

        case ∅() =>

        case ∥(_, components*) =>
          components.foreach(_.foreachʹ(g))

        case `.`(end, _*) =>
          end.foreachʹ(g)

        case !(_, _, _, par) =>
          par.foreachʹ(g)

        case `[]`(_, par) =>
          par.foreachʹ(g)

        case `go.`(_, par) =>
          par.foreachʹ(g)

        case `⟦⟧`(_, par, _, _) =>
          par.foreachʹ(g)

        case _ =>

      })

    def mapreduce[R](g: AST => R)(h: (R, R) => R): R =

      ast match

        case ∅() => g(ast)

        case ∥(_, components*) =>
          components.map(_.mapreduce(g)(h)).reduce(h)

        case it @ `.`(end, _*) =>
          h(g(it), end.mapreduce(g)(h))

        case it @ !(_, _, _, par) =>
          h(g(it), par.mapreduce(g)(h))

        case it @ `[]`(_, par) =>
          h(g(it), par.mapreduce(g)(h))

        case it @ `go.`(_, par) =>
          h(g(it), par.mapreduce(g)(h))

        case it @ `⟦⟧`(_, par, _, _) =>
          h(g(it), par.mapreduce(g)(h))

        case _ => g(ast)

    def mapreduceʹ[R](d: AST => R)(g: PartialFunction[AST, R])(h: (R, R) => R): R =

      ast match

        case ∅() => d(ast)

        case it @ ∥(_, components*) =>
          h(g.applyOrElse(it, d), components.map(_.mapreduceʹ(d)(g)(h)).reduce(h))

        case it @ `.`(end, _*) =>
          h(d(it), end.mapreduceʹ(d)(g)(h))

        case it @ !(_, _, _, par) =>
          h(d(it), par.mapreduceʹ(d)(g)(h))

        case it @ `[]`(_, par) =>
          h(d(it), par.mapreduceʹ(d)(g)(h))

        case it @ `go.`(_, par) =>
          h(d(it), par.mapreduceʹ(d)(g)(h))

        case it @ `⟦⟧`(_, par, _, _) =>
          h(d(it), par.mapreduceʹ(d)(g)(h))

        case _ => d(ast)

    def map(g: AST => AST): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ ∥(_, components*) =>
          it.cc(components = components.map(_.map(g)))

        case it @ `.`(end, _*) =>
          g(it.cc(end = end.map(g)))

        case it @ !(_, _, _, par) =>
          g(it.cc(par = par.map(g)))

        case it @ `[]`(_, par) =>
          g(it.cc(par = par.map(g)))

        case it @ `go.`(_, par) =>
          g(it.cc(par = par.map(g)))

        case it @ `⟦⟧`(_, par, _, _) =>
          g(it.cc(par = par.map(g)))

        case _ => g(ast)

    def mapʹ(g: AST => AST): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ ∥(_, components*) =>
          it.cc(components = components.map(_.mapʹ(g)))

        case it: `.` =>
          val itʹ @ `.`(end, _*) = g(it)
          itʹ.cc(end = end.mapʹ(g))

        case it: ! =>
          val itʹ @ !(_, _, _, par) = g(it)
          itʹ.cc(par = par.mapʹ(g))

        case it: `[]` =>
          val itʹ @ `[]`(_, par) = g(it)
          itʹ.cc(par = par.mapʹ(g))

        case it: `go.` =>
          val itʹ @ `go.`(_, par) = g(it)
          itʹ.cc(par = par.mapʹ(g))

        case it: `⟦⟧` =>
          val itʹ @ `⟦⟧`(_, par, _, _) = g(it)
          itʹ.cc(par = par.mapʹ(g))

        case _ => g(ast)

    def mapʹʹ(g: AST => (AST, Boolean)): T =

      inline given Conversion[AST, T] = _.asInstanceOf[T]

      ast match

        case ∅() => ast

        case it @ ∥(_, components*) =>
          it.cc(components = components.map(_.mapʹʹ(g)))

        case it: `.` =>
          g(it) match
            case (itʹ @ `.`(end, _*), false) =>
              itʹ.cc(end = end.mapʹʹ(g))
            case (itʹ, _) => itʹ

        case it: ! =>
          g(it) match
            case (itʹ @ !(_, _, _, par), false) =>
              itʹ.cc(par = par.mapʹʹ(g))
            case (itʹ, _) => itʹ

        case it: `[]` =>
          g(it) match
            case (itʹ @ `[]`(_, par), false) =>
              itʹ.cc(par = par.mapʹʹ(g))
            case (itʹ, _) => itʹ

        case it: `go.` =>
          g(it) match
            case (itʹ @ `[]`(_, par), false) =>
              itʹ.cc(par = par.mapʹʹ(g))
            case (itʹ, _) => itʹ

        case it: `⟦⟧` =>
          g(it) match
            case (itʹ @ `⟦⟧`(_, par, _, _), false) =>
              itʹ.cc(par = par.mapʹʹ(g))
            case (itʹ, _) => itʹ

        case _ => g(ast)._1
