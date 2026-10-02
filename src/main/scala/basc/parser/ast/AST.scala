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

import io.circe.{ Encoder, Json }
import io.circe.syntax.*

import BioAmbients.{ Free, Sum }
import Encoding.Definition
import Pre.*
import AST.*


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

  given Encoder[AST] = _ match

    case it @ ∅() =>
      Json.obj { "+" ->
        Json
          .obj(
            "scaling" -> Json.fromInt(-1),
            "operands" -> Json.arr()
          )
          .deepMerge(it.asInstanceOf[Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case it @ +(scaling, choices*) =>
      Json.obj { "+" ->
        Json
          .obj(
            "scaling" -> Json.fromInt(scaling),
            "operands" -> Json.fromValues(choices.map((_: AST).asJson))
          )
          .deepMerge(it.asInstanceOf[Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case it @ ∥(scaling, components*) =>
      Json.obj { "∥" ->
        Json
          .obj(
            "scaling" -> Json.fromInt(scaling),
            "operands" -> Json.fromValues(components.map((_: AST).asJson))
          )
          .deepMerge(it.asInstanceOf[Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case it @ `.`(end, prefixes*) =>
      Json.obj { "." ->
        Json
          .obj(
            "leaf" -> (end: AST).asJson,
            "prefixes" -> Json.fromValues(prefixes.map(_.asJson))
          )
          .deepMerge(it.asInstanceOf[Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case it @ ?:(((lhs, rhs), mismatch), t, f) =>
      Json.obj { "?:" ->
        Json
          .obj(
            "lhs" -> lhs.toJson,
            "rhs" -> rhs.toJson,
            "mismatch" -> Json.fromBoolean(mismatch),
            "t" -> (t: AST).asJson,
            "f" -> f.map((_: AST).asJson).getOrElse(Json.Null)
          )
          .deepMerge(it.asInstanceOf[Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case it @ !(parallelism, pace: Option[(Long, String)], Some(guard), sum) if parallelism < -1 =>
      Json.obj { "¡" ->
        Json
          .obj(
            "parallelism" -> Json.fromLong(-(parallelism%Int.MaxValue)),
            "pace" -> pace.map { (amount, unit) =>
              Json.obj("amount" -> Json.fromLong(amount)
                      ,"unit" -> Json.fromString(unit))
            }.getOrElse(Json.Null),
            "guard" -> (guard: Pre).asJson,
            "operand" -> (sum: AST).asJson
          )
          .deepMerge(it.asInstanceOf[Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }
    case it @ !(parallelism, pace: Option[(Long, String)], Some(guard), sum) =>
      Json.obj { "!" ->
        Json
          .obj(
            "parallelism" -> Json.fromLong(parallelism),
            "pace" -> pace.map { (amount, unit) =>
              Json.obj("amount" -> Json.fromLong(amount)
                      ,"unit" -> Json.fromString(unit))
            }.getOrElse(Json.Null),
            "guard" -> (guard: Pre).asJson,
            "operand" -> (sum: AST).asJson
          )
          .deepMerge(it.asInstanceOf[Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case it @ `[]`(label, sum) =>
      Json.obj { "[]" ->
        Json
          .obj(
            "label" -> Json.fromStringOrNull(label),
            "operand" -> (sum: AST).asJson
          )
          .deepMerge(it.asInstanceOf[Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case `⟦⟧`(_, sum, _, _) => (sum: AST).asJson

    case it @ `(*)`(identifier, params*) =>
      Json.obj { "(*)" ->
        Json
          .obj(
            "identifier" -> Json.fromString(identifier),
            "parameters" -> Json.fromValues(params.map(_.toJson))
          )
          .deepMerge(it.asInstanceOf[Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case _ => Json.Null

  given `_+_`: {} with
    extension (self: +)
      def cc(scaling: Int = self.scaling,
             choices: Seq[∥] = self.choices): + =
        import Cast.given
        `+`(scaling, choices*)(using self.lc).setPos(self.pos).free = self.free

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
      case sum: + => sum.isVoid
      case _ => false

  extension (sum: +)
    def isVoid: Boolean = sum match
      case +(_) => true
      case _ => sum.choices.forall(_.components.forall { case `.`(sum: +) => sum.isVoid case _ => false })
