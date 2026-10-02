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

import scala.meta.{ Pat, Term, Type }

import scala.util.parsing.input.{ NoPosition, Position }

import io.circe.{ Encoder, Json }
import io.circe.syntax.*


package object ast:

  import AST.{ `(*)`, + }

  type Bind = (`(*)`, +)

  object Bind:
    given Encoder[Bind] = _ match
      case (bind @ `(*)`(identifier, params*), sum) =>
        Json.obj {
          s"${identifier}_${params.size}" ->
          Json.obj("agent" -> (bind: AST).asJson
                  ,"body" -> (sum: AST).asJson)
        }


  trait Positional extends scala.util.parsing.input.Positional:
    val lc: Int => (Int, Int)

  object Positional:
    given Encoder[Positional] = { it =>
      val (line, col) = it.lc(it.pos.column-1)
      Json.obj("line"   -> Json.fromInt(line)
              ,"column" -> Json.fromInt(col))
    }


  object Cast:

    given [T <: Pre]: Conversion[Pre, T] = _.asInstanceOf[T]

    given [T <: AST]: Conversion[AST, T] = _.asInstanceOf[T]


  case class λ(`val`: Any)(using val `type`: Option[(Type, Option[Type])] = None):
    val isSymbol: Boolean = `val`.isInstanceOf[Symbol]
    def asSymbol: Symbol = `val`.asInstanceOf[Symbol]

    type Kind = `val`.type

    val kind: String = `val` match
      case _: Symbol => "channel name"
      case _: BigDecimal => "decimal number"
      case _: Boolean => "True False"
      case _: String => "string literal"
      case _: Term => "Scalameta Term"
      case _ => "polyadic names"

    def toJson: Json =
      `val` match
        case it: Symbol => Json.fromString(it.name)
        case it: BigDecimal => Json.fromBigDecimal(it)
        case it: Boolean => Json.fromBoolean(it)
        case it: String => Json.fromString("\"" + it + "\"")
        case it: List[`λ`] => Json.fromValues(it.map(_.toJson))
        case it: Term => Json.Null

    def toPat: Pat =
      import scala.meta._
      import dialects.Scala3
      `val` match
        case it: Symbol => Pat.Macro(Term.QuotedMacroExpr(Term.Name(it.name)))
        case it: BigDecimal => Lit.Double(it.toDouble)
        case it: Boolean => Lit.Boolean(it)
        case it: String => Lit.String(it)
        case it: Term => it.asInstanceOf[Pat]

    def toTerm: Term =
      import scala.meta._
      import dialects.Scala3
      `val` match
        case it: Symbol => Term.Name(it.name)
        case it: BigDecimal => Term.Apply(Term.Name("BigDecimal"), Term.ArgClause(Lit.String(it.toString)::Nil))
        case it: Boolean => Lit.Boolean(it)
        case it: String => Lit.String(it)
        case it: Term => Expression(it)._1

    override def toString: String = `val` match
      case it: Symbol => it.name
      case it: BigDecimal => "" + it
      case it: Boolean => it.toString.capitalize
      case it: String => "\"" + it + "\""
      case it: Term => "/*" + it + "*/"
      case it: List[`λ`] => it.mkString(", ")
