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

import io.circe.{ Encoder, Json }
import io.circe.syntax.*

import Expression.Code
import BioAmbients.{ `$`, Cap, Act, Bound, Free }


enum Pre extends Positional with Bound with Free:

  case ν(names: String*) // forcibly
        (using override val lc: Int => (Int, Int))

  case τ(override val rate: Option[Any],
         code: Option[Code])(id: => String)
        (using override val lc: Int => (Int, Int))
      extends Pre with Act(() => id)

  case π(dir: `$`,
         channel: λ,
         name: λ,
         polarity: Option[String],
         override val rate: Option[Any],
         code: Option[Code])(id: => String)
        (using override val lc: Int => (Int, Int))
      extends Pre with Act(() => id)

  case ζ(cap: Cap,
         name: String,
         polarity: Boolean,
         override val rate: Option[Any],
         code: Option[Code])(id: => String)
        (using override val lc: Int => (Int, Int))
      extends Pre with Act(() => id)

  override def toString: String = this match
    case ν(names*) => names.mkString("ν(", ", ", ")")
    case π(dir, channel, name, polarity, _, _) =>
      if polarity.isDefined
      then
        polarity.get match
          case "ν"  =>
            (if dir == `$`.local then "" else "" + dir + " ") + channel + " ! {ν" + name + "}."
          case ""   =>
            (if dir == `$`.local then "" else "" + dir + " ") + channel + " ? {" + name + "}."
          case cons =>
            channel.asSymbol.name + s"$cons ? {" + name + "}."
      else (if dir == `$`.local then "" else "" + dir + " ") + channel + " ! {" + name + "}."
    case ζ(cap, name, _, _, _) =>
      "" + cap + " " + name + "."
    case _ => "τ."


object Pre:

  given Encoder[Bound & Free] = { it =>
    import Bound.given, Free.given
    val bound = it.asInstanceOf[Bound].asJson
    val free = it.asInstanceOf[Free].asJson
    free.deepMerge(bound)
  }

  given Encoder[Pre] = _ match

    case it @ ν(names*) =>
      Json.obj { "ν" ->
        Json
          .obj("names" -> Json.fromValues(names.map(Json.fromString)))
          .deepMerge(it.asInstanceOf[Bound & Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case it @ τ(Some(rate), _) =>
      Json.obj { "τ" ->
        it.asInstanceOf[Act].asJson
          .deepMerge(it.asInstanceOf[Bound & Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case it @ π(dir, ch, arg, Some("ν"), Some(rate), _) =>
      Json.obj { "πν" ->
        Json
          .obj(
            "direction" -> dir.asJson,
            "channel" -> ch.toJson,
            "argument" -> arg.toJson,
            "polarity" -> Json.False
          )
          .deepMerge(it.asInstanceOf[Act].asJson)
          .deepMerge(it.asInstanceOf[Bound & Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }
    case it @ π(dir, ch, par, Some(""), Some(rate), _) =>
      Json.obj { "π" ->
        Json
          .obj(
            "direction" -> dir.asJson,
            "channel" -> ch.toJson,
            "parameter" -> par.toJson,
            "polarity" -> Json.True
          )
          .deepMerge(it.asInstanceOf[Act].asJson)
          .deepMerge(it.asInstanceOf[Bound & Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }
    case it @ π(_, ch, ops, Some(cons), _, _) =>
      Json.obj { cons ->
        Json
          .obj(
            "channel" -> ch.toJson,
            "operator" -> Json.fromString(cons),
            "operands" -> ops.toJson
          )
          .deepMerge(it.asInstanceOf[Bound & Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }
    case it @ π(dir, ch, λ(_: Term), None, Some(rate), _) =>
      Json.obj { "π" ->
        Json
          .obj(
            "direction" -> dir.asJson,
            "channel" -> ch.toJson,
            "polarity" -> Json.False
          )
          .deepMerge(it.asInstanceOf[Act].asJson)
          .deepMerge(it.asInstanceOf[Bound & Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }
    case it @ π(dir, ch, arg, None, Some(rate), _) =>
      Json.obj { "π" ->
        Json
          .obj(
            "direction" -> dir.asJson,
            "channel" -> ch.toJson,
            "argument" -> arg.toJson,
            "polarity" -> Json.False
          )
          .deepMerge(it.asInstanceOf[Act].asJson)
          .deepMerge(it.asInstanceOf[Bound & Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case it @ ζ(cap, name: String, polarity: Boolean, Some(rate), _) =>
      Json.obj { "ζ" ->
        Json
          .obj(
            "capability" -> cap.asJson,
            "channel" -> Json.fromString(name),
            "polarity" -> Json.fromBoolean(polarity)
          )
          .deepMerge(it.asInstanceOf[Act].asJson)
          .deepMerge(it.asInstanceOf[Bound & Free].asJson)
          .deepMerge(it.asInstanceOf[Positional].asJson)
      }

    case _ => Json.Null

  given `_ν_`: {} with
    extension (self: ν)
      def cc(names: Seq[String] = self.names): ν =
        import Cast.given
        (ν(names*)(using self.lc).setPos(self.pos).bound = self.bound).free = self.free

  given `_τ_`: {} with
    extension (self: τ)
      def cc(rate: Option[Any] = self.rate,
             code: Option[Code] = self.code)(id: => String = self.id): τ =
        import Cast.given
        (τ(rate, code)(id)(using self.lc).setPos(self.pos).bound = self.bound).free = self.free

  given `_π_`: {} with
    extension (self: π)
      def cc(dir: `$` = self.dir,
             channel: λ = self.channel,
             name: λ = self.name,
             polarity: Option[String] = self.polarity,
             rate: Option[Any] = self.rate,
             code: Option[Code] = self.code)(id: => String = self.id): π =
        import Cast.given
        (π(dir, channel, name, polarity, rate, code)(id)(using self.lc).setPos(self.pos).bound = self.bound).free = self.free

  given `_ζ_`: {} with
    extension (self: ζ)
      def cc(cap: Cap = self.cap,
             name: String = self.name,
             polarity: Boolean = self.polarity,
             rate: Option[Any] = self.rate,
             code: Option[Code] = self.code)(id: => String = self.id): ζ =
        import Cast.given
        (ζ(cap, name, polarity, rate, code)(id)(using self.lc).setPos(self.pos).bound = self.bound).free = self.free
