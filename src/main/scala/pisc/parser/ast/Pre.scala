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
package parser
package ast

import Expression.Code
import StochasticPi.{ Act, Bound, Free }


enum Pre extends Positional with Bound with Free:

  case ν(names: String*) // forcibly
        (using override val lc: Int => (Int, Int))

  case τ(override val rate: Option[Any],
         code: Option[Code])(id: => String)
        (using override val lc: Int => (Int, Int))
      extends Pre with Act(() => id)

  case π(channel: λ,
         name: λ,
         polarity: Option[String],
         override val rate: Option[Any],
         code: Option[Code])(id: => String)
        (using override val lc: Int => (Int, Int))
      extends Pre with Act(() => id)

  override def toString: String = this match
    case ν(names*) => names.mkString("ν(", ", ", ")")
    case π(channel, name, polarity, _, _) => ""
      if polarity.isDefined
      then
        polarity.get match
          case "ν"  =>
            "" + channel + "<ν" + name + ">."
          case ""   =>
            "" + channel + "(" + name + ")."
          case cons =>
            "" + channel + s"${polarity.get}(" + name + ")."
      else "" + channel + "<" + name + ">."
    case _ => "τ."


object Pre:

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
      def cc(channel: λ = self.channel,
             name: λ = self.name,
             polarity: Option[String] = self.polarity,
             rate: Option[Any] = self.rate,
             code: Option[Code] = self.code)(id: => String = self.id): π =
        import Cast.given
        (π(channel, name, polarity, rate, code)(id)(using self.lc).setPos(self.pos).bound = self.bound).free = self.free
