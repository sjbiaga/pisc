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

import Ambient.Free


enum Cap extends Positional with Free:

  case Λ(name: String)
        (using override val lc: Int => (Int, Int))

  case ε()
        (using override val lc: Int => (Int, Int))

  case ζ(op: Cap.Op, amb: String)
        (using override val lc: Int => (Int, Int))

  override def toString: String = this match
    case Λ(name) => name
    case ζ(op, amb) => s"$op $amb"
    case _ => "ε"


object Cap:

  enum Op { case in, out, open }

  given `_Λ_`: {} with
    extension (self: Λ)
      def cc(name: String = self.name): Λ =
        import Cast.given
        Λ(name)(using self.lc).setPos(self.pos).free = self.free

  given `_ζ_`: {} with
    extension (self: ζ)
      def cc(op: Op = self.op,
             amb: String = self.amb): ζ =
        import Cast.given
        ζ(op, amb)(using self.lc).setPos(self.pos).free = self.free
