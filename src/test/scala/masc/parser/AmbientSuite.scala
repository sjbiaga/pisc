/*
 * Copyright (c) 2023-2024 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

import scala.io.Source

import munit.FunSuite

import Ambient._
import Calculus._
import Encoding._
import AmbientSuite._

class AmbientSuite extends FunSuite:

  test("agent-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #0") {
      Ambient {
        source("""
               ⟦⟧ =
               P(u) = (x). (x). ( (v). ⟦ z. ⟧ )
               """)
      }
    }

  }

  test("encoding-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #0 in the right hand side of encoding 1") {
      Ambient {
        source("""
               ⟦⟧ =
               ⟦1 t"X" 1⟧ = ⟦ z. ⟧
               """)
      }
    }

  }

  test("encoding-uniqueness-hardcoded-binding") {

    interceptMessage[UniquenessBindingParsingException]("A binding name (x) does not correspond to a unique hardcoded binding occurrence, but is duplicated at nesting level #0 in the right hand side of encoding 1") {
      Ambient {
        source("""
               ⟦1 t"λ $x . ${$M}" 1⟧{u} = (x). (x). M{u}
               """)
      }
    }

  }

  test("encoding-uniqueness-encoded-binding") {

    interceptMessage[UniquenessBindingParsingException]("A binding name (z) does not correspond to a unique encoded binding occurrence, but is duplicated at nesting level #1 in the right hand side of encoding 3") {
      Ambient {
        source("""
               ⟦ 'x ⟧{u} = x[ <u> ]
               ⟦2 t"λ $x,$y . ${$M}" 2⟧{u} = u[ (x). (y). M{y} ]
               ⟦3 t"λλ $z" 3⟧ = ⟦2 λ z,z . ⟦ z ⟧ 2⟧
               """)
      }
    }

  }

  test("encoding-non-parameter-hardcoded-binding") {

    interceptMessage[NonParameterBindingParsingException]("A binding name (u) in a hardcoded binding occurrence does not correspond to a parameter at nesting level #0 in the right hand side of encoding 1") {
      Ambient {
        source("""
               ⟦1 t"λ $x . ${$M}" 1⟧{u} = (u). (x). M{x}
               """)
      }
    }

  }

  test("encoding-non-parameter-encoded-binding") {

    interceptMessage[NonParameterBindingParsingException]("A binding name (u) in an encoded binding occurrence does not correspond to a parameter at nesting level #1 in the right hand side of encoding 3") {
      Ambient {
        source("""
               ⟦ 'x ⟧{u} = x[ <u> ]
               ⟦2 t"λ $x,$y . ${$M}" 2⟧{u} = u[ (x). (y). M{y} ]
               ⟦3 t"λλ $z" 3⟧{u} = ⟦2 λ z,u . ⟦ z ⟧ 2⟧
               """)
      }
    }

  }


object AmbientSuite:

  def source(src: String) = Source.fromString(src)
