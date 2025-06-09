/*
 * Copyright (c) 2023-2025 Sebastian I. Gliţa-Catina <gseba@users.sourceforge.net>
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

import scala.collection.mutable.{ LinkedHashSet => Set }

import scala.io.Source

import munit.FunSuite

import Pi.*
import Calculus.*
import Encoding.*
import PiSuite.*


class PiSuite extends FunSuite:

  test("agent-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #1") {
     Main(getClass.getSimpleName) {
        source("""
               ⟦⟧ =
               P(u) = u(x). u(x). ( u(v). ⟦ z(x). ⟧ )
               """)
      }
    }

  }

  test("encoding-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #1 in the right hand side of encoding 1") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦⟧ =
               ⟦1 t"X" 1⟧ = ⟦ z(x). ⟧
               """)
      }
    }

  }

  test("encoding-uniqueness-hardcoded-binding") {

    interceptMessage[UniquenessBindingParsingException]("A binding name (x) does not correspond to a unique hardcoded binding occurrence, but is duplicated at nesting level #0 in the right hand side of encoding 1") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦1 t"λ $x . ${$M}" 1⟧{u} = u(x). u(x). M{v}
               """)
      }
    }

  }

  test("encoding-uniqueness-encoded-binding") {

    interceptMessage[UniquenessBindingParsingException]("A binding name (z) does not correspond to a unique encoded binding occurrence, but is duplicated at nesting level #1 in the right hand side of encoding 3") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦ 'x ⟧{u} = x<u>.
               ⟦2 t"λ $x,$y . ${$M}" 2⟧{u} = u(x). u(y). M{y}
               ⟦3 t"λλ $z" 3⟧ = ⟦2 λ z,z . ⟦ z ⟧ 2⟧
               """)
      }
    }

  }

  test("encoding-non-parameter-hardcoded-binding") {

    interceptMessage[NonParameterBindingParsingException]("A binding name (u) in a hardcoded binding occurrence does not correspond to a parameter at nesting level #0 in the right hand side of encoding 1") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦1 t"λ $x . ${$M}" 1⟧{u} = x(u).
               """)
      }
    }

  }

  test("encoding-non-parameter-encoded-binding") {

    interceptMessage[NonParameterBindingParsingException]("A binding name (u) in an encoded binding occurrence does not correspond to a parameter at nesting level #1 in the right hand side of encoding 3") {
      Main(getClass.getSimpleName) {
        source("""
               ⟦ 'x ⟧{u} = x<u>.
               ⟦1 t"λ $x . ${$M}" 1⟧{u} = u(x). u(v). M{v}
               ⟦3 t"Id" 3⟧{u} = ⟦1 λ u . ⟦ z ⟧ 1⟧
               """)
      }
    }

  }

  test("encoding - with invocation - parameters and pointers mixed or not") {

    Main(getClass.getSimpleName) {
      source("""
                ⟦ 'P ^ 'Q ⟧{x,y} = P{x} | Q{y}
                Agent0 = ()
                Agent2(a, b) = b<a>.()
                Process1 = ν(x, y) ⟦ ν(x) Agent2(){x} ^ Agent0 ⟧{x, y}
                Process2 = ν(x, y) ⟦ ν(x) Agent2(x, x) ^ Agent0 ⟧{x, y}
             """)
    } match
      case _ :: _ :: Right((_, +(∥(`.`(exp1, ν("x", "y")))))) :: Right((_, +(∥(`.`(exp2, ν("x", "y")))))) :: Nil =>
        exp1 match
          case `⟦⟧`(_, _, +(∥(`.`(`{}`("Agent2", List(Symbol("x_υ6υ"), Symbol("x_υ4υ")), true), ν("x_υ6υ")),
                              `.`(`(*)`("Agent0", Nil)))), assignment1) =>
            assertEquals(assignment1, Set(Symbol("x_υ4υ") -> Symbol("x"), Symbol("y_υ5υ") -> Symbol("y")))
          case _ =>
            assert(false)
        exp2 match
          case `⟦⟧`(_, _, +(∥(`.`(`(*)`("Agent2", Nil, λ(Symbol("x_υcυ")), λ(Symbol("x_υcυ"))), ν("x_υcυ")),
                              `.`(`(*)`("Agent0", Nil)))), assignmentʹ) =>
            assertEquals(assignmentʹ, Set(Symbol("x_υaυ") -> Symbol("x"), Symbol("y_υbυ") -> Symbol("y")))
          case _ =>
            assert(false, exp2)
      case _ =>
        assert(false)

  }

  test("encoding - nested") {

    Main(getClass.getSimpleName) {
      source("""
                ⟦ 'P ^ 'Q ⟧{x,y} = P{x} | Q{y}
                ⟦1 t"Out" 1⟧{z} = z<z>.()
                ⟦2 t"In" 2⟧{w} = w(z).τ/*println('z)*/.()
                ⟦3 t"Nest" 3⟧ = ν(ch) ⟦ ⟦1 Out 1⟧ ^ ⟦2 In 2⟧ ⟧{ch, ch}
                Main = ⟦3 Nest 3⟧
             """)
    } match
      case Right((_, +(∥(`.`(exp))))) :: Nil =>
        exp match
          case `⟦⟧`(_, _, +(∥(`.`(expʹ, ν("ch_υnυ")))), _) =>
            expʹ match
              case `⟦⟧`(_, _, +(∥(`.`(exp1), `.`(exp2))), assignment) =>
                assertEquals(assignment, Set(Symbol("x_υoυ") -> Symbol("ch_υnυ"), Symbol("y_υpυ") -> Symbol("ch_υnυ")))
                exp1 match
                  case `⟦⟧`(_, _, +(∥(`.`(∅(_), π(λ(Symbol("z_υqυ")), λ(Symbol("z_υqυ")), false, None)))), assignment1) =>
                    assertEquals(assignment1, Set(Symbol("z_υqυ") -> Symbol("x_υoυ")))
                  case _ =>
                    assert(false)
                exp2 match
                  case `⟦⟧`(_, _, +(∥(`.`(∅(_), π(λ(Symbol("w_υrυ")), λ(Symbol("z_υsυ")), true, None), τ(Some(_))))), assignmentʹ) =>
                    assertEquals(assignmentʹ, Set(Symbol("w_υrυ") -> Symbol("y_υpυ")))
                  case _ =>
                    assert(false)
              case _ =>
                assert(false)
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }


object PiSuite:

  def source(src: String) = Source.fromString(src)
