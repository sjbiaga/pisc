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
     Main(Emitter.test, getClass.getSimpleName) {
        source("""
               ⟦⟧ =
               P(u) = u(x). u(x). ( u(v). ⟦ z(x). ⟧ )
               """)
      }
    }

  }

  test("encoding-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #1 in the right hand side of definition 1") {
      Main(Emitter.test, getClass.getSimpleName) {
        source("""
               ⟦⟧ =
               ⟦1 t"X" 1⟧ = ⟦ z(x). ⟧
               """)
      }
    }

  }

  test("encoding-uniqueness-hardcoded-binding") {

    interceptMessage[UniquenessBindingParsingException]("A binding name (x) does not correspond to a unique hardcoded binding occurrence, being duplicated at nesting level #0 in the right hand side of definition 1") {
      Main(Emitter.test, getClass.getSimpleName) {
        source("""
               ⟦1 t"λ $x . ${$M}" 1⟧{u} = u(x). u(x). M{v}
               """)
      }
    }

  }

  test("encoding-uniqueness-encoded-binding") {

    interceptMessage[RuntimeException]("A binding name (z) does not correspond to a unique encoded binding occurrence, being duplicated at nesting level #1 in the right hand side of definition 3") {
      Main(Emitter.test, getClass.getSimpleName) {
        source("""
               ⟦ 'x ⟧{u} = x<u>.
               ⟦2 t"λ $x,$y . ${$M}" 2⟧{u} = u(x). u(y). M{y}
               ⟦3 t"λλ $z" 3⟧ = ⟦2 λ z,z . ⟦ z ⟧ 2⟧
               """)
      }
    }

  }

  test("encoding-non-parameter-hardcoded-binding") {

    Main(Emitter.test, getClass.getSimpleName) {
      source("""
             ⟦1 t"λ $x . ${$M}" 1⟧{u} = x(u).
             Main = ν(x) ⟦ λ x . () ⟧
             """)
    } match
      case _ :: Right((`(*)`("Main", _), +(_, ∥(_, `.`(exp, ν("x")))))) :: Nil =>
        assertMatches(exp) {
          case `⟦⟧`(_, +(_, ∥(_, `.`(∅(), π(λ(Symbol("x")), λ(Symbol(s"u_$_")), Some(""), None)))), _, _) => true
        }

  }

  test("encoding-non-parameter-encoded-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #2 in the right hand side of definition 3") {
      Main(Emitter.test, getClass.getSimpleName) {
        source("""
               ⟦ 'x ⟧{u} = x<u>.
               ⟦1 t"λ $x . ${$M}" 1⟧{u} = u(x). u(v). M{v}
               ⟦3 t"Id" 3⟧{u} = ⟦1 λ u . ⟦ z ⟧ 1⟧
               """)
      }
    }

  }

  test("encoding - with invocation - parameters and pointers mixed or not") {

    Main(Emitter.test, getClass.getSimpleName) {
      source("""
                ⟦ 'P ^ 'Q ⟧{x,y} = P{x} | Q{y}
                Agent0 = ()
                Agent2(a, b) = b<a>.()
                Process1 = ν(x, y) ⟦ ν(x) Agent2(){x} ^ Agent0 ⟧{x, y}
                Process2 = ν(x, y) ⟦ ν(x) Agent2(x, x) ^ Agent0 ⟧{x, y}
             """)
    } match
      case _ :: _ :: _ :: Right((_, +(_, ∥(_, `.`(exp1, ν("x", "y")))))) :: Right((_, +(_, ∥(_, `.`(exp2, ν("x", "y")))))) :: Nil =>
        exp1 match
          case `⟦⟧`(Definition(_, _, _, variables1, _),
                    +(_, ∥(_, `.`(`{}`("Agent2", List(Symbol("x_υ6υ"), Symbol("x_υ4υ")), true), ν("x_υ6υ")),
                              `.`(`(*)`("Agent0", Nil)))), _, pointers1) =>
            val assignment1 = variables1 zip pointers1
            assertEquals(assignment1, Set(Symbol("x_υ4υ") -> Symbol("x"), Symbol("y_υ5υ") -> Symbol("y")))
          case _ =>
            assert(false)
        exp2 match
          case `⟦⟧`(Definition(_, _, _, variables2, _),
                    +(_, ∥(_, `.`(`(*)`("Agent2", Nil, λ(Symbol("x_υcυ")), λ(Symbol("x_υcυ"))), ν("x_υcυ")),
                              `.`(`(*)`("Agent0", Nil)))), _, pointers2) =>
            val assignment2 = variables2 zip pointers2
            assertEquals(assignment2, Set(Symbol("x_υaυ") -> Symbol("x"), Symbol("y_υbυ") -> Symbol("y")))
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }

  test("encoding - nested") {

    Main(Emitter.test, getClass.getSimpleName) {
      source("""
                ⟦ 'P ^ 'Q ⟧{x,y} = P{x} | Q{y}
                ⟦1 t"Out" 1⟧{z} = z<z>.()
                ⟦2 t"In" 2⟧{w} = w(z).τ/*println('z)*/.()
                ⟦3 t"Nest" 3⟧ = ν(ch) ⟦ ⟦1 Out 1⟧ ^ ⟦2 In 2⟧ ⟧{ch, ch}
                Main = ⟦3 Nest 3⟧
             """)
    } match
      case _ :: Right((_, +(_, ∥(_, `.`(exp))))) :: Nil =>
        exp match
          case `⟦⟧`(_, +(_, ∥(_, `.`(expʹ, ν("ch_υnυ")))), _,  _) =>
            expʹ match
              case `⟦⟧`(Definition(_, _, _, variables, _), +(_, ∥(_, `.`(exp1), `.`(exp2))), _, pointers) =>
                val assignment = variables zip pointers
                assertEquals(assignment, Set(Symbol("x_υoυ") -> Symbol("ch_υnυ"), Symbol("y_υpυ") -> Symbol("ch_υnυ")))
                exp1 match
                  case `⟦⟧`(Definition(_, _, _, variables1, _), +(_, ∥(_, `.`(∅(), π(λ(Symbol("z_υqυ")), λ(Symbol("z_υqυ")), None, None)))), _, pointers1) =>
                    val assignment1 = variables1 zip pointers1
                    assertEquals(assignment1, Set(Symbol("z_υqυ") -> Symbol("x_υoυ")))
                  case _ =>
                    assert(false)
                exp2 match
                  case `⟦⟧`(Definition(_, _, _, variables2, _), +(_, ∥(_, `.`(∅(), π(λ(Symbol("w_υrυ")), λ(Symbol("z_υsυ")), Some(_), None), τ(Some(_))))), _, pointers2) =>
                    val assignment2 = variables2 zip pointers2
                    assertEquals(assignment2, Set(Symbol("w_υrυ") -> Symbol("y_υpυ")))
                  case _ =>
                    assert(false)
              case _ =>
                assert(false)
          case _ =>
            assert(false)
      case _ =>
        assert(false)

  }

  test("encoding-pending-once") {

    interceptMessage[RuntimeException]("A binding name (x) does not correspond to a unique encoded binding occurrence, being clobbered at nesting level #1 in the right hand side of definition 0") {
      Main(Emitter.test, getClass.getSimpleName) {
        source("""
               ⟦ 'x ^ 'P ⟧ = ν(x) P{}
               ⟦ 'x & 'P ⟧ = ⟦ y ^ y<x>. ⟧ | ⟦ x ^ P{}⟧
               """)
      }
    }

  }

  test("encoding-pending-twice") {

    interceptMessage[RuntimeException]("A binding name (x) does not correspond to a unique encoded binding occurrence, being clobbered at nesting level #1 in the right hand side of definition 0") {
      Main(Emitter.test, getClass.getSimpleName) {
        source("""
               ⟦ 'x ^ 'P ^ 'Q ⟧ = ν(x) ( P{} | Q{} )
               ⟦ 'x & 'P ⟧(k) = ⟦ y ^ y<x>. ^ k<x>. ⟧ | ⟦ x ^ P{} ^ P{} ⟧
               """)
      }
    }

  }

  test("encoding-pending-expanded-out-of-scope") {

    interceptMessage[RuntimeException]("An occurrence of a definition parameter (y) is not in the scope of its binding occurrence at nesting level #1 in the right hand side of definition 0") {
      Main(Emitter.test, getClass.getSimpleName) {
        source("""
               ⟦ 'x ^ 'y ^ 'P ⟧ = x(y). P{}
               ⟦ 'x & 'y ⟧ = ⟦ x ^ y ^ x<y>. ⟧ | ⟦ y ^ z ^ ⟧
               """)
      }
    }

  }

  test("encoding-pending-pointers-out-of-scope") {

    interceptMessage[RuntimeException]("An occurrence of a definition parameter (y) is not in the scope of its binding occurrence at nesting level #0 in the right hand side of definition 0") {
      Main(Emitter.test, getClass.getSimpleName) {
        source("""
               ⟦ 'x ^ 'y ^ 'P ⟧ = x(y). P{}
               ⟦ 'x & 'y ⟧ = ⟦ x ^ y ^ x<y>. ⟧{y}
               """)
      }
    }

  }

  test("encoding-pending-hardcoded-out-of-scope") {

    interceptMessage[RuntimeException]("An occurrence of a definition parameter (y) is not in the scope of its binding occurrence at nesting level #0 in the right hand side of definition 0") {
      Main(Emitter.test, getClass.getSimpleName) {
        source("""
               ⟦ 'x ^ 'y ^ 'P ⟧ = x(y). P{}
               ⟦ 'x & 'y ⟧ = ⟦ x ^ y ^ x<y>. ⟧ | x<y>.
               """)
      }
    }

  }


object PiSuite:

  def source(src: String) = Source.fromString(src)
