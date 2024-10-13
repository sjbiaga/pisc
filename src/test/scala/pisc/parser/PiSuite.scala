package pisc
package parser

import scala.io.Source

import munit.FunSuite

import Pi._
import Calculus._
import PiSuite._

class PiSuite extends FunSuite:

  test("agent-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #0") {
      Pi(source("""
                ⟦⟧ =
                P(u) = u(x). u(x). ( u(v). ⟦ z(x). ⟧ )
                """))
    }

  }

  test("encoding-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #0 in the right hand side of encoding 1") {
      Pi(source("""
                ⟦⟧ =
                ⟦1 t"X" 1⟧ = ⟦ z(x). ⟧
                """))
    }

  }

  test("encoding-uniqueness-hardcoded-binding") {

    interceptMessage[UniquenessBindingParsingException]("A binding name (x) does not correspond to a unique hardcoded binding occurrence, but is duplicated at nesting level #0 in the right hand side of encoding 1") {
      Pi(source("""
                ⟦ 'x ⟧{u} = x<u>.
                ⟦1 t"λ $x . ${$M}" 1⟧{u} = u(x). u(x). M{v}
                """))
    }

  }

  test("encoding-uniqueness-encoded-binding") {

    interceptMessage[UniquenessBindingParsingException]("A binding name (z) does not correspond to a unique encoded binding occurrence, but is duplicated at nesting level #1 in the right hand side of encoding 3") {
      Pi(source("""
                ⟦ 'x ⟧{u} = x<u>.
                ⟦2 t"λ $x,$y . ${$M}" 2⟧{u} = u(x). u(y). M{y}
                ⟦3 t"λλ $z" 3⟧ = ⟦2 λ z,z . ⟦ z ⟧ 2⟧
                """))
    }

  }


object PiSuite:

  def source(src: String) = Source.fromString(src)
