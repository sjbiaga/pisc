package masc
package parser

import scala.io.Source

import munit.FunSuite

import Ambient._
import Calculus._
import AmbientSuite._

class AmbientSuite extends FunSuite:

  test("agent-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #0") {
      Ambient(source("""
                     ⟦⟧ =
                     P(u) = (x). (x). ( (v). ⟦ z. ⟧ )
                     """))
    }

  }

  test("encoding-no-binding") {

    interceptMessage[NoBindingParsingException]("No binding for z at nesting level #0 in the right hand side of encoding 1") {
      Ambient(source("""
                     ⟦⟧ =
                     ⟦1 t"X" 1⟧ = ⟦ z. ⟧
                     """))
    }

  }

  test("encoding-uniqueness-hardcoded-binding") {

    interceptMessage[UniquenessBindingParsingException]("A binding name (x) does not correspond to a unique hardcoded binding occurrence, but is duplicated at nesting level #0 in the right hand side of encoding 1") {
      Ambient(source("""
                     ⟦1 t"λ $x . ${$M}" 1⟧{u} = (x). (x). M{u}
                     """))
    }

  }

  test("encoding-uniqueness-encoded-binding") {

    interceptMessage[UniquenessBindingParsingException]("A binding name (z) does not correspond to a unique encoded binding occurrence, but is duplicated at nesting level #1 in the right hand side of encoding 3") {
      Ambient(source("""
                     ⟦ 'x ⟧{u} = x[ <u> ]
                     ⟦2 t"λ $x,$y . ${$M}" 2⟧{u} = u[ (x). (y). M{y} ]
                     ⟦3 t"λλ $z" 3⟧ = ⟦2 λ z,z . ⟦ z ⟧ 2⟧
                     """))
    }

  }


object AmbientSuite:

  def source(src: String) = Source.fromString(src)
