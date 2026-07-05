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

import scala.collection.mutable.{
  LinkedHashMap => Map,
  LinkedHashSet => Set
}

import scala.io.Source

import munit.FunSuite

import Pi.*
import Calculus.*
import Encoding.*
import PiOccurrenceSuite.*


class PiOccurrenceSuite extends FunSuite:

  given Int = 1

  /**
    * `⟦13 'x ^ 'y 13⟧(k) = k<x>. k(y).`
    *
    * `⟦13 'x 13⟧(k) = ⟦ x ^ x ⟧`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, Position(counter, _), true)) if counter < 0 =>
    *       throw UniquenessBindingParsingException(_code, _nest, name, hardcoded, "clobbered")
    * }}}
    */
  test("occurrence - definition - parameter - clobbered binding") {
    val negative = -1
    val name = Symbol("x")
    val parameter = Occurrence(shadow = None, Position(counter = negative, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(13)

    parser.pendingOccurrenceObject(name)

    interceptMessage[UniquenessBindingParsingException](s"A binding name (${name.name}) does not correspond to a unique hardcoded binding occurrence, being clobbered at nesting level #0 in the right hand side of definition 13") {
      parser.bindingOccurrenceObject(Set(name))
    }
  }

  /**
    * `⟦13 'x ^ 'y 13⟧(k) = k<x>. k(y).`
    *
    * `P13(a) = ⟦ a ^ a ⟧`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, Position(counter, _), true)) if counter < 0 => // not the case
    *       ...
    *     case Some(Occurrence(_, it @ Position(counter, false), _)) =>
    *       if counter < 0 // not the case
    *       then
    *         ...
    *       else
    *         throw ExistingNonParameterBindingParsingException(_code, _nest, name, hardcoded)
    * }}}
    */
  test("occurrence - equation - parameter - pending - encoded binding") {
    val positive = 1
    val name = Symbol("a")
    val shadow = Some(Symbol("y_shadow"))
    val parameter = Occurrence(shadow = None, Position(counter = positive, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(-1)

    parser.pendingOccurrenceObject(name)

    interceptMessage[ExistingNonParameterBindingParsingException](s"A binding name (${name.name}) in an encoded binding occurrence already exists and not as a definition parameter at nesting level #0") {
      parser.bindingOccurrenceObject(name, shadow)
    }
  }

  /**
    * `⟦13 'x ^ 'P 13⟧(k) = k<x>. P{}`
    *
    * `P13(a, k) = ⟦ a ^ ν(a) ⟧`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, Position(counter, _), true)) if counter < 0 => // not the case
    *     case Some(Occurrence(_, Position(counter, false), _)) if counter > 0 =>
    *       bindings += name -> Occurrence(shadow, pos(true))
    * }}}
    */
  test("occurrence - equation - parameter - pending - hardcoded binding") {
    val positive = 1
    val name = Symbol("a")
    val parameter = Occurrence(shadow = None, Position(counter = positive, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(-1)
    val _ = parser.pos() // increment counter

    parser.pendingOccurrenceObject(name)

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(None, Position(counter @ 1, binds @ false), pending @ false)) => true
    }

    parser.bindingOccurrenceObject(Set(name))

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(None, Position(counter @ 2, binds @ true), pending @ false)) => true
    }
  }

  /**
    * `⟦13 'x ^ 'P 13⟧(k) = 0* k(x). P{}`
    *
    * `⟦13 'x ^ 'P 13⟧(k) = 2* k(x). P{}`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, it @ Position(counter, binds @ false), _)) if counter < 0 && scaling != 1 =>
    *       throw UniquenessBindingParsingException(_code, _nest, name, hardcoded, "scaled")
    * }}}
    */
  test("occurrence - definition - parameter - hardcoded binding - scaled binding") {
    val negative = -1
    val name = Symbol("x")
    val parameter = Occurrence(shadow = None, Position(counter = negative, binds = false))
    given Bindings = Map(name -> parameter)
    given Int = 0

    val parser = new PiMain

    parser._code(13)

    interceptMessage[UniquenessBindingParsingException](s"A binding name (${name.name}) does not correspond to a unique hardcoded binding occurrence, being scaled at nesting level #0 in the right hand side of definition 13") {
      parser.bindingOccurrenceObject(Set(name))
    }
  }

  /**
    * `⟦13 'x ^ 'P 13⟧(k) = k(x). P{}`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, it @ Position(counter, binds @ false), _)) if counter < 0 => // scaling == 1
    *       bindings += name -> Occurrence(shadow, it.copy(binds = true))
    * }}}
    */
  test("occurrence - definition - parameter - hardcoded binding") {
    val negative = -1
    val name = Symbol("x")
    val parameter = Occurrence(shadow = None, Position(counter = negative, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(13)

    parser.bindingOccurrenceObject(Set(name))

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(shadow @ Some(`name`), Position(counter @ `negative`, binds @ true), pending @ false)) => true
    }
  }

  /**
    * `⟦ 'x ^ 'P ⟧(k) = k(x). P{}`
    *
    * `⟦13 'z 13⟧ = ⟦ z ^ () ⟧`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, it @ Position(counter, binds @ false), _)) if counter < 0 => // scaling == 1
    *       bindings += name -> Occurrence(shadow, it.copy(binds = true))
    * }}}
    */
  test("occurrence - definition - parameter - encoded binding") {
    val negative = -1
    val name = Symbol("z")
    val shadow = Some(Symbol("x_shadow"))
    val parameter = Occurrence(shadow = None, Position(counter = negative, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(13)

    parser.bindingOccurrenceObject(name, shadow)

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(`shadow`, Position(counter @ `negative`, binds @ true), pending @ false)) => true
    }
  }

  /**
    * `⟦13 'x 13⟧ = ν(x)`
    *
    * `P13(a) = ⟦13 a 13⟧`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, Position(_, false), _)) if counter > 0 && !hardcoded =>
    *       throw ExistingNonParameterBindingParsingException(_code, _nest, name, hardcoded)
    * }}}
    */
  test("occurrence - equation - parameter - encoded binding") {
    val positive = 1
    val name = Symbol("a")
    val shadow = Some(Symbol("x_shadow"))
    val parameter = Occurrence(shadow = None, Position(counter = positive, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(-1)

    interceptMessage[ExistingNonParameterBindingParsingException](s"A binding name (${name.name}) in an encoded binding occurrence already exists and not as a definition parameter at nesting level #0") {
      parser.bindingOccurrenceObject(name, shadow)
    }
  }

  /**
    * `⟦13 'x 13⟧ = ν(x)`
    *
    * `P13() = ν(n) ⟦13 n 13⟧`
    *
    * `P13(m) = m(n). ⟦13 n 13⟧`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, Position(_, true), _)) if counter > 0 && !hardcoded =>
    *       throw ExistingNonParameterBindingParsingException(_code, _nest, name, hardcoded)
    * }}}
    */
  test("occurrence - equation - new name | input prefix - encoded binding") {
    val positive = 1
    val name = Symbol("n")
    val shadow = Some(Symbol("x_shadow"))
    val occurrence = Occurrence(shadow = Some(name), Position(counter = positive, binds = true))
    given Bindings = Map(name -> occurrence)

    val parser = new PiMain

    parser._code(-1)

    interceptMessage[ExistingNonParameterBindingParsingException](s"A binding name (${name.name}) in an encoded binding occurrence already exists and not as a definition parameter at nesting level #0") {
      parser.bindingOccurrenceObject(name, shadow)
    }
  }

  /**
    * `⟦ 'x ⟧ = ν(x)`
    *
    * `⟦13 'x ^ 'P 13⟧(k) = ⟦ k ⟧`
    *
    * `⟦13 'x ^ 'P 13⟧{p} = ⟦ p ⟧`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, Position(_, false), _)) if counter > 0 && !hardcoded =>
    *       throw ExistingNonParameterBindingParsingException(_code, _nest, name, hardcoded)
    * }}}
    */
  test("occurrence - definition - constant | variable - encoded binding") {
    val positive = 1
    val name = Symbol("k")
    val shadow = Some(Symbol("x_shadow"))
    val constant_or_variable = Occurrence(shadow = None, Position(counter = positive, binds = false))
    given Bindings = Map(name -> constant_or_variable)

    val parser = new PiMain

    parser._code(13)

    interceptMessage[ExistingNonParameterBindingParsingException](s"A binding name (${name.name}) in an encoded binding occurrence already exists and not as a definition parameter at nesting level #0 in the right hand side of definition 13") {
      parser.bindingOccurrenceObject(name, shadow)
    }
  }

  /**
    * `⟦ 'x ⟧ = ν(x)`
    *
    * `⟦13 'x ^ 'P 13⟧ = ν(n) ⟦ n ⟧`
    *
    * `⟦13 'x ^ 'P 13⟧ = x(n). ⟦ n ⟧`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, Position(_, true), _)) if counter > 0 && !hardcoded =>
    *       throw ExistingNonParameterBindingParsingException(_code, _nest, name, hardcoded)
    * }}}
    */
  test("occurrence - definition - new name | input prefix - encoded binding") {
    val positive = 1
    val name = Symbol("n")
    val shadow = Some(Symbol("x_shadow"))
    val occcurrence = Occurrence(shadow = Some(name), Position(counter = positive, binds = true))
    given Bindings = Map(name -> occcurrence)

    val parser = new PiMain

    parser._code(13)

    interceptMessage[ExistingNonParameterBindingParsingException](s"A binding name (${name.name}) in an encoded binding occurrence already exists and not as a definition parameter at nesting level #0 in the right hand side of definition 13") {
      parser.bindingOccurrenceObject(name, shadow)
    }
  }

  /**
    * `⟦13 'x 13⟧ = ν(x) x(x).`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     ... // scaling != 1
    *     case Some(Occurrence(_, it @ Position(counter, binds @ false), _)) if counter < 0 => // first
    *       bindings += name -> Occurrence(shadow, it.copy(binds = true))
    *     case Some(Occurrence(shadow @ Some(_), Position(counter, binds @ true), _)) if counter < 0 => // second
    *       throw UniquenessBindingParsingException(_code, _nest, name, hardcoded, "duplicated")
    * }}}
    */
  test("occurrence - definition - parameter - new name & input prefix - hardcoded binding - twice") {
    val negative = -1
    val name = Symbol("x")
    val parameter = Occurrence(shadow = None, Position(counter = negative, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(13)

    parser.bindingOccurrenceObject(Set(name))

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(shadow @ Some(`name`), Position(counter @ `negative`, binds @ true), pending @ false)) => true
    }

    interceptMessage[UniquenessBindingParsingException](s"A binding name (${name.name}) does not correspond to a unique hardcoded binding occurrence, being duplicated at nesting level #0 in the right hand side of definition 13") {
      parser.bindingOccurrenceObject(Set(name))
    }
  }

  /**
    * `⟦13 'x 13⟧ = ν(x)`
    *
    * `P13 = ⟦ n ⟧`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case None =>
    *       bindings += name -> Occurrence(shadow, pos(true))
    * }}}
    */
  test("occurrence - nonexistent - encoded binding") {
    val positive = 1
    val name = Symbol("n")
    val shadow = Some(Symbol("x_shadow"))
    given Bindings = Bindings()

    val parser = new PiMain

    parser._code(-1)

    parser.bindingOccurrenceObject(name, shadow)

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(`shadow`, Position(counter @ positive, binds @ true), pending @ false)) => true
    }
  }

  /**
    * `⟦ 'x ^ 'y ⟧ = ν(x) x(y).`
    *
    * `⟦13 'z 13⟧ = ⟦ z ^ z ⟧`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     ... // scaling != 1
    *     case Some(Occurrence(_, it @ Position(counter, binds @ false), _)) if counter < 0 => // first
    *       bindings += name -> Occurrence(shadow, it.copy(binds = true))
    *     case Some(Occurrence(shadow @ Some(_), Position(counter, binds @ true), _)) if counter < 0 => // second
    *       throw UniquenessBindingParsingException(_code, _nest, name, hardcoded, "duplicated")
    * }}}
    */
  test("occurrence - definition - parameter - encoded binding - twice") {
    val negative = -1
    val name = Symbol("z")
    val shadow = Some(Symbol("x_shadow"))
    val shadowʹ = Some(Symbol("y_shadow"))
    val parameter = Occurrence(shadow = None, Position(counter = negative, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(13)

    parser.bindingOccurrenceObject(name, shadow)

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(`shadow`, Position(counter @ `negative`, binds @ true), pending @ false)) => true
    }

    interceptMessage[UniquenessBindingParsingException](s"A binding name (${name.name}) does not correspond to a unique encoded binding occurrence, being duplicated at nesting level #0 in the right hand side of definition 13") {
      parser.bindingOccurrenceObject(name, shadowʹ)
    }
  }

  /**
    * `P13() = ν(x)`
    *
    * `P13() = ν(x) ν(x)`
    *
    * `P13(k) = k(x).`
    *
    * `P13(k) = k(x). k(x).`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     ... // everything else was not the case
    *     case None =>
    *       bindings += name -> Occurrence(shadow, pos(true))
    *     case Some(Occurrence(_, Position(counter, binds @ true), _)) if counter > 0 && hardcoded => // bind anew
    *       bindings += name -> Occurrence(shadow, pos(true))
    * }}}
    */
  test("occurrence - equation - new name | input prefix - hardcoded binding - once or more") {
    val name = Symbol("x")
    given Bindings = Bindings()

    val parser = new PiMain

    parser._code(-1)

    parser.bindingOccurrenceObject(Set(name))

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(shadow @ None, Position(counter @ 1, binds @ true), pending @ false)) => true
    }

    parser.bindingOccurrenceObject(Set(name))

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(shadow @ None, Position(counter @ 2, binds @ true), pending @ false)) => true
    }
  }

  /**
    * `P13(a) = ν(a)`
    *
    * `P13(a, b) = b(a).`
    *
    * @example {{{
    * def apply(name: Symbol, shadow: Option[Symbol], hardcoded: Boolean = false)
    *          (using bindings: Bindings, scaling: Int): Unit =
    *   bindings.get(name) match
    *     case Some(Occurrence(_, it @ Position(counter, false), _)) if counter > 0 && hardcoded =>
    *       bindings += name -> Occurrence(shadow, pos(true))
    * }}}
    */
  test("occurrence - equation - parameter - new name | input prefix - hardcoded binding") {
    val positive = 1
    val name = Symbol("a")
    val parameter = Occurrence(shadow = None, Position(counter = positive, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(-1)
    val _ = parser.pos() // increment counter

    parser.bindingOccurrenceObject(Set(name))

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(shadow @ None, Position(counter @ 2, binds @ true), pending @ false)) => true
    }
  }

  /**
    * `⟦ 'x ⟧ = x<x>.`
    *
    * `P13(a) = ⟦ a ⟧`
    */
  test("occurrence - equation - parameter - encoded binding - pending not applicable") {
    val positive = 1
    val name = Symbol("a")
    val parameter = Occurrence(shadow = None, Position(counter = positive, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(-1)

    parser.pendingOccurrenceObject(name)

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(None, Position(counter @ `positive`, binds @ false), pending @ false)) => true
    }
  }

  /**
    * `⟦ 'x ^ 'y ⟧ = ν(x) x<y>.`
    *
    * `⟦13 'z 13⟧ = ⟦ z ^ z ⟧`
    */
  test("occurrence - definition - parameter - encoded binding - pending not applicable") {
    val negative = -1
    val name = Symbol("z")
    val shadow = Some(Symbol("x_shadow"))
    val parameter = Occurrence(shadow = None, Position(counter = negative, binds = false))
    given Bindings = Map(name -> parameter)

    val parser = new PiMain

    parser._code(13)

    parser.bindingOccurrenceObject(name, shadow)

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(`shadow`, Position(counter @ negative, binds @ true), pending @ false)) => true
    }

    parser.pendingOccurrenceObject(name)

    assertMatches(given_Bindings.get(name)) {
      case Some(Occurrence(`shadow`, Position(counter @ negative, binds @ true), pending @ false)) => true
    }
  }


object PiOccurrenceSuite:

  class PiMain extends Pi.Main(Emitter.test, getClass.getSimpleName):
    _init
    _cntr = Map(0 -> 0L)
    _nth = Map(0 -> 0L)

    import scala.util.matching.Regex

    override def regexMatch(r: Regex) = ???
    override def ln: String = "line #0"

    def _code(code: Int) : Unit = _code = code

    val bindingOccurrenceObject = BindingOccurrence
    val pendingOccurrenceObject = PendingOccurrence

  def source(src: String) = Source.fromString(src)
