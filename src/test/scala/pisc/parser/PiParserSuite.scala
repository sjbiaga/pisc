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

package pisc
package parser

import scala.collection.mutable.{ LinkedHashMap => Map2 }

import scala.meta.{ Position => _, _ }

import munit.FunSuite

import Pi._
import Calculus._
import Encoding._
import PiParserSuite._


class PiParserSuite extends FunSuite:

  test("ident - start with lowercase") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(ident, "ch") match
          case Success("ch", _) =>
            assert(true)
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("ident - not start with lowercase") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(ident, "_ch") match
          case Failure("channel name expected but '_' found", _) =>
            assert(true)
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("ident - with quotes") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(ident, "ch\"'") match
          case Success("ch\"'", _) =>
            assert(true)
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("ident - with not identifier") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(ident, "ch/") match
          case Failure("channel name part expected", _) =>
            assert(true)
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("name - ident") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(name, "ch") match
          case Success((λ(Symbol("ch")), free), _) =>
            assertEquals(free, Names() + Symbol("ch"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("name - floatingPointNumber") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(name, "0.5") match
          case Success((λ(fpn), free), _) =>
            assertEquals(fpn, BigDecimal("0.5"))
            assert(free.isEmpty)
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("name - stringLiteral") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(name, "\"string\"") match
          case Success((λ("\"string\""), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("name - Boolean") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(name, "False") match
          case Success((λ(false), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("name - expression term") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(name, "/*'n*/") match
          case Success((λ(Expr(Term.Name("n"))), free), _) =>
            assertEquals(free, Names() + Symbol("n"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("name - expression enums") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(name, "/*_ <- IO.println()*/")
    }

    interceptMessage[TermParsingException]("The embedded Scalameta should be a Term, not Enumerator `List(_ <- IO.println())'") {
      `13`.test
    }

  }

  test("μ. - silent - expression with a symbol") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "τ /*println('ch)*/") match
          case Success((τ(Some(_)), (binding, free)), _) =>
            assert(binding.isEmpty)
            assertEquals(free, Names() + Symbol("ch"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("μ. - output - channel not a name") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "True < >")
    }

    interceptMessage[PrefixChannelParsingException]("true is not a channel name but a True False") {
      `13`.test
    }

  }

  test("μ. - output - null - no expression") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch<>") match
          case Success((π(λ(Symbol("ch")), λ(Expr(Term.Apply(Term.Name("()"), Lit.Null() :: Nil))), false, None), (binding, free)), _) =>
            assert(binding.isEmpty)
            assertEquals(free, Names() + Symbol("ch"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("μ. - output - name - no expression") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch<n>") match
          case Success((π(λ(Symbol("ch")), λ(Symbol("n")), false, None), (binding, free)), _) =>
            assert(binding.isEmpty)
            assertEquals(free, Names() + Symbol("ch") + Symbol("n"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("μ. - output - name - expression with a symbol") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch<n> /*println('m)*/") match
          case Success((π(λ(Symbol("ch")), λ(Symbol("n")), false, Some(_)), (binding, free)), _) =>
            assert(binding.isEmpty)
            assertEquals(free, Names() + Symbol("ch") + Symbol("n") + Symbol("m"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("μ. - output - value - no expression") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch<True>") match
          case Success((π(λ(Symbol("ch")), λ(true), false, None), (binding, free)), _) =>
            assert(binding.isEmpty)
            assertEquals(free, Names() + Symbol("ch"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("μ. - output - value - expression with a symbol") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch<True> /*println('n)*/") match
          case Success((π(λ(Symbol("ch")), λ(true), false, Some(_)), (binding, free)), _) =>
            assert(binding.isEmpty)
            assertEquals(free, Names() + Symbol("ch") + Symbol("n"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("μ. - output - expression - no expression") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch</*0*/>") match
          case Success((π(λ(Symbol("ch")), λ(Expr(Lit.Int(0))), false, None), (binding, free)), _) =>
            assert(binding.isEmpty)
            assertEquals(free, Names() + Symbol("ch"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("μ. - output - expression - expression with a symbol") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch</*0*/> /*println('n)*/") match
          case Success((π(λ(Symbol("ch")), λ(Expr(Lit.Int(0))), false, Some(_)), (binding, free)), _) =>
            assert(binding.isEmpty)
            assertEquals(free, Names() + Symbol("ch") + Symbol("n"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("μ. - output - null - expression with a symbol") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch<>/*println('n)*/") match
          case Success((π(λ(Symbol("ch")), λ(Expr(Term.Apply(Term.Name("()"), Lit.Null() :: Nil))), false, Some(_)), (binding, free)), _) =>
            assert(binding.isEmpty)
            assertEquals(free, Names() + Symbol("ch") + Symbol("n"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("μ. - input - channel not a name") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "True(ch)")
    }

    interceptMessage[PrefixChannelParsingException]("true is not a channel name but a True False") {
      `13`.test
    }

  }

  test("μ. - input - parameter not a channel name") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch(True)")
    }

    interceptMessage[PrefixChannelParsingException]("true is not a channel name but a True False") {
      `13`.test
    }

  }

  test("μ. - input - expression enums") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch(n) /*_ <- IO.println()*/")
    }

    interceptMessage[TermParsingException]("The embedded Scalameta should be a Term, not Enumerator `List(_ <- IO.println())'") {
      `13`.test
    }

  }

  test("μ. - input - no expression") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch(n)") match
          case Success((π(λ(Symbol("ch")), λ(Symbol("n")), true, None), (binding, free)), _) =>
            assertEquals(binding, Names() + Symbol("n"))
            assertEquals(free, Names() + Symbol("ch"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("μ. - input - expression with a symbol") {

    val `13` = new PiParserTest {
      override def test =
        parseAll(`μ.`, "ch(n) /*println('n)*/") match
          case Success((π(λ(Symbol("ch")), λ(Symbol("n")), true, Some(_)), (binding, free)), _) =>
            assertEquals(binding, Names() + Symbol("n"))
            assertEquals(free, Names() + Symbol("ch") + Symbol("n"))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("Names2 - encoded binding occurrence - parameter") {

    val `13` = new PiParserTest {
      override def test =
        _nest = 0
        _code = 0
        given binding2: Names2 = Map2(Symbol("x") -> Occurrence(None, Position(-1, false)))
        Names2Occurrence(Symbol("x"), Some(Symbol("x_shadow")))
        binding2.head match
          case (Symbol("x"), it @ Shadow(Symbol("x_shadow"))) =>
            assertEquals(it, Occurrence(Some(Symbol("x_shadow")), Position(-1, true)))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("Names2 - hardcoded binding occurrence - parameter") {

    val `13` = new PiParserTest {
      override def test =
        _nest = 0
        _code = 0
        given binding2: Names2 = Map2(Symbol("x") -> Occurrence(None, Position(-1, false)))
        Names2Occurrence(Symbol("x"), Some(Symbol("x_shadow")), hardcoded = true)
        binding2.head match
          case (Symbol("x"), it @ Shadow(Symbol("x_shadow"))) =>
            assertEquals(it, Occurrence(Some(Symbol("x_shadow")), Position(-1, true)))
          case _ =>
            assert(false)
    }

    `13`.test

  }

  test("Names2 - encoded binding occurrence - parameter - uniqueness") {

    val `13` = new PiParserTest {
      override def test =
        _nest = 0
        _code = 0
        given binding2: Names2 = Map2(Symbol("x") -> Occurrence(None, Position(-1, false)))
        Names2Occurrence(Symbol("x"), Some(Symbol("x_shadow")))
        Names2Occurrence(Symbol("x"), Some(Symbol("x_shadow2")))
    }

    interceptMessage[UniquenessBindingParsingException]("A binding name (x) does not correspond to a unique encoded binding occurrence, but is duplicated at nesting level #0 in the right hand side of encoding 0") {
      `13`.test
    }

  }

  test("Names2 - hardcoded binding occurrence - parameter - uniqueness") {

    val `13` = new PiParserTest {
      override def test =
        _nest = 0
        _code = 0
        given binding2: Names2 = Map2(Symbol("x") -> Occurrence(None, Position(-1, false)))
        Names2Occurrence(Symbol("x"), Some(Symbol("x_shadow")), hardcoded = true)
        Names2Occurrence(Symbol("x"), Some(Symbol("x_shadow2")), hardcoded = true)
    }

    interceptMessage[UniquenessBindingParsingException]("A binding name (x) does not correspond to a unique hardcoded binding occurrence, but is duplicated at nesting level #0 in the right hand side of encoding 0") {
      `13`.test
    }

  }

  test("Names2 - encoded binding occurrence - non-parameter") {

    val `13` = new PiParserTest {
      override def test =
        _nest = 0
        _code = 0
        given binding2: Names2 = Map2(Symbol("x") -> Occurrence(None, Position(1, false)))
        Names2Occurrence(Symbol("x"), Some(Symbol("x_shadow")))
    }

    interceptMessage[NonParameterBindingParsingException]("A binding name (x) in an encoded binding occurrence does not correspond to a parameter at nesting level #0 in the right hand side of encoding 0") {
      `13`.test
    }

  }

  test("Names2 - hardcoded binding occurrence - non-parameter") {

    val `13` = new PiParserTest {
      override def test =
        _nest = 0
        _code = 0
        given binding2: Names2 = Map2(Symbol("x") -> Occurrence(None, Position(1, false)))
        Names2Occurrence(Symbol("x"), Some(Symbol("x_shadow")), hardcoded = true)
    }

    interceptMessage[NonParameterBindingParsingException]("A binding name (x) in a hardcoded binding occurrence does not correspond to a parameter at nesting level #0 in the right hand side of encoding 0") {
      `13`.test
    }

  }

  test("Names2 - binding occurrence - agent parameter") {

    val `13` = new PiParserTest {
      override def test =
        _code = -1
        given binding2: Names2 = Map2(Symbol("x") -> Occurrence(None, Position(1, false)))
        Names2Occurrence(Symbol("x"), Some(Symbol("x_shadow")))
        binding2.head match
          case (Symbol("x"), Shadow(Symbol("x_shadow"))) =>
            assert(false)
          case _ =>
            assert(true)
    }

    `13`.test

  }


object PiParserSuite:

  import scala.util.matching.Regex

  abstract class PiParserTest extends Pi:
    def regexMatch(_r: Regex): Parser[Regex.Match] = ???

    def test: Unit