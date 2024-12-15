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

import scala.collection.mutable.{ HashMap => Map, LinkedHashSet => Set }

import munit.FunSuite

import Pi._
import Calculus._
import Encoding._
import CalculusParserSuite._


class CalculusParserSuite extends FunSuite:

  test("qual - repeated") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(qual, "{path}{to}{package}") match
          case Success(List("path", "to", "package"), _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }

  test("IDENT - start with uppercase") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(IDENT, "P") match
          case Success("P", _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }

  test("IDENT - not start with uppercase") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(IDENT, "_P") match
          case Failure("agent identifier expected but '_' found", _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }

  test("IDENT - with quotes") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(IDENT, "P\"'") match
          case Success("P\"'", _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }

  test("IDENT - with not IDENTIFIER") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(IDENT, "P/") match
          case Failure("agent identifier part expected", _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }

  test("invocation - equation with qualifier") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(invocation(true), "{path}{to}{package}P")

    interceptMessage[EquationQualifiedException]("A qualified package path.to.package is present in the left hand side of P") {
      `13`.test
    }

  }

  test("invocation - equation with non-symbol parameters") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(invocation(true), "P(True, \"string\", m, n)")

    interceptMessage[EquationParamsException]("""The "formal" parameters (True, "string") are not names in the left hand side of P""") {
      `13`.test
    }

  }

  test("invocation - Self with parameters") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(invocation(), "Self(m, n)") match
          case Success((`(*)`("Self_0", _, λ(Symbol("m")), λ(Symbol("n"))), free), _) =>
            assertEquals(free, Names() + Symbol("m") + Symbol("n"))
          case _ =>
            assert(false)
      _code = 0
      self = Set()

    `13`.test

  }

  test("invocation - Self without parameters") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(invocation(), "Self") match
          case Success((`(*)`("Self_0", _), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)
      _code = 0
      self = Set()

    `13`.test

  }

  test("invocation - Self with code and with parameters") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(invocation(), "Self_2(m, n)") match
          case Success((`(*)`("Self_2", _, λ(Symbol("m")), λ(Symbol("n"))), free), _) =>
            assertEquals(free, Names() + Symbol("m") + Symbol("n"))
          case _ =>
            assert(false)
      self = Set()

    `13`.test

  }

  test("invocation - Self with code and without parameters") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(invocation(), "Self_0") match
          case Success((`(*)`("Self_0", _), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)
      self = Set()

    `13`.test

  }

  test("invocation - identifier with parameters") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(invocation(), "P(m, n)") match
          case Success((`(*)`("P", _, λ(Symbol("m")), λ(Symbol("n"))), free), _) =>
            assertEquals(free, Names() + Symbol("m") + Symbol("n"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("invocation - identifier without parameters") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(invocation(), "P") match
          case Success((`(*)`("P", _), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("invocation - identifier with empty parentheses") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(invocation(), "P()") match
          case Failure(_, _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }

  test("condition - match names") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(condition, "(( m = n ))") match
          case Success((((λ(Symbol("m")), λ(Symbol("n"))), false), free), _) =>
            assertEquals(free, Names() + Symbol("m") + Symbol("n"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("condition - match values") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(condition, "(( True = False ))") match
          case Success((((λ(true), λ(false)), false), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("condition - mismatch names") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(condition, "(( m ≠ n ))") match
          case Success((((λ(Symbol("m")), λ(Symbol("n"))), true), free), _) =>
            assertEquals(free, Names() + Symbol("m") + Symbol("n"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("condition - mismatch values") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(condition, "(( True ≠ False ))") match
          case Success((((λ(true), λ(false)), true), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("prefix - restriction with empty parentheses") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(prefix(using Names2()), "ν()") match
          case Failure(_, _) =>
            assert(true)
          case _ =>
            assert(false)

    `13`.test

  }

  test("prefix - restriction with non-names") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(prefix(using Names2()), "ν(True, \"string\", m, n)")

    interceptMessage[PrefixChannelsParsingException]("""True, "string" are not channel names but True False, string literal""") {
      `13`.test
    }

  }

  test("prefix - restriction without binding2 check") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(prefix(using Names2()), "ν(m, n)") match
          case Success((ν("m", "n"), (binding, free)), _) =>
            assertEquals(binding, Names() + Symbol("m") + Symbol("n"))
            assert(free.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("prefix - restriction with binding2 check") {

    val `13` = new CalculusParserTest:
      override def test =
        given binding2: Names2 = Names2()
        parseAll(prefix, "ν(m)") match
          case Success((ν("m"), _), _) =>
            binding2.headOption match
              case Some((Symbol("m"), Occurrence(None, Position(1, true)))) =>
                assert(true)
              case _ =>
                assert(false)
          case _ =>
            assert(false)
      _code = -1

    `13`.test

  }

  test("prefix - μ. - input without binding2 check") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(prefix(using Names2()), "ch(n).") match
          case Success((π(λ(Symbol("ch")), λ(Symbol("n")), true, None), (binding, free)), _) =>
            assertEquals(binding, Names() + Symbol("n"))
            assertEquals(free, Names() + Symbol("ch"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("prefix - μ. - input with binding2 check") {

    val `13` = new CalculusParserTest:
      override def test =
        given binding2: Names2 = Names2()
        parseAll(prefix, "ch(n).") match
          case Success((π(λ(Symbol("ch")), λ(Symbol("n")), true, None), (binding, free)), _) =>
            assertEquals(binding, Names() + Symbol("n"))
            assertEquals(free, Names() + Symbol("ch"))
            binding2.headOption match
              case Some((Symbol("n"), Occurrence(None, Position(1, true)))) =>
                assert(true)
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("prefix - μ. - output") {

    val `13` = new CalculusParserTest:
      override def test =
        given binding2: Names2 = Names2()
        parseAll(prefix, "ch<n>.") match
          case Success((π(λ(Symbol("ch")), λ(Symbol("n")), false, None), (binding, free)), _) =>
            assert(binding.isEmpty)
            assertEquals(free, Names() + Symbol("ch") + Symbol("n"))
            assert(binding2.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }
  test("leaf - conditional - match") {

    val `13` = new CalculusParserTest:
      override def test =
        given binding2: Names2 = Names2()
        parseAll(leaf, "[m = n]") match
          case Success((?:(((λ(Symbol("m")), λ(Symbol("n"))), false), ∅, None), free), _) =>
            assertEquals(free, Names() + Symbol("m") + Symbol("n"))
            assert(binding2.isEmpty)
          case it =>
            assert(false)

    `13`.test

  }

  test("leaf - conditional - if-then-else") {

    val `13` = new CalculusParserTest:
      override def test =
        given binding2: Names2 = Names2()
        parseAll(leaf, "if True ≠ False then else") match
          case Success((?:(((λ(true), λ(false)), true), ∅, Some(∅)), free), _) =>
            assert(free.isEmpty)
            assert(binding2.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - conditional - elvis operator") {

    val `13` = new CalculusParserTest:
      override def test =
        given binding2: Names2 = Names2()
        parseAll(leaf, "False ≠ True ? :") match
          case Success((?:(((λ(false), λ(true)), true), ∅, Some(∅)), free), _) =>
            assert(free.isEmpty)
            assert(binding2.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - replication - unguarded") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(leaf(using Names2()), "!") match
          case Success((!(None, ∅), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - replication - input guard - subject and object identical") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(leaf(using Names2()), "!.ch(ch).")
      _werr = true

    interceptMessage[GuardParsingException]("ch is both the channel name and the binding parameter name in an input guard") {
      `13`.test
    }

  }

  test("leaf - replication - input guard - without binding2 check") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(leaf(using Names2()), "!.ch(n).") match
          case Success((!(Some(π(λ(Symbol("ch")), λ(Symbol("n")), true, None)), ∅), free), _) =>
            assertEquals(free, Names() + Symbol("ch"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - replication - input guard - with binding2 check") {

    val `13` = new CalculusParserTest:
      override def test =
        given binding2: Names2 = Names2()
        parseAll(leaf, "!.ch(n).") match
          case Success((!(Some(π(λ(Symbol("ch")), λ(Symbol("n")), true, None)), ∅), free), _) =>
            assertEquals(free, Names() + Symbol("ch"))
            binding2.headOption match
              case Some((Symbol("n"), Occurrence(None, Position(1, true)))) =>
                assert(true)
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - replication - output guard") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(leaf(using Names2()), "!.ch<n>/*println('m)*/.") match
          case Success((!(Some(π(λ(Symbol("ch")), λ(Symbol("n")), false, Some(_))), ∅), free), _) =>
            assertEquals(free, Names() + Symbol("ch") + Symbol("n") + Symbol("m"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - replication - silent prefix guard") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(leaf(using Names2()), "!.τ/*println('n)*/.") match
          case Success((!(Some(τ(Some(_))), ∅), free), _) =>
            assertEquals(free, Names() + Symbol("n"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("equation - free names - one") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(equation, "P(m) = ch<>.")

    interceptMessage[EquationFreeNamesException]("The free names (ch) in the right hand side are not formal parameters of the left hand side of P") {
      `13`.test
    }

  }

  test("equation - free names - one of two") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(equation, "P(m, n) = ch<n>.")

    interceptMessage[EquationFreeNamesException]("The free names (ch) in the right hand side are not formal parameters of the left hand side of P") {
      `13`.test
    }

  }

  test("equation - free names - one of one") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(equation, "P(m) = ν(n) ch<n>.")

    interceptMessage[EquationFreeNamesException]("The free names (ch) in the right hand side are not formal parameters of the left hand side of P") {
      `13`.test
    }

  }


object CalculusParserSuite:

  import scala.util.matching.Regex

  abstract class CalculusParserTest extends Calculus:
    def regexMatch(_r: Regex): Parser[Regex.Match] = ???
    def instantiation(using Names2): Parser[(`⟦⟧`, Names)] =
      new Parser[(`⟦⟧`, Names)]:
        override def apply(_in: Input): ParseResult[(`⟦⟧`, Names)] =
          Failure(null, _in)
    def capital: Parser[(`{}`, Names)] =
      new Parser[(`{}`, Names)]:
        override def apply(_in: Input): ParseResult[(`{}`, Names)] =
          Failure(null, _in)

    _nest = 0
    _cntr = Map(0 -> 0L)

    def test: Unit
