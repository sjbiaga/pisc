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

import scala.collection.mutable.{ LinkedHashMap => Map, LinkedHashSet => Set }

import munit.FunSuite

import Pi.*
import Calculus.*
import Encoding.*
import CalculusParserSuite.*
import scala.util.parsing.combinator.pisc.parser.Expansion.Duplications


class CalculusParserSuite extends FunSuite:

  test("qual - repeated") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(qual, "{path}{to}{package}") match
          case Success(List("path", "to", "package"), _) =>
          case _ =>
            assert(false)

    `13`.test

  }

  test("IDENT - start with uppercase") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(IDENT, "P") match
          case Success("P", _) =>
          case _ =>
            assert(false)

    `13`.test

  }

  test("IDENT - not start with uppercase") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(IDENT, "_P") match
          case Failure("agent identifier expected but '_' found", _) =>
          case _ =>
            assert(false)

    `13`.test

  }

  test("IDENT - with quotes") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(IDENT, "P\"'") match
          case Success("P\"'", _) =>
          case _ =>
            assert(false)

    `13`.test

  }

  test("IDENT - with not IDENTIFIER") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(IDENT, "P/") match
          case Failure("agent identifier part expected", _) =>
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
        parseAll(prefixes(using Bindings()), "ν()") match
          case Failure(_, _) =>
          case _ =>
            assert(false)

    `13`.test

  }

  test("prefix - restriction with non-names") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(prefixes(using Bindings()), "ν(True, \"string\", m, n)")

    interceptMessage[PrefixChannelsParsingException]("""True, "string" are not channel names but True False, string literal""") {
      `13`.test
    }

  }

  test("prefix - restriction without bindings check") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(prefixes(using Bindings()), "ν(m, n)") match
          case Success((ν("m", "n") :: Nil, (bound, free)), _) =>
            assertEquals(bound, Names() + Symbol("m") + Symbol("n"))
            assert(free.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("prefix - restriction with bindings check") {

    val `13` = new CalculusParserTest:
      override def test =
        given bindings: Bindings = Bindings()
        parseAll(prefixes, "ν(m)") match
          case Success((ν("m") :: Nil, _), _) =>
            bindings.headOption match
              case Some((Symbol("m"), Occurrence(None, Position(1, true)))) =>
              case _ =>
                assert(false)
          case _ =>
            assert(false)
      _code = -1

    `13`.test

  }

  test("prefix - μ. - input without bindings check") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(prefixes(using Bindings()), "ch(n).") match
          case Success((π(λ(Symbol("ch")), λ(Symbol("n")), true, None) :: Nil, (bound, free)), _) =>
            assertEquals(bound, Names() + Symbol("n"))
            assertEquals(free, Names() + Symbol("ch"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("prefix - μ. - input with bindings check") {

    val `13` = new CalculusParserTest:
      override def test =
        given bindings: Bindings = Bindings()
        parseAll(prefixes, "ch(n).") match
          case Success((π(λ(Symbol("ch")), λ(Symbol("n")), true, None) :: Nil, (bound, free)), _) =>
            assertEquals(bound, Names() + Symbol("n"))
            assertEquals(free, Names() + Symbol("ch"))
            bindings.headOption match
              case Some((Symbol("n"), Occurrence(None, Position(1, true)))) =>
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("prefix - μ. - output") {

    val `13` = new CalculusParserTest:
      override def test =
        given bindings: Bindings = Bindings()
        parseAll(prefix, "ch<n>.") match
          case Success((π(λ(Symbol("ch")), λ(Symbol("n")), false, None), (bound, free)), _) =>
            assert(bound.isEmpty)
            assertEquals(free, Names() + Symbol("ch") + Symbol("n"))
            assert(bindings.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }
  test("leaf - conditional - match") {

    val `13` = new CalculusParserTest:
      override def test =
        given bindings: Bindings = Bindings()
        parseAll(leaf, "[m = n]") match
          case Success((?:(((λ(Symbol("m")), λ(Symbol("n"))), false), ∅(_), None), free), _) =>
            assertEquals(free, Names() + Symbol("m") + Symbol("n"))
            assert(bindings.isEmpty)
          case it =>
            assert(false)

    `13`.test

  }

  test("leaf - conditional - if-then-else") {

    val `13` = new CalculusParserTest:
      override def test =
        given bindings: Bindings = Bindings()
        parseAll(leaf, "if True ≠ False then else") match
          case Success((?:(((λ(true), λ(false)), true), ∅(_), Some(∅(_))), free), _) =>
            assert(free.isEmpty)
            assert(bindings.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - conditional - elvis operator") {

    val `13` = new CalculusParserTest:
      override def test =
        given bindings: Bindings = Bindings()
        parseAll(leaf, "False ≠ True ? :") match
          case Success((?:(((λ(false), λ(true)), true), ∅(_), Some(∅(_))), free), _) =>
            assert(free.isEmpty)
            assert(bindings.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - replication - unguarded") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(leaf(using Bindings()), "!") match
          case Success((!(None, ∅(_)), free), _) =>
            assert(free.isEmpty)
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - replication - input guard - subject and object identical") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(leaf(using Bindings()), "!.ch(ch).")
      _werr = true

    interceptMessage[GuardParsingException]("ch is both the channel name and the binding parameter name in an input guard") {
      `13`.test
    }

  }

  test("leaf - replication - input guard - without bindings check") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(leaf(using Bindings()), "!.ch(n).") match
          case Success((!(Some(π(λ(Symbol("ch")), λ(Symbol("n")), true, None)), ∅(_)), free), _) =>
            assertEquals(free, Names() + Symbol("ch"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - replication - input guard - with bindings check") {

    val `13` = new CalculusParserTest:
      override def test =
        given bindings: Bindings = Bindings()
        parseAll(leaf, "!.ch(n).") match
          case Success((!(Some(π(λ(Symbol("ch")), λ(Symbol("n")), true, None)), ∅(_)), free), _) =>
            assertEquals(free, Names() + Symbol("ch"))
            bindings.headOption match
              case Some((Symbol("n"), Occurrence(None, Position(1, true)))) =>
              case _ =>
                assert(false)
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - replication - output guard") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(leaf(using Bindings()), "!.ch<n>/*println('m)*/.") match
          case Success((!(Some(π(λ(Symbol("ch")), λ(Symbol("n")), false, Some(_))), ∅(_)), free), _) =>
            assertEquals(free, Names() + Symbol("ch") + Symbol("n") + Symbol("m"))
          case _ =>
            assert(false)

    `13`.test

  }

  test("leaf - replication - silent prefix guard") {

    val `13` = new CalculusParserTest:
      override def test =
        parseAll(leaf(using Bindings()), "!.τ/*println('n)*/.") match
          case Success((!(Some(τ(Some(_))), ∅(_)), free), _) =>
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
    override protected def in: String = getClass.getSimpleName
    override def ln: String = "line #0"
    def instantiation(using Bindings, Duplications): Parser[(`⟦⟧`, Names)] =
      new Parser[(`⟦⟧`, Names)]:
        override def apply(_in: Input): ParseResult[(`⟦⟧`, Names)] =
          Failure(null, _in)
    def capital: Parser[(`{}`, Names)] =
      new Parser[(`{}`, Names)]:
        override def apply(_in: Input): ParseResult[(`{}`, Names)] =
          Failure(null, _in)

    _nest = 0
    _cntr = Map(0 -> 0L)

    given Duplications()

    def test: Unit
